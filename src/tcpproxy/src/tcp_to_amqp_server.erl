-module(tcp_to_amqp_server).
-behaviour(gen_server).

-include_lib("deps/erlson/include/erlson.hrl").

-export([start_link/4, send_task_result/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    listener :: pid(),

    % For SSL sockets must be ssl:sslsocket().
    % For non-SSL sockets must be inet:socket(),
    socket :: ssl:sslsocket(),
    %socket :: inet:socket(),

    transport :: module(),
    inactivity_timeout :: timeout(),
    max_request_size :: integer(),
    task_timeout :: timeout(),
    source_address :: inet:ip_address(),
    source_port :: non_neg_integer(),

    % Heartbeat send and timeout timers
    heartbeat_send_timer :: timer:tref(),
    heartbeat_send_interval :: timer:time(),
    heartbeat_timeout_timer :: timer:tref(),
    heartbeat_timeout_interval :: timer:time()
}).

-define(ERROR_REQUEST_IS_TOO_BIG, 1).
-define(ERROR_REQUEST_IS_MALFORMED, 2).

%% ----------------------------------------------------------------------------
%% Public API
%% ----------------------------------------------------------------------------
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    ok = lager:info("tcp_to_amqp_server:start_link/4 entry. pid: ~p\n", [self()]),
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Opts], []).

send_task_result(Pid, Result) ->
    ok = lager:info("tcp_to_amqp_server:send_task_result/2 entry. Pid: ~p\n", [Pid]),
    gen_server:cast(Pid, {task_result, Result}).
%% ----------------------------------------------------------------------------

%% gen_server callbacks

%-spec init([pid(), inet:socket(), module(), any()]) -> {ok, #state{}, non_neg_integer()}.
init([ListenerPid, Socket, Transport, Opts]) ->
    ok = lager:info("tcp_to_amqp_server:init/4 entry. pid: ~p\n", [self()]),

    % !!AI uncomment for tracing all messages ('m'), function calls ('c'),
    % and process related events ('p') in this gen_server instance.
    dbg:p(self(), [p]),

    InactivityTimeout = proplists:get_value(inactivity_timeout, Opts, 5000),
    MaxRequestSize = proplists:get_value(max_request_size, Opts, 65536),
    TaskTimeout = proplists:get_value(task_timeout, Opts, 60000),
    HeartbeatSendInterval = proplists:get_value(heartbeat_send_interval, Opts, 10*60*1000), % 10 minutes
    HeartbeatTimeoutInterval = proplists:get_value(heartbeat_timeout_interval, Opts, 5000),

    % To get peername for non-SSL socket is inet:peername(Socket).
    % To get peername for SSL socket is ssl:peername(Socket).
    {ok, {SourceAddress, SourcePort}} = ssl:peername(Socket),

    Transport:setopts(Socket, [
        {active, once},
        {packet, 4},
        {packet_size, MaxRequestSize},
        {nodelay, true}
    ]),

    % -------------------------------------------------------------------------
    %   The result of gen_server:init/1 is {ok, State, Timeout}. We use
    %   Timeout = 0 to allow the process who called start_link/4 to
    %   immediately return, rather than wait for this process to finish
    %   starting. This results in handle_info/3 with timeout getting called.
    %   When this handle_info/3 gets called only then can we tell
    %   cowboy that we are ready to process this socket.
    %
    %   Reference: ErlangOTPInAction pages 111, 266.
    % -------------------------------------------------------------------------
    {ok, #state{listener=ListenerPid,
                socket=Socket,
                transport=Transport,
                inactivity_timeout=InactivityTimeout,
                max_request_size=MaxRequestSize,
                task_timeout=TaskTimeout,
                source_address=SourceAddress,
                source_port=SourcePort,
                heartbeat_send_interval=HeartbeatSendInterval,
                heartbeat_timeout_interval=HeartbeatTimeoutInterval}, 0}.
    % -------------------------------------------------------------------------

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({task_result, Result},
            State=#state{socket=Socket,
                         transport=Transport}) ->
    ok = lager:info("tcp_to_amqp_server:handle_cast/1. Got task_result\n"),
    ok = validate_result(Result),
    Transport:send(Socket, Result),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

% -----------------------------------------------------------------------------
%   handle_info/2 for timeout after init/1 finishes.
% -----------------------------------------------------------------------------
handle_info(timeout, State=#state{listener=ListenerPid,
                                  socket=_Socket,
                                  source_address=SourceAddress,
                                  source_port=SourcePort,
                                  heartbeat_send_interval=HeartbeatSendInterval}) ->
    ok = lager:info("tcp_to_amqp_server:handle_info/1. Msg is timeout, initialization finished for ~p:~p.", [SourceAddress, SourcePort]),
    ok = cowboy:accept_ack(ListenerPid),
    {ok, HeartbeatSendTimer} = timer:send_after(HeartbeatSendInterval, heartbeat_send),
    State2 = State#state{heartbeat_send_timer = HeartbeatSendTimer},
    {noreply, State2};
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
%   handle_info/2 for TCP socket data.
%
%   The first atom is different for SSL connections. We explicitly support
%   messages from only SSL connections, but leave TCP connection messages
%   commented as reference.
%
%   Any reads on the socket imply that it is still alive, so cancel the
%   heartbeat timer for this interval.
% -----------------------------------------------------------------------------
% SSL
handle_info({ssl, _Socket, RawData}, State=#state{heartbeat_timeout_timer=HeartbeatTimeoutTimer}) ->
    % the timeout timer may not be activated yet, so don't pattern match
    % on the result.
    timer:cancel(HeartbeatTimeoutTimer),

    handle_request(RawData, State);
handle_info({ssl_closed, _Socket}, State) ->
    handle_connection_closed(State);
handle_info({ssl_error, _Socket, Reason}, State) ->
    handle_connection_error(Reason, State);

% non-SSL
%handle_info({ssl, _Socket, RawData}, State=#state{heartbeat_timeout_timer=HeartbeatTimeoutTimer}) ->
%    {ok, cancel} = timer:cancel(HeartbeatTimeoutTimer),
%    handle_request(RawData, State);
%handle_info({tcp_closed, _Socket}, State) ->
%    handle_connection_closed(State);
%handle_info({tcp_error, _Socket, Reason}, State) ->
%    handle_connection_error(Reason, State);
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
%   handle_info/2 for heartbeats.
% -----------------------------------------------------------------------------
handle_info(heartbeat_send, State=#state{socket=Socket,
                                         transport=Transport,
                                         source_address=SourceAddress,
                                         source_port=SourcePort,
                                         heartbeat_timeout_interval=HeartbeatTimeoutInterval}) ->
    ok = lager:info("tcp_to_amqp_server:handle_info/2. heartbeat_send for ~p:~p.", [SourceAddress, SourcePort]),
    Transport:send(Socket, <<"ping">>),
    {ok, HeartbeatTimeoutTimer} = timer:send_after(HeartbeatTimeoutInterval, heartbeat_timeout),
    State2 = State#state{heartbeat_timeout_timer = HeartbeatTimeoutTimer},
    {noreply, State2};

handle_info(heartbeat_timeout, State=#state{source_address=SourceAddress, source_port=SourcePort}) ->
    ok = lager:info("tcp_to_amqp_server:handle_info/2. heartbeat_timeout for ~p:~p.", [SourceAddress, SourcePort]),
    {stop, normal, State};
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
%   handle_info/2 for unknown message.
% -----------------------------------------------------------------------------
handle_info(Msg, State=#state{}) ->
    ok = lager:info("tcp_to_amqp_server:handle_info/2. Unknown msg: ~p.\n", [Msg]),
    {noreply, State}.
% -----------------------------------------------------------------------------

terminate(_Reason, _State=#state{socket=Socket,
                                 transport=Transport}) ->
    Transport:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private functions.

% Handle connection closes and failures.
handle_connection_closed(State=#state{source_address=SourceAddress, source_port=SourcePort}) ->
    ok = lager:info("tcp_to_amqp_server:handle_connection_closed/1. For ~p:~p.", [SourceAddress, SourcePort]),
    {stop, normal, State}.
handle_connection_error(Reason, State=#state{source_address=SourceAddress, source_port=SourcePort}) ->
    ok = lager:info("tcp_to_amqp_server:handle_connection_error/1. For ~p:~p. reason: ~p.\n", [SourceAddress, SourcePort, Reason]),
    {stop, normal, State}.

% -----------------------------------------------------------------------------
%   handle_request/2 for 'ping'.
% -----------------------------------------------------------------------------
handle_request(RawData, State=#state{socket=Socket,
                                     transport=Transport,
                                     source_address=SourceAddress,
                                     source_port=SourcePort}) when RawData =:= <<"ping">> ->
    ok = lager:debug("tcp_to_amqp_server:handle_request/2. 'ping' received from ~p:~p.", [SourceAddress, SourcePort]),
    Transport:send(Socket, <<"pong">>),
    Transport:setopts(Socket, [{active, once}]),
    {noreply, State};
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
%   handle_request/2 for 'pong'.
% -----------------------------------------------------------------------------
handle_request(RawData, State=#state{socket=Socket,
                                     transport=Transport,
                                     source_address=SourceAddress,
                                     source_port=SourcePort,
                                     heartbeat_send_interval=HeartbeatSendInterval}) when RawData =:= <<"pong">> ->
    ok = lager:debug("tcp_to_amqp_server:handle_request/2. 'pong' received from ~p:~p.", [SourceAddress, SourcePort]),
    {ok, HeartbeatSendTimer} = timer:send_after(HeartbeatSendInterval, heartbeat_send),
    State2 = State#state{heartbeat_send_timer = HeartbeatSendTimer},
    Transport:setopts(Socket, [{active, once}]),
    {noreply, State2};
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
%   handle_request/2 for 'close'.
% -----------------------------------------------------------------------------
handle_request(RawData, State=#state{socket=_Socket,
                                     transport=_Transport,
                                     source_address=SourceAddress,
                                     source_port=SourcePort}) when RawData =:= <<"close">> ->
    ok = lager:debug("tcp_to_amqp_server:handle_request/2. 'close' received from ~p:~p.", [SourceAddress, SourcePort]),
    {stop, normal, State};
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
%   handle_request/2 for JSON payloads.
% -----------------------------------------------------------------------------
handle_request(RawData, State=#state{socket=Socket,
                                     transport=Transport,
                                     source_address=SourceAddress,
                                     source_port=SourcePort}) ->
    ok = lager:debug("tcp_to_amqp_server:handle_request/2. RawData from ~p:~p: ~p.\n", [SourceAddress, SourcePort, RawData]),
    ok = validate_request(RawData),

    % -------------------------------------------------------------------------
    %   Process request.
    %   -   If it is a 'task' then spawn a amqp_task_server instance.
    % -------------------------------------------------------------------------
    Request = dict:from_list(erlson:from_json(RawData)),
    Type = dict:fetch(type, Request),
    if Type =:= <<"task">> ->
        {ok, Pid} = amqp_task_server:start_link(self(), Request),
        ok = lager:info("tcp_to_amqp_server:handle_request/2 launched amqp_task_server with Pid ~p for ~p:~p.", [Pid, SourceAddress, SourcePort])
    end,
    % -------------------------------------------------------------------------

    Transport:setopts(Socket, [{active, once}]),
    {noreply, State}.

validate_result(_RawData) ->
    % -------------------------------------------------------------------------
    %   Parse then validate request.
    % -------------------------------------------------------------------------
    %_Request = dict:from_list(erlson:from_json(RawData)),
    % -------------------------------------------------------------------------

    ok.

validate_request(RawData) ->
    % -------------------------------------------------------------------------
    %   Constants. !!AI factor out into hrl.
    % -------------------------------------------------------------------------
    ValidTypeRe = "^task$",
    % -------------------------------------------------------------------------

    % -------------------------------------------------------------------------
    %   Parse then validate request.
    % -------------------------------------------------------------------------
    Request = dict:from_list(erlson:from_json(RawData)),
    Type = dict:fetch(type, Request),
    {match, _Range} = re:run(Type, ValidTypeRe, []),
    % -------------------------------------------------------------------------

    ok.

