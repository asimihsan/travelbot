-module(tcp_to_amqp_server).
-behaviour(gen_server).

-include("deps/amqp_client/include/amqp_client.hrl").

-export([start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    listener :: pid(),
    socket :: inet:socket(),
    transport :: module(),
    inactivity_timeout :: timeout(),
    max_request_size :: integer()
}).

-define(ERROR_REQUEST_IS_TOO_BIG, 1).
-define(ERROR_REQUEST_IS_MALFORMED, 2).

%% Public API
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    ok = lager:info("tcp_to_amqp_server:start_link/4 entry. pid: ~p\n", [self()]),
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Opts], []).

%% gen_server callbacks

%-spec init([pid(), inet:socket(), module(), any()]) -> {ok, #state{}, non_neg_integer()}.
init([ListenerPid, Socket, Transport, Opts]) ->
    ok = lager:info("tcp_to_amqp_server:init/4 entry. pid: ~p\n", [self()]),

    % !!AI uncomment for tracing all messages ('m'), function calls ('c'),
    % and process related events ('p') in this gen_server instance.
    dbg:p(self(), [p]),

    InactivityTimeout = proplists:get_value(inactivity_timeout, Opts, 5000),
    MaxRequestSize = proplists:get_value(max_request_size, Opts, 65536),
    Transport:setopts(Socket, [
        {active, once},
        {packet, 4},
        {packet_size, MaxRequestSize}
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
                max_request_size=MaxRequestSize}, 0}.
    % -------------------------------------------------------------------------

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

% -----------------------------------------------------------------------------
%   handle_info/2 for timeout after init/1 finishes.
% -----------------------------------------------------------------------------
handle_info(timeout, State=#state{listener=ListenerPid}) ->
    ok = lager:info("tcp_to_amqp_server:handle_info/1. Msg is timeout, initialization finished.\n"),
    ok = cowboy:accept_ack(ListenerPid),
    {noreply, State};
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
%   handle_info/2 for TCP socket data.
% -----------------------------------------------------------------------------
handle_info({tcp, _Socket, RawData}, State) ->
    handle_request(RawData, State);
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    ok = lager:info("tcp_to_amqp_server:handle_info/1. tcp_error. reason: ~p.\n", [Reason]),
    {stop, normal, State};
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
%   handle_info/2 for unknown message.
% -----------------------------------------------------------------------------
handle_info(Msg, State=#state{}) ->
    ok = lager:info("tcp_to_amqp_server:handle_info/1. Unknown msg: ~p.\n", [Msg]),
    {noreply, State}.
% -----------------------------------------------------------------------------

terminate(_Reason, _State=#state{socket=Socket,
                                 transport=Transport}) ->
    Transport:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private functions.

% -----------------------------------------------------------------------------
%   handle_request/2 for 'ping'.
% -----------------------------------------------------------------------------
handle_request(RawData, State=#state{socket=Socket, transport=Transport}) when RawData =:= <<"ping">> ->
    ok = lager:debug("tcp_to_amqp_server:handle_request/2. 'ping' received.\n"),
    Transport:send(Socket, <<"pong">>),
    Transport:setopts(Socket, [{active, once}]),
    {noreply, State};
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
%   handle_request/2 for 'close'.
% -----------------------------------------------------------------------------
handle_request(RawData, State=#state{socket=_Socket, transport=_Transport}) when RawData =:= <<"close">> ->
    ok = lager:debug("tcp_to_amqp_server:handle_request/2. 'close' received.\n"),
    {stop, normal, State};
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
%   handle_request/2 for JSON payloads.
% -----------------------------------------------------------------------------
handle_request(RawData, State=#state{socket=Socket, transport=Transport}) ->
    ok = lager:debug("tcp_to_amqp_server:handle_request/2. RawData: ~p.\n", [RawData]),
    ok = validate_request(RawData),
    ok = post_request(RawData),
    Transport:setopts(Socket, [{active, once}]),
    {noreply, State}.

validate_request(RawData) ->
    _Request = json:from_json_to_dict(RawData),
    ok.

post_request(RawData) ->
    Request = json:from_json_to_dict(RawData),
    Method = dict:fetch(method, Request),
    ok = lager:debug("Method: ~p.\n", [Method]),

    % -------------------------------------------------------------------------
    %   Post onto AMQP broker.
    % -------------------------------------------------------------------------
    {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    %amqp_channel:call(Channel, #'queue.declare'{queue = list_to_binary(Method)}),
    amqp_channel:cast(Channel,
        #'basic.publish'{
            exchange = <<"">>,
            routing_key = list_to_binary(Method)},
        #amqp_msg{payload = RawData}),
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection),
    ok.


