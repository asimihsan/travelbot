%% ----------------------------------------------------------------------------
%%  References:
%%  -   http://www.rabbitmq.com/releases/rabbitmq-erlang-client/v2.8.5/doc/amqp_connection.html
%%  -   http://www.rabbitmq.com/releases/rabbitmq-erlang-client/v2.8.5/doc/amqp_channel.html
%% ----------------------------------------------------------------------------

-module(amqp_task_server).
-behaviour(gen_server).

-include("deps/amqp_client/include/amqp_client.hrl").
-include_lib("deps/erlson/include/erlson.hrl").

-record(state, {
    parent_pid :: pid(),
    request :: dict(),
    uuid :: binary(),
    amqp_connection :: pid(),
    amqp_channel :: any(),
    amqp_subscription_tag :: binary()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/2, stop/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link(pid(), dict()) -> {ok, pid()}.
start_link(ParentPid, Request) ->
    ok = lager:info("amqp_taks_server:start_link/2. ParentPid: ~p, Request: ~p.", [ParentPid, Request]),
    gen_server:start_link(?MODULE, [ParentPid, Request], []).

stop() ->
    gen_server:cast(self(), stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([ParentPid, Request]) ->
    ok = lager:info("amqp_taks_server:init/1. ParentPid: ~p, Request: ~p.", [ParentPid, Request]),

    % dbg:p(self(), [p, c]),
    process_flag(trap_exit, true),
    {ok, #state{parent_pid=ParentPid,
                request=Request}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% ----------------------------------------------------------------------------
%%  handle_info/2 for timeout handles the asynchronous initialization of this
%%  task.
%% ----------------------------------------------------------------------------
handle_info(timeout, State=#state{request=Request}) ->
    ok = lager:info("amqp_task_server:handle_info/2. Msg is timeout, initialization finished.\n"),

    Host = "localhost",
    {ok, AMQPConnection} = amqp_connection:start(#amqp_params_network{host = Host}),
    {ok, AMQPChannel} = amqp_connection:open_channel(AMQPConnection),
    UUID = erlang:list_to_binary(uuid:to_string(uuid:uuid4())),
    State2 = State#state{request=Request,
                         uuid=UUID,
                         amqp_connection=AMQPConnection,
                         amqp_channel=AMQPChannel},
    ok = lager:info("amqp_task_server:handle_info/2 with Pid ~p handling task UUID ~p.", [self(), UUID]),

    {ok, State3} = post_request(State2),
    {ok, State4} = subscribe_for_request_result(State3),
    {noreply, State4};
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
%%  handle_info/2 for subscription messages from the task's result queue.
%% ----------------------------------------------------------------------------
handle_info({#'basic.consume_ok'{}}, State=#state{uuid=UUID}) ->
    ok = lager:info("amqp_task_server_~p:handle_info/2. basic.consume_ok. Subscribed to results.", [UUID]),
    {noreply, State};
handle_info({#'basic.cancel_ok'{}}, State=#state{uuid=UUID}) ->
    ok = lager:info("amqp_task_server_~p:handle_info/2. basic.cancel_ok. Unsubscribed to results.", [UUID]),
    {noreply, State};
handle_info({#'basic.deliver'{delivery_tag=Tag}, _Content=#amqp_msg{payload=Payload}},
            State=#state{uuid=UUID,
                         amqp_channel=AMQPChannel,
                         parent_pid=ParentPid}) ->
    ok = lager:info("amqp_task_server_~p:handle_info/2. basic.deliver", [UUID]),
    tcp_to_amqp_server:send_task_result(ParentPid, Payload),
    amqp_channel:cast(AMQPChannel, #'basic.ack'{delivery_tag = Tag}),
    stop(),
    {noreply, State};
%% ----------------------------------------------------------------------------

handle_info(Msg, State=#state{uuid=UUID}) ->
    ok = lager:info("amqp_task_server_~p:handle_info/2. Unknown message: ~p.\n", [UUID, Msg]),
    {noreply, State}.

terminate(_Reason, _State=#state{uuid=UUID,
                                 amqp_connection=AMQPConnection,
                                 amqp_channel=AMQPChannel,
                                 amqp_subscription_tag=AMQPSubscriptionTag}) ->
    ok = lager:info("amqp_task_server_~p:terminate/0.\n", [UUID]),
    unsubscribe_subscription(AMQPChannel, AMQPSubscriptionTag),
    unbind_rule_delete_queue(AMQPChannel, UUID),
    close_amqp_connection(AMQPConnection, AMQPChannel, UUID),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
unsubscribe_subscription(AMQPChannel, AMQPSubscriptionTag)
                        when (AMQPChannel /= undefined) and
                             (AMQPSubscriptionTag /= undefined) ->
    amqp_channel:call(AMQPChannel, #'basic.cancel'{consumer_tag = AMQPSubscriptionTag}),
    ok;
unsubscribe_subscription(_AMQPChannel, _AMQPSubscriptionTag) ->
    ok.

unbind_rule_delete_queue(AMQPChannel, UUID) when AMQPChannel /= undefined ->
    % -------------------------------------------------------------------------
    %   Unbind rule between queue and exchange, delete queue.
    % -------------------------------------------------------------------------
    Exchange = <<"tcpproxy">>,
    RoutingKeyHeader = <<"task_result_">>,
    RoutingKey = <<RoutingKeyHeader/binary, UUID/binary>>,
    Binding = #'queue.unbind'{queue = RoutingKey,
                              exchange = Exchange,
                              routing_key = RoutingKey},
    #'queue.unbind_ok'{} = amqp_channel:call(AMQPChannel, Binding),
    Delete = #'queue.delete'{queue = RoutingKey},
    #'queue.delete_ok'{} = amqp_channel:call(AMQPChannel, Delete),
    % -------------------------------------------------------------------------

    ok;
unbind_rule_delete_queue(_AMQPChannel, _UUID) ->
    ok.

close_amqp_connection(AMQPConnection, AMQPChannel, UUID)
                    when (AMQPConnection /= undefined) and
                         (AMQPChannel /= undefined) ->
    ok = lager:info("amqp_task_sever_~p:close_amqp_connection/2 with defined variables.", [UUID]),
    ok = amqp_channel:close(AMQPChannel),
    ok = amqp_connection:close(AMQPConnection),
    ok;
close_amqp_connection(_AMQPConnection, _AMQPChannel, UUID) ->
    ok = lager:info("amqp_task_sever_~p:close_amqp_connection/2 with undefined variables.", [UUID]),
    ok.

-spec subscribe_for_request_result(#state{}) -> {ok, #state{}}.
subscribe_for_request_result(State=#state{uuid=UUID,
                                          amqp_channel=AMQPChannel}) ->
    ok = lager:info("amqp_task_server_~p:subscribe_for_request_result/1.\n", [UUID]),

    % -------------------------------------------------------------------------
    %   We need to bind to the 'tcpproxy' exchange with a queue name of
    %   'task_result_<<UUID>>'.
    % -------------------------------------------------------------------------
    Exchange = <<"tcpproxy">>,
    RoutingKeyHeader = <<"task_result_">>,
    RoutingKey = <<RoutingKeyHeader/binary, UUID/binary>>,
    ok = lager:info("amqp_task_server_~p:subscribe_for_request_result/1. RoutingKey: ~p\n", [UUID, RoutingKey]),
    amqp_channel:call(AMQPChannel, #'queue.declare'{queue = RoutingKey}),
                                                    %durable = true,
                                                    %auto_delete = true,
                                                    %exclusive = false}),
    Binding = #'queue.bind'{queue = RoutingKey,
                            exchange = Exchange,
                            routing_key = RoutingKey},
    #'queue.bind_ok'{} = amqp_channel:call(AMQPChannel, Binding),
    % -------------------------------------------------------------------------

    % -------------------------------------------------------------------------
    %   Subscribe for messages in this task's result queue. We'll handle
    %   the messages in handle_info.
    % -------------------------------------------------------------------------
    Sub = #'basic.consume'{queue = RoutingKey},
    #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:call(AMQPChannel, Sub), %% the caller is the subscriber
    State2 = State#state{amqp_subscription_tag = Tag},
    % -------------------------------------------------------------------------

    {ok, State2}.

-spec post_request(#state{}) -> {ok, #state{}}.
post_request(State=#state{request=Request,
                          uuid=UUID,
                          amqp_channel=AMQPChannel}) ->
    ok = lager:info("amqp_task_server_~p:post_request/1. Request: ~p.\n", [UUID, dict:to_list(Request)]),

    % -------------------------------------------------------------------------
    %   -   Determine the type of request.
    %   -   Put a UUIDv4 '_id' field into it.
    % -------------------------------------------------------------------------
    %Type = dict:fetch(type, Request),
    RequestWithId = dict:store('_id', UUID, Request),
    % -------------------------------------------------------------------------

    % -------------------------------------------------------------------------
    %   Post onto AMQP broker.
    % -------------------------------------------------------------------------
    Exchange = <<"tcpproxy">>,
    DeclareExchange = #'exchange.declare'{exchange = Exchange},
    #'exchange.declare_ok'{} = amqp_channel:call(AMQPChannel, DeclareExchange),
    RoutingKey = <<"task">>,
    Payload = erlang:iolist_to_binary(
                  erlson:to_json(
                      dict:to_list(RequestWithId)
              )),
    ok = lager:info("Payload: ~p.\n", [Payload]),
    amqp_channel:cast(AMQPChannel,
        #'basic.publish'{
            exchange = Exchange,
            routing_key = RoutingKey},
        #amqp_msg{payload = Payload}),

    {ok, State}.

