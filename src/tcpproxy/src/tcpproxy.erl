-module(tcpproxy).
-behaviour(application).
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(cowboy),
    ok = application:start(tcpproxy).

start(_StartType, _StartArgs) ->
    {ok, _Pid} = cowboy:start_listener(daytime_listener, 5,
        cowboy_tcp_transport, [{port, 13000}],
        tcpproxy_daytime_protocol, []
    ),
    {ok, _Pid2} = cowboy:start_listener(tcp_to_amqp_listener, 5,
        cowboy_ssl_transport, [
            {port, 8080},
            %{certfile, "priv/ssl/cowboy_certs/cert.pem"},
            %{keyfile, "priv/ssl/cowboy_certs/key.pem"},
            %{password, "cowboy"}],
            {certfile, "priv/ssl/server_crt.pem"},
            {keyfile, "priv/ssl/server_key.pem"}],
        tcpproxy_tcp_to_amqp_protocol, []
    ),

    lager:start(),
    lager:set_loglevel(lager_console_backend, debug),
    lager:set_loglevel(lager_file_backend, "console.log", debug),
    dbg:start(),
    dbg:tracer(),

    tcpproxy_sup:start_link().

stop(_State) ->
    ok.

%priv() ->
%    code:priv_dir(tcpproxy).
