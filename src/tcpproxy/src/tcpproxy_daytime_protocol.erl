%% Copyright (c) 2012, Asim Ihsan <asim.ihsan@gmail.com>

%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% ----------------------------------------------------------------------------
%%  travelbot/src/tcpproxy/src/tcpproxy_daytime_protocol.erl
%%
%%  Implements the Daytime service. On connection sends the current datetime
%%  in ISO 8601 format and then closes the connection.
%% ----------------------------------------------------------------------------

%% References:
%% https://github.com/extend/cowboy/blob/master/src/cowboy_protocol.erl
%% https://github.com/extend/cowboy/blob/master/src/cowboy_http_protocol.erl

-module(tcpproxy_daytime_protocol).
-behaviour(cowboy_protocol).

-export([start_link/4]). %% API
-export([init/4]). %% FSM

%% API

%% @doc Start a Daytime protocol process
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    ok = lager:debug("tcpproxy_daytime_protocol:start_link/4 entry.\n"),
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

%% FSM

%% @private
-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, _Opts) ->
    ok = lager:debug("tcpproxy_daytime_protocol:init/4 entry.\n"),
    ok = cowboy:accept_ack(ListenerPid),
    Transport:send(Socket, get_datetime_string()),
    Transport:close(Socket),
    ok.

%% @private
-spec get_datetime_string() -> string().
get_datetime_string() ->
    {_MegaSecs, _Secs, MicroSecs} = erlang:now(),
    IsoDatetime = dh_date:format("Y-m-dTG:i:s", calendar:universal_time()),
    ReturnValue = io_lib:format("~s.~pZ", [IsoDatetime, MicroSecs / 1000]),
    ReturnValue.

