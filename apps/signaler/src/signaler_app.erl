-module(signaler_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(signaler).

start(_StartType, _StartArgs) ->
    Port = app_config:get_conf(signaler_listen_port, 0),
    LDomain = app_config:get_conf(signaler_listen_host,"localhost"),
    Dispatch = cowboy_router:compile([
                                      {LDomain,[
                                            {"/s", handler_websocket,[]} %% signaling, websocket
                                           ]}
                                     ]),
    {ok, _} = cowboy:start_http(websocket, 100, [{port, Port}], [
                                {env, [{dispatch, Dispatch}]},
                                {max_keepalive, 50},
                                {timeout, 500}
                                                           ]),
    ets:new(signaler_ets,[set, named_table, public]),
    signaler_sup:start_link().

stop(_State) ->
    ok.


