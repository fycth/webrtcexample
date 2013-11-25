-module(signaler_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(lager),
    ok = application:start(ranch),
    ok = application:start(crypto),
    ok = application:start(cowboy),
    ok = application:start(gproc),
    ok = application:start(signaler).

start(_StartType, _StartArgs) ->
    Port = app_config:get_conf(signaler_listen_port, 0),
    Dispatch = cowboy_router:compile([
                                      {'_',[
                                            {"/s", handler_websocket,[]} %% signaling, websocket
                                           ]}
                                     ]),
    {ok, _} = cowboy:start_http(websocket, 100, [{port, Port},{ip,{127,0,0,1}}], [
                                {env, [{dispatch, Dispatch}]},
                                {max_keepalive, 50},
                                {timeout, 500}
                                                           ]),
    ets:new(signaler_ets,[set, named_table, public]),
    signaler_sup:start_link().

stop(_State) ->
    ok.


