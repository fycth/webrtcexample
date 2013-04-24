-module(vchat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(compiler),
    application:start(syntax_tools),
    application:start(lager),
    application:start(ranch),
    application:start(crypto),
    application:start(cowboy),
    application:start(gproc),
    application:start(signaler),
    application:start(vchat).

start(_StartType, _StartArgs) ->
    Port = app_config:get_conf(listen_port, 0),
    LDomain = app_config:get_conf(listen_host, "localhost"),
    Dispatch = cowboy_router:compile([
                                      {LDomain,[
                                            {"/chat", handler_chat,[]}
                                           ]}
                                     ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
                                {env, [{dispatch, Dispatch}]},
                                {max_keepalive, 50},
                                {timeout, 500}
                                                           ]),
    vchat_sup:start_link().

stop(_State) ->
    ok.


