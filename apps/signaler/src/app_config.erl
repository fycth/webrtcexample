-module(app_config).

-export([get_conf/2]).

get_conf(Param, DefVal) ->
    R = application:get_env(Param),
    val_or_def(R, DefVal).

% private
val_or_def({ok, Val}, _DefVal) ->
    Val;
val_or_def(_, DefVal) ->
    DefVal.
