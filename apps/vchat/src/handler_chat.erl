-module(handler_chat).
%%-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
%%-include("records.hrl").

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req1),
    {ok, Req2} = process_request(Method, HasBody, Req1),
    {ok, Req2, State}.

process_request(<<"GET">>, false, Req) ->
    {ok, Content} = chat_dtl:render([]),
    cowboy_req:reply(200, [], Content, Req).
   
terminate(_Reason, _Req, _State) ->
    ok.
