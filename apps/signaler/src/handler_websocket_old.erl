-module(handler_websocket_old).
-behaviour(cowboy_websocket_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-record(state, {
          client = undefined :: undefined | {inet:ip_address(), inet:port_number()},
          state = undefined :: undefined | started | running,
          room
}).

init(_Any, _Req, _Opt) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opt) ->
    {Client,_} = cowboy_req:peer(Req),
    State = #state{client = Client, state = started},
    lager:info("Connection from: ~p", [Client]),
    {ok, Req, State, hibernate}.

websocket_handle({text,Data}, Req, State) ->
    StateNew = case (State#state.state) of
                   started ->
                       State#state{state = running};
                   _ ->
                       State
                end,
    {[H|T]} = jsonx:decode(Data),
    {M,_} = H,
    case M of
        <<"type">> ->
            {_, Type} = H,
            case Type of
                <<"GETROOM">> ->
                    Room = generate_room(),
                    R = jsonx:encode([{type, <<"GETROOM">>}, {value, Room}]),
                    gproc:reg({p,l, Room}),
                    S = (StateNew#state{room = Room}),
                    {reply, {text, <<R/binary>>}, Req, S, hibernate};
                <<"INVITE">> ->
                    [H1|_] = T,
                    {<<"value">>,Room} = H1,
                    Participants = gproc:lookup_pids({p,l,Room}),
                    case length(Participants) of
                        1 ->
                            gproc:reg({p,l, Room}),
                            S = (StateNew#state{room = Room}),
                            {ok, Req, S, hibernate};
                        _->
                            R = jsonx:encode([{type, <<"WRONGROOM">>}]),
                            {reply, {text, <<R/binary>>}, Req, StateNew, hibernate}
                    end;
                <<"ROULETTE">> ->
                    case ets:lookup(signaler_ets,room) of
                        [{room,Room}] ->
                            case gproc:lookup_pids({p,l,Room}) of
                                [] ->
                                    CMD = <<"wait">>;
                                _ ->
                                    CMD = <<"go">>,
                                    ets:delete(signaler_ets,room)
                            end;
                        _ ->
                            CMD = <<"wait">>,
                            Room = generate_room(),
                            ets:insert(signaler_ets,{room, Room})
                    end,
                    gproc:reg({p,l, Room}),
                    S = (StateNew#state{room = Room}),
                    R = jsonx:encode([{type, <<"ROULETTE">>}, {value, <<CMD/binary>>}]),
                    {reply, {text, <<R/binary>>}, Req, S, hibernate};
                _ ->
                    reply2peer(Data, StateNew#state.room),
                    {ok, Req, StateNew, hibernate}
            end;
        _ ->
            reply2peer(Data, State#state.room),
            {ok, Req, StateNew, hibernate}
    end;

websocket_handle(_Any, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_info(_Info, Req, State) ->
    {reply, {text,_Info}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

reply2peer(R, Room) ->
    [P ! <<R/binary>> || P <- gproc:lookup_pids({p,l,Room}) -- [self()]].

generate_room() ->
    Now = {_, _, Micro} = now(),
    Nowish = calendar:now_to_universal_time(Now),
    Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
    Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
    list_to_binary(Prefix ++ to_hex(crypto:rand_bytes(9))).

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N-10.
