-module(timestamp).

-export([add_micros/2, add_secs/2]).
-export([from_epoch_micros/1, from_epoch_secs/2]).
-export([to_epoch_micros/1, to_epoch_millis/1, to_epoch_secs/1]).
-export([diff_millis/2]).

-define(MEGA, 1000000).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec add_micros(erlang:timestamp(), integer()) -> erlang:timestamp().
add_micros({Megas, Secs, Micros}, AddMicros) ->
    from_epoch_micros((Megas * ?MEGA + Secs) * ?MEGA + Micros + AddMicros).


add_secs({Megas, Secs, Micros}, AddSecs) ->
    from_epoch_secs(Megas * ?MEGA + Secs + AddSecs, Micros).


from_epoch_micros(Micros) ->
    from_epoch_secs(Micros div ?MEGA, Micros rem ?MEGA).


from_epoch_secs(Secs, Micros) ->
    {Secs div ?MEGA, Secs rem ?MEGA, Micros}.


to_epoch_micros({Megas, Secs, Micros}) ->
    (Megas * ?MEGA + Secs) * ?MEGA + Micros.


to_epoch_millis({Megas, Secs, Micros}) ->
    (Megas * ?MEGA + Secs) * 1000 + Micros div 1000.


-spec to_epoch_secs(erlang:timestamp()) -> integer().
to_epoch_secs({Megas, Secs, _}) ->
    Megas * ?MEGA + Secs.


diff_millis({Megas1, Secs1, Micros1}, {Megas2, Secs2, Micros2}) ->
    Megas = Megas1 - Megas2,
    Secs = Secs1 - Secs2,
    Micros = Micros1 - Micros2,
    (Megas * ?MEGA + Secs) * 1000 + Micros div 1000.


-ifdef(TEST).

add_micros_test_() ->
    [?_assertMatch({1, 2, 4}, add_micros({1, 2, 3}, 1)),
     ?_assertMatch({1, 3, 4}, add_micros({1, 2, 3}, 1000001)),
     ?_assertMatch({2, 3, 4}, add_micros({1, 2, 3}, 1000001000001)),

     ?_assertMatch({1, 2, 2}, add_micros({1, 2, 3}, -1)),
     ?_assertMatch({1, 2, 0}, add_micros({1, 2, 3}, -3)),
     ?_assertMatch({1, 1, 2}, add_micros({1, 2, 3}, -1000001)),
     ?_assertMatch({1, 0, 2}, add_micros({1, 2, 3}, -2000001)),
     ?_assertMatch({0, 0, 2}, add_micros({1, 2, 3}, -1000002000001)),
     ?_assertMatch({0, 0, -1}, add_micros({1, 2, 3}, -1000002000004)),
     ?_assertMatch({-1, -1, -1}, add_micros({1, 2, 3}, -2000003000004))].


add_secs_test_() ->
    [?_assertMatch({1, 3, 3}, add_secs({1, 2, 3}, 1)),
     ?_assertMatch({2, 3, 3}, add_secs({1, 2, 3}, 1000001))].


from_eposh_secs_test_() ->
    [?_assertMatch({1, 2, 3}, from_epoch_secs(1000002, 3)),
     ?_assertMatch({3, 2, 1}, from_epoch_secs(to_epoch_secs({3, 2, 1}), 1))].


from_epoch_micros_test_() ->
    [?_assertMatch({1, 2, 3}, from_epoch_micros(1000002000003)),
     ?_assertMatch({5, 4, 3}, from_epoch_micros(to_epoch_micros({5, 4, 3}))),
     ?_assertMatch({5, 6, 7}, from_epoch_micros(to_epoch_millis({5, 6, 7}) * 1000 + 7))].


diff_millis_test_() ->
    T0 = os:timestamp(),
    [?_assertMatch(42, diff_millis(T0, add_micros(T0, -42000)))].

-endif.
