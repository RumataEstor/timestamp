-module(timestamp).

-export([add_micros/2, add_secs/2]).
-export([to_epoch/1]).

-define(MEGA, 1000000).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec add_micros(erlang:timestamp(), integer()) -> erlang:timestamp().
add_micros({Megas, Secs, Micros}, AddMicros) ->
    from_micros((((Megas * ?MEGA) + Secs) * ?MEGA) + Micros + AddMicros).


add_secs({Megas, Secs, Micros}, AddSecs) ->
    from_secs(Megas * ?MEGA + Secs + AddSecs, Micros).


from_micros(Micros) ->
    from_secs(Micros div ?MEGA, Micros rem ?MEGA).


from_secs(Secs, Micros) ->
    {Secs div ?MEGA, Secs rem ?MEGA, Micros}.


-spec to_epoch(erlang:timestamp()) -> integer().
to_epoch({Megas, Secs, _}) ->
    (Megas * ?MEGA) + Secs.


-ifdef(TEST).

add_micros_test_() ->
    [?_assertMatch({1, 2, 4}, add_micros({1, 2, 3}, 1)),
     ?_assertMatch({1, 3, 4}, add_micros({1, 2, 3}, ?MEGA + 1)),
     ?_assertMatch({2, 3, 4}, add_micros({1, 2, 3}, (?MEGA + 1) * ?MEGA + 1)),

     ?_assertMatch({1, 2, 2}, add_micros({1, 2, 3}, -1)),
     ?_assertMatch({1, 2, 0}, add_micros({1, 2, 3}, -3)),
     ?_assertMatch({1, 1, 2}, add_micros({1, 2, 3}, -(?MEGA + 1))),
     ?_assertMatch({1, 0, 2}, add_micros({1, 2, 3}, -(2*?MEGA + 1))),
     ?_assertMatch({0, 0, 2}, add_micros({1, 2, 3}, -((?MEGA + 2) * ?MEGA + 1))),
     ?_assertMatch({0, 0, -1}, add_micros({1, 2, 3}, -((?MEGA + 2) * ?MEGA + 4))),
     ?_assertMatch({-1, -1, -1}, add_micros({1, 2, 3}, -((2 * ?MEGA + 3) * ?MEGA + 4)))].


add_secs_test_() ->
    [?_assertMatch({1, 3, 3}, add_secs({1, 2, 3}, 1)),
     ?_assertMatch({2, 3, 3}, add_secs({1, 2, 3}, ?MEGA + 1))].

-endif.
