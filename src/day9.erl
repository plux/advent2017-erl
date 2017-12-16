-module(day9).
-compile([export_all]).

solve(Input) ->
    solve(Input, 1, 0, 0).

solve([${|Rest], Level, GroupSum, GarbageSum) ->
    solve(Rest, Level + 1, GroupSum + Level, GarbageSum);
solve([$<|Rest0], Level, GroupSum, GarbageSum) ->
    {GarbageCount, Rest} = gc(Rest0, 0),
    solve(Rest, Level, GroupSum, GarbageSum + GarbageCount);
solve([$}|Rest], Level, GroupSum, GarbageSum) ->
    solve(Rest, Level - 1, GroupSum, GarbageSum);
solve([$,|Rest], Level, GroupSum, GarbageSum) ->
    solve(Rest, Level, GroupSum, GarbageSum);
solve([], _N, GroupSum, GarbageSum) ->
    {GroupSum,GarbageSum}.

gc([$!, _| Rest], GarbageSum) -> gc(Rest, GarbageSum);
gc([$>|Rest], GarbageSum)     -> {GarbageSum, Rest};
gc([_|Rest], GarbageSum)      -> gc(Rest, GarbageSum+1).


-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({1,0}, solve("{}"))
    , ?_assertEqual({6,0}, solve("{{{}}}"))
    , ?_assertEqual({5,0}, solve("{{},{}}"))
    , ?_assertEqual({16,0}, solve("{{{},{},{{}}}}"))
    , ?_assertEqual({1,4}, solve("{<a>,<a>,<a>,<a>}"))
    , ?_assertEqual({9,0}, solve("{{<!!>},{<!!>},{<!!>},{<!!>}}"))
    , ?_assertEqual({9,8}, solve("{{<ab>},{<ab>},{<ab>},{<ab>}}"))
    , ?_assertEqual({3,17}, solve("{{<a!>},{<a!>},{<a!>},{<ab>}}"))
    , ?_assertEqual({10820, 5547}, advent2017:solve(day9, "input/day9.txt"))
    ].
