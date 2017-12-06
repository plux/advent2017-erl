-module(day1).
-compile([export_all]).

solve(Input) ->
    Nums = [C - $0 || C <- Input],
    {part1(Nums), part2(Nums)}.

part1(L) ->
    solve(1, L).

part2(L) ->
    solve(length(L) div 2, L).

solve(N, L) ->
    lists:sum([X || {X, X} <- lists:zip(L, rotate(N, L))]).

rotate(N, List) ->
    {First, Last} = lists:split(N, List),
    Last ++ First.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual(3, part1([1,1,2,2]))
    , ?_assertEqual(12, part2([1,2,3,1,2,3]))
    , ?_assertEqual({1216, 1072}, advent2017:solve(?MODULE, "input/day1.txt"))
    ].
