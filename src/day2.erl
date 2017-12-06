-module(day2).
-compile([export_all]).

solve(Input) ->
    Rows = parse(Input),
    {part1(Rows), part2(Rows)}.

parse(Input) ->
    [ [ list_to_integer(N) || N <- string:tokens(Row, "\t")]
      || Row <- string:tokens(Input, "\n")].

part1(Rows) ->
    lists:sum([lists:max(Row) - lists:min(Row) || Row <- Rows]).

part2(Rows) ->
    lists:sum([find_divisor(Row) || Row <- Rows]).

find_divisor(Row) ->
    hd([Num div Denom || Num <- Row,
                         Denom <- Row,
                         Num rem Denom =:= 0,
                         Num =/= Denom]).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual(18, part1([ [5, 1, 9, 5]
                              , [7, 5, 3]
                              , [2, 4, 6, 8]
                              ]))
    , ?_assertEqual(9, part2([ [5, 9, 2, 8]
                             , [9, 4, 7, 3]
                             , [3, 8, 6, 5]]))
    , ?_assertEqual({54426, 333}, advent2017:solve(?MODULE, "input/day2.txt"))
    ].
