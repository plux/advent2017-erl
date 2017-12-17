-module(day17).
-compile([export_all]).

solve(Input) ->
    Steps = list_to_integer(Input),
    {solve_part1(Steps), solve_part2(Steps)}.

solve_part1(Steps) ->
    {Pos, List} = lists:foldl(
                    fun(N, {Pos, List}) ->
                            InsertPos = ((Pos+Steps) rem N) + 1,
                            {First, Last} = lists:split(InsertPos, List),
                            {InsertPos, First ++ [N] ++ Last}
                    end, {1, [0]}, lists:seq(1, 2017)),
    lists:nth(Pos+2, List).

solve_part2(Steps) ->
    solve_part2(1, 1, -1, Steps).

solve_part2(50000000, _Pos, After0, _Steps) ->
    After0;
solve_part2(N, Pos, After0, Steps) ->
    InsertPos = ((Pos+Steps) rem N) + 1,
    case InsertPos of
        1 -> solve_part2(N + 1, InsertPos, N, Steps);
        _ -> solve_part2(N + 1, InsertPos, After0, Steps)
    end.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual(638, solve_part1(3))
    , ?_assertEqual(1670, solve_part1(328))
    , ?_assertEqual(2316253, solve_part2(328))
    ].
