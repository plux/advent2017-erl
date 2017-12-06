-module(day5).
-compile([export_all]).

solve(Input) ->
    {solve_part1(parse(Input), 0, 0), solve_part2(parse(Input), 0, 0)}.

parse(Input) ->
    erase(),
    lists:foldl(fun(N, Acc) ->
                        put(Acc, list_to_integer(N)),
                        Acc+1
                end, 0, string:tokens(Input, "\n")).

solve_part1(Size, Pos, N) when Pos < 0; Pos >= Size ->
    N;
solve_part1(Size, Pos, N) ->
    Offset = get(Pos),
    put(Pos, Offset+1),
    solve_part1(Size, Pos+Offset, N+1).

solve_part2(Size, Pos, N) when Pos < 0; Pos >= Size ->
    N;
solve_part2(Size, Pos, N) ->
    case get(Pos) of
        Offset when Offset >= 3 -> put(Pos, Offset-1);
        Offset                  -> put(Pos, Offset+1)
    end,
    solve_part2(Size, Pos+Offset, N+1).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({364539, 27477714}, advent2017:solve(?MODULE, "input/day5.txt"))
    ].
