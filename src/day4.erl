-module(day4).
-compile([export_all]).

solve(Input) ->
    {do_solve(parse_part1(Input)), do_solve(parse_part2(Input))}.

parse_part1(Input) ->
    [ string:tokens(Row, " ") || Row <- string:tokens(Input, "\n")].

parse_part2(Input) ->
    [ [ lists:sort(Str) || Str <- string:tokens(Row, " ")]
      || Row <- string:tokens(Input, "\n")].

do_solve(Rows) ->
    length(lists:filter(fun is_valid/1, Rows)).

is_valid(Row) ->
    length(lists:usort(Row)) =:= length(Row).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({386, 208}, advent2017:solve(?MODULE, "input/day4.txt"))
    ].
