-module(day8).
-compile([export_all]).

solve(Input) ->
    {Regs, Max} = run(parse(Input)),
    {lists:max(maps:values(Regs)), Max}.

run(Ops) ->
    lists:foldl(
      fun([A, Op, X, B, Cmp, Y], {Regs, Max}) ->
              case Cmp(maps:get(B, Regs, 0), Y) of
                  true  ->
                      Val = Op(maps:get(A, Regs, 0), X),
                      {maps:put(A, Val, Regs), max(Max, Val)};
                  false ->
                      {Regs, Max}
              end
      end, {#{}, 0}, Ops).

op("!=")                -> op("=/=");
op("<=")                -> op("=<");
op("inc")               -> op("+");
op("dec")               -> op("-");
op(Op) when is_list(Op) -> op(list_to_atom(Op));
op(Op) when is_atom(Op) -> fun erlang:Op/2.

parse(Input) ->
    lists:map(
      fun([A, Op, X, "if", B, Cmp, Y]) ->
              [A, op(Op), list_to_integer(X), B, op(Cmp), list_to_integer(Y)]
      end, [string:tokens(Line, " ") || Line <- string:tokens(Input, "\n")]).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({4647, 5590}, advent2017:solve(day8, "input/day8.txt"))
    ].
