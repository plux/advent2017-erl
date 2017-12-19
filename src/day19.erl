-module(day19).
-compile([export_all]).

-define(up, {0,-1}).
-define(down, {0,1}).
-define(left, {-1,0}).
-define(right, {1,0}).
-define(dirs, [?up, ?down, ?left, ?right]).

solve(Input) ->
    parse(Input),
    {X,Y} = find_start({0,0}),
    follow_line($|, {X,Y}, [], [], ?down).

find_start({X,Y}) ->
    case get({X,Y}) of
        $| -> {X,Y};
        _  -> find_start({X+1,Y})
    end.

follow_line($+, Pos, Visited, Collected, _Dir) ->
    io:format("~p ~p\n", [Pos, [get(Pos)]]),
    [NextDir] = lists:filter(fun(NextDir) ->
                                     NextPos = move(Pos, NextDir),
                                     C = get(NextPos),
                                     C =/= $  andalso
                                         C =/= undefined andalso
                                         not lists:member(NextPos, Visited)
                             end, ?dirs),
    NextPos = move(Pos, NextDir),
    follow_line(get(NextPos), NextPos, [Pos|Visited], Collected, NextDir);
follow_line(C, Pos, Visited, Collected, Dir) when C =/= $  ->
    NextPos = move(Pos, Dir),
    follow_line(get(NextPos), NextPos, [Pos|Visited], maybe_collect(get(Pos), Collected), Dir);
follow_line(_C, _Pos, Visited, Collected, _Dir) ->
    {lists:reverse(Collected), length(Visited)}.

maybe_collect(C, Collected) when C >= $A, C =< $Z -> [C|Collected];
maybe_collect(_, Collected)                       -> Collected.

move({X, Y}, {DX, DY}) ->
    {X+DX, Y+DY}.

parse(Input) ->
    erase(),
    lists:foldl(
      fun(Line, Y) ->
              lists:foldl(
                fun(C, X) ->
                        put({X,Y}, C),
                        X+1
                end, 0,  Line),
              Y+1
      end, 0, string:tokens(Input, "\n")).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    In = "     |          \n"
         "     |  +--+    \n"
         "     A  |  C    \n"
         " F---|----E|--+ \n"
         "     |  |  |  D \n"
         "     +B-+  +--+ \n",
    [ ?_assertEqual({"ABCDEF", 38}, solve(In))
    , ?_assertEqual({"VEBTPXCHLI", 18702}, advent2017:solve(day19, "input/day19.txt"))
    ].
