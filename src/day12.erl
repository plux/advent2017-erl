-module(day12).
-compile([export_all]).

solve(Input) ->
    Graph = parse(Input),
    Comps = digraph_utils:components(Graph),
    { length(hd([Comp || Comp <- Comps, lists:member("0", Comp)]))
    , length(Comps)
    }.

parse(Input) ->
    Graph = digraph:new(),
    lists:foreach(fun(Line) ->
                          [Prog|Conns] = string:tokens(Line, "<-> ,"),
                          digraph:add_vertex(Graph, Prog),
                          [digraph:add_edge(Graph, Prog, Conn) || Conn <- Conns]
                  end, string:tokens(Input, "\n")),
    Graph.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = "0 <-> 2\n"
        "1 <-> 1\n"
        "2 <-> 0, 3, 4\n"
        "3 <-> 2, 4\n"
        "4 <-> 2, 3, 6\n"
        "5 <-> 6\n"
        "6 <-> 4, 5",
    [ ?_assertEqual({6, 2}, solve(Input))
    , ?_assertEqual({380, 181}, advent2017:solve(day12, "input/day12.txt"))
    ].
