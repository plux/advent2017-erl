-module(day7).
-compile([export_all]).

solve(Input) ->
    parse(Input),
    {find_root(get(start)), solve_part2(Input)}.

solve_part2(Input) ->
    %% TODO: Clean up and fix code
    parse(Input),
    Root = find_root(get(start)),
    find_unbalanced(Root).

parse(Input) ->
    erase(),
    lists:map(fun([Node, Weight]) ->
                      put(start, Node),
                      put({children, Node}, []),
                      put({weight, Node}, list_to_integer(Weight));
                  ([Node, Weight|Children]) ->
                      put({children, Node}, Children),
                      put({weight, Node}, list_to_integer(Weight)),
                      lists:foreach(fun(Child) ->
                                            put({parent, Child}, Node)
                                    end, Children)
               end, [string:tokens(Row, "() ->,") || Row <- string:tokens(Input, "\n")]).

find_unbalanced(Node) ->
    case get({children, Node}) of
        undefined ->
            error({no_children, Node});
        [] ->
            Node;
        Children ->
            L = [{calc_weight(Child), Child} || Child <- Children],
            {Weights, _} = lists:unzip(L),
            case lists:partition(fun(X)-> hd(Weights) =:= X end, Weights) of
                {[Weight], _} ->
                    find_unbalanced(element(2, lists:keyfind(Weight, 1, L)));
                {_, [Weight]} ->
                    find_unbalanced(element(2, lists:keyfind(Weight, 1, L)));
                {_, []} ->
                    Parent = get({parent, Node}),
                    Siblings = get({children, Parent}),
                    io:format("~p\n",
                              [[{calc_weight(Child), Child} || Child <- Siblings]]),
                    io:format("bad weight: ~p\n", [get({weight, Node})]),
                    Node
            end
    end.

calc_weight(Node) ->
    get({weight, Node}) +
        lists:sum([calc_weight(Child) || Child <- get({children, Node})]).

find_root(Node) ->
    case get({parent, Node}) of
        undefined ->
            Node;
        Parent ->
            find_root(Parent)
    end.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertMatch({"airlri", _}, advent2017:solve(day7, "input/day7.txt"))
    ].
