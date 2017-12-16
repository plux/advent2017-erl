-module(day13).
-compile([export_all]).

solve(Input) ->
    FWs  = parse(Input),
    Keys = lists:max(maps:keys(FWs)),
    { penetrate(0, Keys, FWs, fun() -> ok end)
    , find_best(0, Keys, FWs)
    }.

find_best(Delay, Keys, FWs) ->
    case catch penetrate(0, Keys, FWs, fun() -> throw(caught) end)
    of
        0      -> Delay;
        caught -> find_best(Delay+1, Keys, move_fw(FWs))
    end.

penetrate(T, Num, _, _) when T > Num ->
    0;
penetrate(T, Num, FWs, F) ->
    case maps:get(T, FWs, nil) of
        {0, _, Size} ->
            F(),
            Size*T + penetrate(T+1, Num, move_fw(FWs), F);
        _ ->
            penetrate(T+1, Num, move_fw(FWs), F)
    end.

move_fw(FWs) ->
    maps:map(fun(_, {Pos, 1, Size}) when Pos =:= (Size-1) -> {Pos-1, -1, Size};
                (_, {0, -1, Size})                        -> {1, 1, Size};
                (_, {Pos, Move, Size})                    -> {Pos+Move, Move, Size}
             end, FWs).

parse(Input) ->
    maps:from_list(lists:map(fun(Line) ->
                                     [Id, Size] = string:tokens(Line, ": "),
                                     {list_to_integer(Id), {0, 1, list_to_integer(Size)}}
                             end, string:tokens(Input, "\n"))).


-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = "0: 3\n"
        "1: 2\n"
        "4: 4\n"
        "6: 4",
    [ ?_assertEqual({24,10}, solve(Input))
    %% Too slow
    %%, ?_assertEqual({1632,3834136}, advent2017:solve(day13, "input/day13.txt")
    ].
