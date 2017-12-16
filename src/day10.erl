-module(day10).
-compile([export_all]).

solve(Input) ->
    {[X,Y|_], _, _} = hash(parse(Input), lists:seq(0, 255), 0, 0),
    {X*Y, knot_hash(Input)}.

parse(Input) ->
    [list_to_integer(X) || X <- string:tokens(Input, ",")].

knot_hash(Input0) ->
    Nums0 = lists:seq(0, 255),
    Lens = Input0 ++ [17, 31, 73, 47, 23],
    {Sparse, _, _} = lists:foldl(fun(_, {Nums, Pos, SkipSize}) ->
                                         hash(Lens, Nums, Pos, SkipSize)
                                 end, {Nums0, 0, 0}, lists:seq(1, 64)),
    hex(dense(Sparse)).

dense([]) ->
    [];
dense(List) ->
    {L, R} = lists:split(min(16, length(List)), List),
    [lists:foldl(fun(X, Acc) -> Acc bxor X end, 0, L)|dense(R)].

hex(L) ->
    lists:flatten([io_lib:format("~2.16.0b", [B]) || B <- L]).

hash([], Nums, Pos, SkipSize) ->
    {Nums, Pos, SkipSize};
hash([Len|Rest], Nums0, Pos, SkipSize) ->
    Nums = twist(Pos, Len, Nums0),
    hash(Rest, Nums, (Pos + Len + SkipSize) rem length(Nums), SkipSize + 1).

twist(Pos, Len, Nums) ->
    {L, R} = lists:split(Len, left_shift(Pos, Nums)),
    right_shift(Pos, lists:reverse(L) ++ R).

left_shift(Pos, Nums) ->
    {L, R} = lists:split(Pos, Nums),
    R ++ L.

right_shift(Pos, Nums) ->
    left_shift(length(Nums) - Pos, Nums).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({[3,4,2,1,0], 4, 4}, hash([3,4,1,5], lists:seq(0, 4), 0, 0))
    , ?_assertEqual("a2582a3a0e66e6e86e3812dcb672a272", knot_hash(""))
    , ?_assertEqual("33efeb34ea91902bb2f59c9920caa6cd", knot_hash("AoC 2017"))
    , ?_assertEqual({826, "d067d3f14d07e09c2e7308c3926605c4"},
                    advent2017:solve(day10, "input/day10.txt"))
    ].
