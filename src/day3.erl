-module(day3).
-compile([export_all]).

-define(up, {0,-1}).
-define(down, {0, 1}).
-define(left, {-1, 0}).
-define(right, {1, 0}).

solve(Input) ->
    N = list_to_integer(binary_to_list(Input)),
    {part1(N), part2(N)}.

part1(N) ->
    erase(),
    {X, Y} = spiral_pos(N),
    abs(X) + abs(Y).

part2(N) ->
    erase(),
    put(max_val, N),
    catch spiral_pos(N).

spiral_pos(N) ->
    spiral_pos(?right, 1, 1, N, {0,0}).

spiral_pos(_Dir, _Steps, _StepsLeft, 1, Pos) ->
    Pos;
spiral_pos(Dir0, Steps, 0, N, Pos) ->
    put_val(Pos),
    {Dir, X} = turn(Dir0),
    spiral_pos(Dir, Steps+X, Steps+X-1, N-1, move(Dir, Pos));
spiral_pos(Dir, Steps, StepsLeft, N, Pos) ->
    put_val(Pos),
    spiral_pos(Dir, Steps, StepsLeft-1, N-1, move(Dir, Pos)).

turn(?up)    -> {?left, 1};
turn(?left)  -> {?down, 0};
turn(?down)  -> {?right, 1};
turn(?right) -> {?up, 0}.

move({DX, DY}, {X, Y}) ->
    {X+DX, Y+DY}.

put_val({0,0}) ->
    put({0,0}, 1);
put_val(Pos) ->
    case get(max_val) of
        undefined ->
            ok;
        MaxVal ->
            Dirs = [ ?up, ?down, ?left, ?right
                   , {1,1}, {-1,-1}, {1,-1}, {-1,1}
                   ],
            Sum = lists:sum([get(move(Dir, Pos), 0) || Dir <- Dirs]),
            case Sum > MaxVal of
                true  -> throw(Sum);
                false -> put(Pos, Sum)
            end
    end.

get(Key, Default) ->
    case get(Key) of
        undefined -> Default;
        Val       -> Val
    end.

%% 147  142  133  122   59
%% 304    5    4    2   57
%% 330   10    1    1   54
%% 351   11   23   25   26
%% 362  747  806--->   ...

%% 17  16  15  14  13
%% 18   5   4   3  12
%% 19   6   1   2  11
%% 20   7   8   9  10
%% 21  22  23---> ...

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({0,0},   spiral_pos(1))
    , ?_assertEqual({1,0},   spiral_pos(2))
    , ?_assertEqual({1,-1},  spiral_pos(3))
    , ?_assertEqual({0,-1},  spiral_pos(4))
    , ?_assertEqual({-1,-1}, spiral_pos(5))
    , ?_assertEqual({-1,0},  spiral_pos(6))
    , ?_assertEqual({-1,1},  spiral_pos(7))
    , ?_assertEqual({0, 1},  spiral_pos(8))
    , ?_assertEqual({1, 1},  spiral_pos(9))
    , ?_assertEqual({2, 1},  spiral_pos(10))
    , ?_assertEqual({2, 0},  spiral_pos(11))
    , ?_assertEqual({2, -1}, spiral_pos(12))
    , ?_assertEqual({2, -2}, spiral_pos(13))
    , ?_assertEqual({1, -2}, spiral_pos(14))
    , ?_assertEqual({0,-2},  spiral_pos(15))
    , ?_assertEqual({0, 2},  spiral_pos(23))
    , ?_assertEqual(2,   part1(23))
    , ?_assertEqual(31,  part1(1024))
    , ?_assertEqual(475, part1(277678))
    , ?_assertEqual({475, 279138}, advent2017:solve(?MODULE, "input/day3.txt"))
    ].
