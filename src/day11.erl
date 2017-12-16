-module(day11).
-compile([export_all]).

%% Using cube coordinates: https://www.redblobgames.com/grids/hexagons/
solve(Input) ->
    Dirs = string:tokens(Input, ","),
    {_, Dist, Max} = lists:foldl(
                       fun(Dir, {{X0, Y0, Z0}, _, Max}) ->
                               {DX, DY, DZ} = cube_coord(Dir),
                               {X, Y, Z} = {X0 + DX, Y0 + DY, Z0 + DZ},
                               Distance = (abs(X) + abs(Y) + abs(Z)) div 2,
                               {{X, Y, Z}, Distance, max(Distance, Max)}
                       end, {{0,0,0}, 0, 0}, Dirs),
    {Dist, Max}.

cube_coord("n")  -> { 0, 1,-1};
cube_coord("s")  -> { 0,-1, 1};
cube_coord("ne") -> { 1, 0,-1};
cube_coord("se") -> { 1,-1, 0};
cube_coord("nw") -> {-1, 1, 0};
cube_coord("sw") -> {-1, 0, 1}.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertMatch({3,3}, solve("ne,ne,ne"))
    , ?_assertEqual({0,2}, solve("ne,ne,sw,sw"))
    , ?_assertEqual({2,2}, solve("ne,ne,s,s"))
    , ?_assertEqual({3,3}, solve("se,sw,se,sw,sw"))
    , ?_assertEqual({722, 1551}, advent2017:solve(day11, "input/day11.txt"))
    ].
