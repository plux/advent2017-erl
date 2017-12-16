-module(day14).
-compile([export_all]).

solve(Input) ->
    Grid = make_grid(Input),
    { lists:sum([count1(Row) || Row <- Grid])
    , solve_p2(Grid)
    }.

solve_p2(Grid) ->
    erase(),
    lists:foldl(
      fun(Row, Y) ->
              lists:foldl(
                fun($1, X) ->
                        put({X,Y}, x),
                        X+1;
                   (_, X) ->
                        X+1
                end, 0,  Row),
              Y+1
      end, 0, Grid),
    put(n, 0),
    [fill({X, Y}) || X <- lists:seq(0, 127),
                     Y <- lists:seq(0, 127),
                     get({X,Y}) =:= x],
    get(n).


fill({X, Y}) ->
    fill({X,Y}, put(n, get(n)+1)).

fill({X, Y}, C) ->
    Neighbors = [{X-1, Y}, {X+1, Y}, {X, Y-1}, {X, Y+1}],
    case get({X,Y}) of
        x ->
            put({X,Y}, get(C)),
            [fill(Pos, C) || Pos <- Neighbors];
        _ ->
            ok
    end.

make_grid(Input) ->
    Hashes = [day10:knot_hash(Input ++ "-" ++ integer_to_list(I)) || I <- lists:seq(0, 127)],
    [lists:flatten([hex_to_bin(C) || C <- Hash]) || Hash <- Hashes].

hex_to_bin($0) -> "0000";
hex_to_bin($1) -> "0001";
hex_to_bin($2) -> "0010";
hex_to_bin($3) -> "0011";
hex_to_bin($4) -> "0100";
hex_to_bin($5) -> "0101";
hex_to_bin($6) -> "0110";
hex_to_bin($7) -> "0111";
hex_to_bin($8) -> "1000";
hex_to_bin($9) -> "1001";
hex_to_bin($a) -> "1010";
hex_to_bin($b) -> "1011";
hex_to_bin($c) -> "1100";
hex_to_bin($d) -> "1101";
hex_to_bin($e) -> "1110";
hex_to_bin($f) -> "1111".

count1(L) ->
    lists:sum([1 || $1 <- L]).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({8108, 1242}, solve("flqrgnkx"))
    , ?_assertEqual({8230, 1103}, solve("hfdlxzhv"))
    ].
