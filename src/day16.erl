-module(day16).
-compile([export_all]).

solve(Input) ->
    solve(Input, <<"abcdefghijklmnop">>).

solve(Input, Str) ->
    { run(parse(Input), Str)
    , solve_part2(parse(Input), Str)
    }.

solve_part2(Ops, Str) ->
    N     = find_cycle(Ops, Str, Str, 1),
    Times = 1000000000 rem N,
    lists:foldl(fun(_, Acc) ->
                        run(Ops, Acc)
                end, Str, lists:seq(1, Times)).

find_cycle(Ops, Orig, Acc, N) ->
    case run(Ops, Acc) of
        Orig -> N;
        Next -> find_cycle(Ops, Orig, Next, N+1)
    end.

run([], Str)        -> Str;
run([Op|Rest], Str) -> run(Rest, perform(Op, Str)).

perform({s, N}, Str) ->
    Pos = size(Str) - N,
    <<Begin:Pos/binary, End/binary>> = Str,
    <<End/binary, Begin/binary>>;
perform({x, [X, Y]}, Str) ->
    %% swap pos
    YOffset = (Y-X-1),
    <<Begin:X/binary, ValX, Middle:YOffset/binary, ValY, End/binary>> = Str,
    <<Begin/binary, ValY, Middle/binary, ValX, End/binary>>;
perform({p, [A, B]}, Str0) ->
    %% swap chars
    Str1 = binary:replace(Str0, A, <<"X">>),
    Str2 = binary:replace(Str1, B, A),
    binary:replace(Str2, <<"X">>, B).


parse(Input) ->
    [parse_op(Op) || Op <- string:tokens(Input, ",")].

parse_op([$s|Rest]) -> {s, list_to_integer(Rest)};
parse_op([$x|Rest]) -> {x, lists:sort([list_to_integer(S) || S <- string:tokens(Rest, "/")])};
parse_op([$p|Rest]) -> {p, [list_to_binary(S) || S <- string:split(Rest, "/")]}.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({<<"baedc">>,<<"abcde">>}, solve("s1,x3/4,pe/b", <<"abcde">>))
    , ?_assertEqual({<<"fnloekigdmpajchb">>,<<"amkjepdhifolgncb">>},
                     advent2017:solve(day16, "input/day16.txt"))
    ].
