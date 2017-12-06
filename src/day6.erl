-module(day6).
-compile([export_all]).

solve(Input) ->
    solve(parse(Input), 1).

parse(Input) ->
    erase(),
    lists:foldl(fun(N, Acc) ->
                        put(Acc, list_to_integer(N)),
                        Acc+1
                end, 0, string:tokens(Input, "\t")).

solve(Length, N) ->
    MaxI = find_max(Length),
    Max = get(MaxI),
    put(MaxI, 0),
    ok = distribute(MaxI+1, Max, Length),
    case get(banks_to_list(Length)) of
        undefined ->
            put(banks_to_list(Length), N),
            solve(Length, N+1);
        I ->
            {N, N-I}
    end.

find_max(Length) ->
    Max = lists:max(banks_to_list(Length)),
    hd([I || I <- lists:seq(0, Length-1), get(I) =:= Max]).

distribute(_, 0, _) ->
    ok;
distribute(I, N, I) ->
   distribute(0, N, I);
distribute(I, N, Length) ->
    X = get(I),
    put(I, X+1),
    distribute(I+1, N-1, Length).

banks_to_list(Length) ->
    [get(I) || I <- lists:seq(0, Length-1)].

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({5,4}, day6:solve("0\t2\t7\t0"))
    , ?_assertEqual({3156, 1610}, advent2017:solve(?MODULE, "input/day6.txt"))
    ].
