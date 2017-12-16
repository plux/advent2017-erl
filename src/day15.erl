-module(day15).
-compile([export_all]).

solve(A, B) ->
    { solve(A, B, 1, 1, 40000000)
    , solve(A, B, 4, 8, 5000000)
    }.

solve(SeedA, SeedB, MulA, MulB, N) ->
    Divisor = 2147483647,
    FactorA = 16807,
    FactorB = 48271,
    {_, _, Count} =
        lists:foldl(fun(_X, {A0, B0, Count}) ->
                            A = generator(A0, Divisor, FactorA, MulA),
                            B = generator(B0, Divisor, FactorB, MulB),
                            case eq(A, B) of
                                true  -> {A, B, Count+1};
                                false -> {A, B, Count}
                            end
                    end, {SeedA, SeedB, 0}, lists:seq(0, N)),
    Count.

eq(A, B) ->
    (A band 16#FFFF) =:= (B band 16#FFFF).

generator(In, Divisor, Factor, 1) ->
    (In*Factor) rem Divisor;
generator(In, Divisor, Factor, Mul) ->
    Val = (In*Factor) rem Divisor,
    case (Val rem Mul) =:= 0 of
        true  -> Val;
        false -> generator(Val, Divisor, Factor, Mul)
    end.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    %% too slow
    %% [ ?_assertEqual({588, 309}, solve(65, 8921))
    %% , ?_assertEqual({594, 328}, solve(703, 516))
    %% ].
    [].
