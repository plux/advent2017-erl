-module(day18).
-compile([export_all]).

-record(p, {pc = 1, ops = [], regs = #{}, inbox = queue:new()}).

solve(Input) ->
    { run(#p{ops = parse(Input)})
    , scheduler( #p{ops = parse(Input), regs = #{"p" => 0}}
               , #p{ops = parse(Input), regs = #{"p" => 1}}
               , 0)
    }.

parse(Input) ->
    [string:tokens(Line, " ") || Line <- string:tokens(Input, "\n")].

run(#p{pc = PC, ops = Ops, regs = Regs} = P) ->
    case lists:nth(PC, Ops) of
        ["snd", X]    -> run(P#p{pc = PC+1, regs = Regs#{last_played => val(X, Regs)}});
        ["set", X, Y] -> run(P#p{pc = PC+1, regs = Regs#{X => val(Y, Regs)}});
        ["add", X, Y] -> run(P#p{pc = PC+1, regs = Regs#{X => val(X, Regs) + val(Y, Regs)}});
        ["mul", X, Y] -> run(P#p{pc = PC+1, regs = Regs#{X => val(X, Regs) * val(Y, Regs)}});
        ["mod", X, Y] -> run(P#p{pc = PC+1, regs = Regs#{X => val(X, Regs) rem val(Y, Regs)}});
        ["rcv", X]    ->
            case val(X, Regs) of
                0     -> run(P#p{pc = PC+1});
                _Else -> {recovered, maps:get(last_played, Regs)}
            end;
        ["jgz", X, Y] ->
            case val(X, Regs) > 0 of
                true  -> run(P#p{pc = PC + val(Y, Regs)});
                false -> run(P#p{pc = PC + 1})
            end
    end.

run2(#p{pc = PC, ops = Ops, regs = Regs, inbox = Inbox} = P) ->
    case lists:nth(PC, Ops) of
        ["snd", X]    -> {send, val(X, Regs), P#p{pc = PC+1}};
        ["set", X, Y] -> run2(P#p{pc = PC+1, regs = Regs#{X => val(Y, Regs)}});
        ["add", X, Y] -> run2(P#p{pc = PC+1, regs = Regs#{X => val(X, Regs) + val(Y, Regs)}});
        ["mul", X, Y] -> run2(P#p{pc = PC+1, regs = Regs#{X => val(X, Regs) * val(Y, Regs)}});
        ["mod", X, Y] -> run2(P#p{pc = PC+1, regs = Regs#{X => val(X, Regs) rem val(Y, Regs)}});
        ["rcv", X]    ->
            case queue:out(Inbox) of
                {empty, _}      -> {recv, P};
                {{value, V}, Q} ->
                    run2(P#p{pc = PC+1, regs = Regs#{X => V}, inbox = Q})
            end;
        ["jgz", X, Y] ->
            case val(X, Regs) > 0 of
                true  -> run2(P#p{pc = PC + val(Y, Regs)});
                false -> run2(P#p{pc = PC + 1})
            end
    end.

scheduler(A0, B0, Sent) ->
    case {run2(A0), run2(B0)} of
        {{recv, _}, {recv, _}}             -> {deadlock, Sent};
        {{send, ValA, A}, {send, ValB, B}} -> scheduler(send(ValB, A), send(ValA, B), Sent+1);
        {{send, ValA, A}, {recv, B}}       -> scheduler(A, send(ValA, B), Sent);
        {{recv, A}, {send, ValB, B}}       -> scheduler(send(ValB, A), B, Sent+1)
    end.

send(V, #p{inbox = Inbox} = P) ->
    P#p{inbox = queue:in(V, Inbox)}.

val([X], Regs) when X >= $a, X =< $z -> maps:get([X], Regs, 0);
val(X, _Regs)                        -> list_to_integer(X).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    In = "set a 1\n"
         "add a 2\n"
         "mul a a\n"
         "mod a 5\n"
         "snd a\n"
         "set a 0\n"
         "rcv a\n"
         "jgz a -1\n"
         "set a 1\n"
         "jgz a -2\n",
    [ ?_assertEqual({{recovered, 4}, {deadlock, 1}}, solve(In))
    , ?_assertEqual({{recovered, 7071}, {deadlock, 8001}}, advent2017:solve(day18, "input/day18.txt"))
    ].
