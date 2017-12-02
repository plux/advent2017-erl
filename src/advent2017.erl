-module(advent2017).

-export([main/1]).
-export([solve/2]).

%% escript Entry point
main([Arg]) ->
    main([Arg, "input/" ++ Arg ++ ".txt"]);
main([Arg, File]) ->
    Answer = solve(list_to_atom(Arg), File),
    io:format("~s: ~p\n", [Arg, Answer]),
    erlang:halt(0).

solve(Mod, File) ->
    {ok, Input} = file:read_file(File),
    Mod:solve(string:trim(Input)).
