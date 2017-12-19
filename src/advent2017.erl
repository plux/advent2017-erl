-module(advent2017).

-export([main/1]).
-export([solve/2]).

%% escript Entry point
main([]) ->
    {ok, Files} = file:list_dir("input"),
    solve(Files),
    erlang:halt(0);
main([Arg]) ->
    solve([Arg ++ ".txt"]),
    erlang:halt(0).

solve(Files) ->
    lists:map(
      fun(F) ->
              Mod = list_to_atom(hd(string:tokens(F, "."))),
              {T, Answer} = timer:tc(fun() -> solve(Mod, "input/" ++ F) end),
              io:format("~s: ~p (~p ms)\n",
                        [Mod, Answer, trunc(math:ceil(T / 1000))])
      end, lists:sort(Files)).

solve(Mod, File) ->
    {ok, Input} = file:read_file(File),
    Mod:solve(string:chomp(binary_to_list(Input))).
