-module(wrek_get_vert).

-behaviour(wrek_vert).

-export([run/2]).

run(Pairs, Pid) ->
    Vals = lists:foldl(fun({Key, From}, Acc) ->
        Acc#{From => wrek_vert:get(Pid, From, Key)}
    end, #{}, Pairs),
    {ok, Vals}.
