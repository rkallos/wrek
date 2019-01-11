-module(wrek_get_sandbox_vert).

-behaviour(wrek_vert).

-export([run/2]).

run([Dep], Pid) ->
    Dir = wrek_vert:get_sandbox(Pid, Dep),
    wrek_vert:notify(Pid, {dir, Dir}),
    case Dir of
        undefined ->
            {error, undefined};
        D ->
            {ok, #{dir => Dir}}
    end.
