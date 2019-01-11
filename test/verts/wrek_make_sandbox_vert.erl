-module(wrek_make_sandbox_vert).

-behaviour(wrek_vert).

-export([run/2]).

run(_, Pid) ->
    Dir = wrek_vert:make_sandbox(Pid),
    wrek_vert:notify(Pid, {dir, Dir}),
    {ok, #{dir => Dir}}.
