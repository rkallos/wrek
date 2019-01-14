-module(wrek_reuse_sandbox_vert).

-behaviour(wrek_vert).

-export([run/2]).

run([Who], Pid) ->
    {ok, Dir} = wrek_vert:reuse_sandbox(Pid, Who),
    wrek_vert:notify(Pid, {dir, Dir}),
    {ok, #{dir => Dir}}.
