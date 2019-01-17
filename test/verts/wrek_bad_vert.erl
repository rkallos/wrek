-module(wrek_bad_vert).

-behavior(wrek_vert).

-export([run/2]).


run(_Args, Parent) ->
    Pwd = os:cmd("pwd") -- "\n",
    {ok, Fun} = wrek_vert:exec(Parent, Pwd, "exit 1"),
    ok = Fun(), % should badmatch
    {ok, #{}}.
