-module(wrek_true_vert).

-behavior(wrek_vert).

-export([run/2]).


run(_Args, Parent) ->
    Pwd = os:cmd("pwd") -- "\n",
    Fun = wrek_vert:exec(Parent, Pwd, "true"),
    ok = Fun(),
    {ok, #{}}.
