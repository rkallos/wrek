-module(wrek_sleep_vert).

-behaviour(wrek_vert).
-export([run/2]).

run(_Args, Parent) ->
    Pwd = os:cmd("pwd") -- "\n",
    Fun = wrek_vert:exec(Parent, Pwd, "sleep 9000"),
    ok = Fun(),
    {ok, #{}}.
