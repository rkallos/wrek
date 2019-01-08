-module(wrek_echo_vert).

-behaviour(wrek_vert).
-export([run/2]).

run([Callback, Arg], Parent) ->
    Pwd = os:cmd("pwd") -- "\n",
    Cmd = io_lib:format("echo -n ~s", [Arg]),
    Fun = wrek_vert:exec(Parent, Pwd, Cmd, [], Callback),
    ok = Fun(),
    {ok, #{}}.
