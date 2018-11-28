-module(wrek_id_vert).

-behaviour(wrek_vert).

-export([run/2]).


run([Arg], _Pid) ->
    {ok, #{result => Arg}}.
