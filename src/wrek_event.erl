-module(wrek_event).

-export([exec_output/3,
         wrek_done/2,
         wrek_error/3,
         wrek_msg/3,
         wrek_start/3,
         vert_done/3,
         vert_start/5,
         vert_msg/3]).

exec_output(Mgr, Id, Msg) ->
    gen_event:notify(Mgr, {wrek, Id, exec, Msg}).


wrek_done(Mgr, Id) ->
    gen_event:notify(Mgr, {wrek, Id, {wrek, done}}).


wrek_error(Mgr, Id, Msg) ->
    gen_event:notify(Mgr, {wrek, Id, {wrek, error}, Msg}).


wrek_msg(Mgr, Id, Msg) ->
    gen_event:notify(Mgr, {wrek, Id, {wrek, msg}, Msg}).


wrek_start(Mgr, Id, Map) ->
    gen_event:notify(Mgr, {wrek, Id, {wrek, start}, Map}).


vert_done(Mgr, Id, Res) ->
    gen_event:notify(Mgr, {wrek, Id, {vert, done}, Res}).


vert_start(Mgr, Id, Name, Module, Args) ->
    gen_event:notify(Mgr, {wrek, Id, {vert, start}, {Name, Module, Args}}).


vert_msg(Mgr, Id, Msg) ->
    gen_event:notify(Mgr, {wrek, Id, {vert, msg}, Msg}).
