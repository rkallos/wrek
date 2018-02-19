-module(wrek_event).
-include("wrek_event.hrl").

-export([exec_output/3,
         wrek_done/2,
         wrek_error/3,
         wrek_msg/3,
         wrek_start/3,
         vert_done/3,
         vert_start/5,
         vert_msg/3]).

exec_output(Mgr, Id, Msg) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = exec, msg = Msg}).


wrek_done(Mgr, Id) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = {wrek, done}}).


wrek_error(Mgr, Id, Msg) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = {wrek, error}, msg = Msg}).


wrek_msg(Mgr, Id, Msg) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = {wrek, msg}, msg = Msg}).


wrek_start(Mgr, Id, Map) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = {wrek, start}, msg = Map}).


vert_done(Mgr, Id, Res) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = {vert, done}, msg = Res}).


vert_start(Mgr, Id, Name, Module, Args) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = {vert, start}, msg = {Name, Module, Args}}).


vert_msg(Mgr, Id, Msg) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = {vert, msg}, msg = Msg}).
