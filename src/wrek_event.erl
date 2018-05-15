-module(wrek_event).
-include("wrek_event.hrl").

-export([exec_output/3,
         time_diff/2,
         time_diff/3,
         wrek_done/2,
         wrek_error/3,
         wrek_msg/3,
         wrek_start/3,
         vert_done/3,
         vert_start/5,
         vert_msg/3]).


-spec time_diff(wrek_event(), wrek_event()) -> non_neg_integer().

time_diff(E1, E2) ->
    time_diff(E1, E2, microsecond).


-spec time_diff(wrek_event(), wrek_event(), erlang:time_unit()) ->
    non_neg_integer().

time_diff(#wrek_event{timestamp = T1}, #wrek_event{timestamp = T2}, TimeUnit) ->
    erlang:convert_time_unit(T2 - T1, native, TimeUnit).


exec_output(undefined, _, _) -> ok;
exec_output(Mgr, Id, Msg) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = exec, msg = Msg}).


wrek_done(undefined, _) -> ok;
wrek_done(Mgr, Id) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = {wrek, done}}).


wrek_error(undefined, _, _) -> ok;
wrek_error(Mgr, Id, Msg) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = {wrek, error}, msg = Msg}).


wrek_msg(undefined, _, _) -> ok;
wrek_msg(Mgr, Id, Msg) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = {wrek, msg}, msg = Msg}).


wrek_start(undefined, _, _) -> ok;
wrek_start(Mgr, Id, Map) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = {wrek, start}, msg = Map}).


vert_done(undefined, _, _) -> ok;
vert_done(Mgr, Id, Res) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = {vert, done}, msg = Res}).


vert_start(undefined, _, _, _, _) -> ok;
vert_start(Mgr, Id, Name, Module, Args) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = {vert, start}, msg = {Name, Module, Args}}).


vert_msg(undefined, _, _) -> ok;
vert_msg(Mgr, Id, Msg) ->
    gen_event:notify(Mgr, #wrek_event{id = Id, type = {vert, msg}, msg = Msg}).
