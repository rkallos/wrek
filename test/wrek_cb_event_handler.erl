-module(wrek_cb_event_handler).
-include("wrek_event.hrl").

-export([code_change/3,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         init/1,
         terminate/2]).
-behaviour(gen_event).


-record(state, {
    caller = undefined :: pid(),
    count = 0 :: integer(),
    evts = [] :: [wrek_event()],
    fail_mode = total :: total | partial
}).


%% callbacks

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(get, State) ->
    {ok, State#state.count, State};

handle_call(_, State) ->
    {ok, ok, State}.


handle_event(#wrek_event{type = exec, msg = {_, Data}}, Pid) ->
    Pid ! {event_handler, Data},
    {ok, Pid};

handle_event(Evt, State) ->
    {ok, State}.


handle_info(_, State) ->
    {ok, State}.


init([CallbackPid]) ->
    {ok, CallbackPid}.


terminate(_, _State) ->
    ok.
