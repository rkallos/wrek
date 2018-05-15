-module(wrek_test_handler).
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


handle_event(Evt = #wrek_event{type = {wrek, error}},
    State = #state{fail_mode = total}) ->
    finish(Evt, State);

handle_event(Evt = #wrek_event{type = {wrek, done}}, State) ->
    finish(Evt, State);

handle_event(Evt, State = #state{count = Count, evts = Evts}) ->
    {ok, State#state{count = Count + 1, evts = [Evt | Evts]}}.


handle_info(_, State) ->
    {ok, State}.


init([FailMode, Caller]) ->
    {ok, #state{caller = Caller, fail_mode = FailMode}}.


terminate(_, _State) ->
    ok.


%% private

finish(Evt, State) ->
    #state{
       caller = Caller,
       count = Count,
       evts = Evts
    } = State,
    Caller ! #{count => Count + 1, evts => [Evt | Evts]},
    remove_handler.
