-module(wrek_test_handler).
-include("wrek_event.hrl").

-export([handle_call/2,
         handle_event/2,
         handle_info/2,
         init/1,
         terminate/2]).
-behaviour(gen_event).


-record(state, {
          caller = undefined :: pid(),
          count = 0 :: integer()
         }).


%% callbacks

handle_call(get, State) ->
    {ok, State#state.count, State};

handle_call(_, State) ->
    {ok, ok, State}.


handle_event(#wrek_event{type = {wrek, error}}, State) ->
    #state{
       caller = Caller,
       count = Count
      } = State,
    Caller ! (Count + 1),
    remove_handler;

handle_event(#wrek_event{type = {wrek, done}}, State) ->
    #state{
       caller = Caller,
       count = Count
      } = State,
    Caller ! (Count + 1),
    remove_handler;

handle_event(_Evt, State = #state{count = Count}) ->
    {ok, State#state{count = Count + 1}}.


handle_info(_, State) ->
    {ok, State}.


init([Caller]) ->
    {ok, #state{caller = Caller}}.


terminate(_, _State) ->
    ok.
