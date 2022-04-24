-module(wrek_vert_runner).

-export([start_link/1,
         set_stop_on_completion/2,
         send_request/4,
         check_response/2,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% #{stop_on_completion => true (default) | false}
-spec start_link(#{atom() => term()}) -> {ok, pid()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec set_stop_on_completion(pid(), boolean()) -> ok.
set_stop_on_completion(Pid, Flag) ->
    gen_server:cast(Pid, {set, stop_on_completion, Flag}).

-spec send_request(pid(), atom(), term(), pid()) -> term().
send_request(Pid, Module, Args, Parent) ->
    gen_server:send_request(Pid, {run, Module, Args, Parent}).

-spec check_response(term(), term()) -> {reply, term()} | no_reply.
check_response(Msg, RunId) ->
    gen_server:check_response(Msg, RunId).

-spec init(#{}) -> {ok, #{}}.
init(#{}=Args) ->
    {ok, Args}.

-spec handle_call(term(), term(), map()) -> {stop, term(), term(), map()} | {reply, term(), map()}.
handle_call({run, Module, Args, Parent}, _From, State) ->
    Result = Module:run(Args, Parent),
    case maps:get(stop_on_completion, State, true) of
        true ->
            {stop, normal, Result, State};
        false ->
            {reply, Result, State}
    end.

-spec handle_cast({set, atom(), term()}, map()) -> {noreply, map()}.
handle_cast({set, Key, Var}, State) ->
    {noreply, State#{Key => Var}}.

-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extras) ->
    {ok, State}.

