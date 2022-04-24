-module(wrek_vert).

-export([
    exec/3,
    exec/4,
    exec/5,
    get/3,
    get/4,
    get_all/1,
    get_sandbox/2,
    make_sandbox/1,
    reuse_sandbox/2,
    notify/2
]).

-behaviour(gen_server).
-export([
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2
]).

-record(state, {
    child     = undefined :: pid()                       | undefined,
    data      = undefined :: #{any() => wrek_vert_t:t()} | undefined,
    event_mgr = undefined :: pid()                       | undefined,
    parent    = undefined :: pid()                       | undefined,
    pids      = #{}       :: #{pid() => [pid()] | {result, any()}},
    vert      = undefined :: wrek_vert_t:t()             | undefined,
    run_id    = undefined :: term()
}).
-type state() :: #state{}.


-callback run(Args :: list(), Parent :: pid()) ->
    {ok, Result :: any()} | {error, Reason :: any()}.


-spec exec(pid(), file:filename_all(), string()) ->
    {ok, fun()} | {error, any()}.

exec(Pid, Dir0, Cmd0) ->
    gen_server:call(Pid, {exec, Dir0, Cmd0, []}).


-spec exec(pid(), file:filename_all(), string(), list()) ->
    {ok, fun()} | {error, any()}.

exec(Pid, Dir0, Cmd0, Env) ->
    gen_server:call(Pid, {exec, Dir0, Cmd0, Env}).


-spec exec(pid(), file:filename_all(), string(), list(), fun((any()) -> any())) ->
    {ok, fun()} | {error, any()}.

exec(Pid, Dir0, Cmd0, Env, EventFun) ->
    gen_server:call(Pid, {exec, Dir0, Cmd0, Env, EventFun}).


-spec get(pid(), any(), any()) -> any().

get(Pid, Who, Key) ->
    get(Pid, Who, Key, undefined).


-spec get(pid(), any(), any(), any()) -> any().

get(Pid, Who, Key, Default) ->
    gen_server:call(Pid, {get, Who, Key, Default}).


-spec get_all(pid()) -> map().

get_all(Pid) ->
    gen_server:call(Pid, get_all).


-spec get_sandbox(pid(), any()) -> file:filename_all() | undefined.

get_sandbox(Pid, Who) ->
    gen_server:call(Pid, {get_sandbox, Who}).


-spec make_sandbox(pid()) -> file:filename_all().

make_sandbox(Pid) ->
    gen_server:call(Pid, make_sandbox).


-spec reuse_sandbox(pid(), any()) -> {ok, file:filename_all()} | {error, term()}.

reuse_sandbox(Pid, Who) ->
    gen_server:call(Pid, {reuse_sandbox, Who}).


-spec notify(pid(), any()) -> ok.

notify(Pid, Msg) ->
    gen_server:cast(Pid, {notify, Msg}).


%% callbacks

-spec code_change(_, _, state()) -> {ok, state()}.

code_change(_Req, _From, State) ->
    {ok, State}.


-spec handle_call(_, _, state()) -> {reply, fun(), state()}.

handle_call({exec, Dir, Cmd, Env}, _From, State = #state{event_mgr = EvMgr}) ->
    EventFun =
        fun(Msg) ->
            wrek_event:exec_output(EvMgr, id(State), Msg)
        end,
    handle_exec(Dir, Cmd, Env, EventFun, State);

handle_call({exec, Dir, Cmd, Env, EventFun0}, _From, State = #state{event_mgr = EvMgr}) ->
    EventFun =
        fun(Msg) ->
            wrek_event:exec_output(EvMgr, id(State), Msg),
            EventFun0(Msg)
        end,
    handle_exec(Dir, Cmd, Env, EventFun, State);

handle_call({get, Who0, Key, Default}, _From, State) ->
    Who = case Who0 of
        me -> name(State);
        Other -> Other
    end,

    case State#state.data of
        #{Who := Vert} ->
            case wrek_vert_t:kv(Vert) of
                #{Key := Val} ->
                    {reply, Val, State};
                _ ->
                    {reply, Default, State}
            end;
        _ ->
            {reply, Default, State}
    end;

handle_call(get_all, _From, State = #state{data = Data0}) ->
    Data = maps:fold(fun(Name, Vert, Acc) ->
        D = wrek_vert_t:kv(Vert),
        Acc#{Name => D}
    end, #{}, Data0),
    {reply, Data, State};

handle_call({get_sandbox, Who0}, _From, State) ->
    #state{data = Data} = State,
    Who = case Who0 of
        me -> name(State);
        Other -> Other
    end,

    case Data of
        #{Who := Vert} ->
            {reply, wrek_vert_t:dir(Vert), State};
        _ ->
            {reply, undefined, State}
    end;

handle_call(make_sandbox, _From, State = #state{data = Data, parent = Parent}) ->
    {_DagId, VertId} = id(State),
    DagDir = dag_dir(Parent),
    VertStr = integer_to_list(VertId),
    Dir = wrek_utils:sandbox(DagDir, VertStr),
    {ok, Vert2} = wrek:put_sandbox(Parent, Dir),
    Data2 = Data#{name(State) => Vert2},
    {reply, Dir, State#state{data = Data2, vert = Vert2}};

handle_call({reuse_sandbox, Who}, _From, State = #state{parent = Parent}) ->
    #state{data = Data} = State,
    {Reply, State2} = case Data of
        #{Who := V} ->
            case wrek_vert_t:dir(V) of
                undefined ->
                    {{error, {no_sandbox, Who}}, State};
                Dir ->
                    {ok, Vert2} = wrek:put_sandbox(Parent, Dir),
                    Data2 = Data#{name(State) => Vert2},
                    {{ok, Dir}, State#state{data = Data2, vert = Vert2}}
            end;
        _ ->
            {{error, {no_vert, Who, Data}}, State}
    end,
    {reply, Reply, State2};

handle_call({waitpid, Pid}, From, State) ->
    State2 = add_waitpid(From, Pid, State),
    {noreply, State2};

handle_call(_Req, _From, State) ->
    {reply, ok, State}.


-spec handle_cast(_, state()) -> {noreply, state()}.

handle_cast({notify, Msg}, State = #state{event_mgr = EvMgr}) ->
    wrek_event:vert_msg(EvMgr, id(State), Msg),
    {noreply, State};

handle_cast(_Req, State) ->
    {noreply, State}.


-spec handle_info(_, state()) ->
    {noreply, state()} | {stop, normal | {error, _}, state()}.

handle_info(timeout, State = #state{event_mgr = EvMgr}) ->
    wrek_event:vert_done(EvMgr, id(State), timeout),
    {stop, {shutdown, timeout}, State};

handle_info({'EXIT', Pid, Term} = ExitMsg, State) ->
    #state{
        child = Child,
        event_mgr = EvMgr,
        pids = Pids
    } = State,
    case Pid of
        Child ->
            %% Child exits unexpectedly
            wrek_event:vert_done(EvMgr, id(State), Term),
            {stop, {shutdown, Term}, State};
        _ ->
            case Pids of
                #{Pid := Waits} ->
                    Result = case Term of
                        normal -> ok;
                        {exit_status, _Status} = Exit ->
                            {error, Exit};
                        _ -> Term
                    end,
                    lists:foreach(fun(From) ->
                        gen_server:reply(From, Result)
                    end, Waits),
                    Pids2 = Pids#{Pid => {result, Result}},
                    State2 = State#state{pids = Pids2},
                    {noreply, State2};
                _ ->
                    Msg = io_lib:format("unexpected msg: ~p", [ExitMsg]),
                    wrek_event:vert_msg(EvMgr, id(State), Msg),
                    {noreply, State}
            end
    end;

handle_info(Msg, State) ->
    #state{
        event_mgr = EvMgr,
        run_id = RunId
    } = State,
    case wrek_vert_runner:check_response(Msg, RunId) of
        {reply, Term} ->
            %% Child finishes executing the run/2 function
            wrek_event:vert_done(EvMgr, id(State), Term),
            {stop, {shutdown, Term}, State#state{run_id=undefined}};
        no_reply ->
            {noreply, State}
    end.

-spec init({wrek_vert_t:t(), pid() | undefined, #{any() => wrek_vert_t:t()}, pid(), pid()}) ->
    {ok, state()}.

init({Vert, Runner, Data, EventMgr, Parent}) ->
    process_flag(trap_exit, true),

    Id = wrek_vert_t:id(Vert),
    Name = wrek_vert_t:name(Vert),
    Module = wrek_vert_t:module(Vert),
    Args = wrek_vert_t:args(Vert),

    case wrek_vert_t:timeout(Vert) of
        Ms when is_integer(Ms) ->
            {ok, _TRef} = timer:send_after(Ms, timeout);
        _ -> ok
    end,

    wrek_event:vert_start(EventMgr, Id, Name, Module, Args),

    Self = self(),
    Pid = case Runner of
              undefined ->
                  {ok, P} = wrek_vert_runner:start_link(#{stop_on_completion => true}),
                  P;
              _ ->
                  Runner
          end,
    RunId = wrek_vert_runner:send_request(Pid, Module, Args, Self),

    State = #state{
        child = Pid,
        data = Data,
        event_mgr = EventMgr,
        parent = Parent,
        vert = Vert,
        run_id = RunId
    },
    {ok, State}.


-spec terminate(_, state()) -> ok.

terminate(_Reason, _State) ->
    ok.


%% private

-spec add_waitpid({pid(), term()}, pid(), state()) -> state().

add_waitpid(From, Pid, State = #state{pids = Pids}) ->
    Pids2 = case Pids of
        #{Pid := {result, Result}} ->
            gen_server:reply(From, Result),
            Pids;
        #{Pid := Waits} ->
            Pids#{Pid => Waits ++ [From]}
    end,
    State#state{pids = Pids2}.


-spec dag_dir(pid()) -> file:filename_all().

dag_dir(Pid) ->
    gen_server:call(Pid, sandbox).


-spec handle_exec(Dir, string(), Env, fun(), state()) ->
    {reply, {ok, fun()} | {error, _}, state()}
    when Dir :: file:filename_all(), Env :: [{string(), string()}].

handle_exec(Dir, Cmd, Env, EventFun, State = #state{pids = Pids}) ->
    case wrek_exec:exec(Dir, Cmd, Env, EventFun) of
        {ok, Pid, _OsPid} ->
            Self = self(),
            Fun = fun() -> gen_server:call(Self, {waitpid, Pid}, infinity) end,
            State2 = State#state{pids = Pids#{Pid => []}},
            {reply, {ok, Fun}, State2};
        Err ->
            {reply, Err, State}
    end.


-spec id(state()) -> wrek:vert_id().

id(#state{vert = Vert}) ->
    wrek_vert_t:id(Vert).


-spec name(state()) -> any().

name(#state{vert = Vert}) ->
    wrek_vert_t:name(Vert).
