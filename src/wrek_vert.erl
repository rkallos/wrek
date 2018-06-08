-module(wrek_vert).

-export([exec/3,
         exec/4,
         get/3,
         get/4,
         get_all/1,
         make_sandbox/1,
         notify/2]).

-behaviour(gen_server).
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

-record(state, {
          child     = undefined :: pid() | undefined,
          data      = undefined :: map() | undefined,
          event_mgr = undefined :: pid() | undefined,
          id        = undefined :: {pos_integer(), pos_integer()} | undefined,
          name      = undefined :: any() | undefined,
          parent    = undefined :: pid() | undefined
         }).
-type state() :: #state{}.


-callback run(Args :: list(), Parent :: pid()) ->
    {ok, Result :: any()} | {error, Reason :: any()}.


-spec exec(pid(), file:filename_all(), string()) -> fun().

exec(Pid, Dir0, Cmd0) ->
    gen_server:call(Pid, {exec, Dir0, Cmd0, []}).


-spec exec(pid(), file:filename_all(), string(), list()) -> fun().

exec(Pid, Dir0, Cmd0, Env) ->
    gen_server:call(Pid, {exec, Dir0, Cmd0, Env}).


-spec get(pid(), any(), any()) -> any().

get(Pid, Who, Key) ->
    get(Pid, Who, Key, undefined).


-spec get(pid(), any(), any(), any()) -> any().

get(Pid, Who, Key, Default) ->
    gen_server:call(Pid, {get, Who, Key, Default}).


-spec get_all(pid()) -> map().

get_all(Pid) ->
    gen_server:call(Pid, get_all).


-spec make_sandbox(pid()) -> file:filename_all().

make_sandbox(Pid) ->
    gen_server:call(Pid, make_sandbox).


-spec notify(pid(), any()) -> ok.

notify(Pid, Msg) ->
    gen_server:cast(Pid, {notify, Msg}).


%% callbacks

-spec code_change(_, _, state()) -> {ok, state()}.

code_change(_Req, _From, State) ->
    {ok, State}.


-spec handle_call(_, _, state()) -> {reply, fun(), state()}.

handle_call({exec, Dir, Cmd, Env}, _From, State) ->
    #state{
       event_mgr = EvMgr,
       id = Id
      } = State,
    EventFun =
        fun(Msg) ->
            wrek_event:exec_output(EvMgr, Id, Msg)
        end,
    Result = fun() -> wrek_exec:exec(Dir, Cmd, Env, EventFun) end,
    {reply, Result, State};

handle_call({get, Who0, Key, Default}, _From, State) ->
    #state{name = Name, data = Data} = State,
    Who =
        case Who0 of
            me -> Name;
            Other -> Other
        end,

    case Data of
        #{Who := #{Key := Val}} ->
            {reply, Val, State};
        _ ->
            {reply, Default, State}
    end;

handle_call(get_all, _From, #state{data = Data}) ->
    {reply, ok, Data};

handle_call(make_sandbox, _From, State) ->
    #state{
       id = {_DagId, VertId},
       parent = Parent
      } = State,
    DagDir = dag_dir(Parent),
    VertStr = integer_to_list(VertId),
    Dir = wrek_utils:sandbox(DagDir, VertStr),
    wrek:put_data(Parent, #{dir => Dir}),
    {reply, Dir, State};

handle_call(_Req, _From, State) ->
    {reply, ok, State}.


-spec handle_cast(_, state()) -> {noreply, state()}.

handle_cast({notify, Msg}, State) ->
    #state{
       event_mgr = EvMgr,
       id = Id
      } = State,
    wrek_event:vert_msg(EvMgr, Id, Msg),
    {noreply, State};

handle_cast(_Req, State) ->
    {noreply, State}.


-spec handle_info(_, state()) ->
    {noreply, state()} | {stop, normal | {error, _}, state()}.

handle_info(timeout, State) ->
    #state{
        event_mgr = EvMgr,
        id = Id
    } = State,
    wrek_event:vert_done(EvMgr, Id, timeout),
    {stop, {shutdown, timeout}, State};

handle_info({'EXIT', _Pid, Term}, State) ->
    #state{
       event_mgr = EvMgr,
       id = Id
     } = State,
    wrek_event:vert_done(EvMgr, Id, Term),
    {stop, {shutdown, Term}, State};

handle_info(_Req, State) ->
    {noreply, State}.


-spec init({map(), pid(), {pos_integer(), pos_integer()}, any(), pid()}) ->
    {ok, state()}.

init({Data, EventMgr, Id, Name, Parent}) ->
    process_flag(trap_exit, true),
    #{Name := Map = #{module := Module, args := Args}} = Data,

    case Map of
        #{timeout := Ms} ->
            {ok, _TRef} = timer:send_after(Ms, timeout);
        _ -> ok
    end,

    wrek_event:vert_start(EventMgr, Id, Name, Module, Args),

    Self = self(),
    Pid = spawn_link(fun() ->
        Result = Module:run(Args, Self),
        exit(Result)
    end),

    State = #state{
        child = Pid,
        data = Data,
        event_mgr = EventMgr,
        id = Id,
        name = Name,
        parent = Parent
     },
    {ok, State}.


-spec terminate(_, state()) -> ok.

terminate(_Reason, _State) ->
    ok.


%% private

-spec dag_dir(pid()) -> file:filename_all().

dag_dir(Pid) ->
    gen_server:call(Pid, sandbox).
