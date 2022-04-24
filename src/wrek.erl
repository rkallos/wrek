-module(wrek).

-export([put_sandbox/2,
         start/1,
         start/2,
        start_link/1,
        start_link/2]).

-behaviour(gen_server).
-export([
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2
]).

-type dag_id() :: pos_integer().
-type vert_id() :: {pos_integer(), pos_integer()}.

-define(id(), erlang:unique_integer([positive, monotonic])).

-type vert_defn() :: #{
    module := module(),
    args   := list(),
    deps   := list()
}.

-type dag_map() :: #{any() := vert_defn()} | [{any(), vert_defn()}].

-type option() :: {event_manager, pid()} | {failure_mode, partial | total} |
    {global_timeout, pos_integer() | undefined}.

-export_type([
    dag_id/0,
    dag_map/0,
    option/0,
    vert_defn/0,
    vert_id/0
]).

-record(state, {
    event_mgr    = undefined :: pid() | undefined,
    failure_mode = total     :: partial | total,
    id           = ?id()     :: dag_id(),
    sandbox      = undefined :: file:filename_all() | undefined,
    wrek         = undefined :: wrek_t:t() | undefined,
    runner_sup   = undefined :: pid() | undefined
}).

-type state() :: #state{}.


-spec put_sandbox(pid(), file:filename_all()) -> {ok, wrek_vert_t:t()}.

put_sandbox(Pid, Dir) ->
    gen_server:call(Pid, {put_sandbox, Dir}).


-spec start(dag_map()) -> supervisor:startchild_ret().

start(Defns) ->
    start(Defns, []).


-spec start(dag_map(), [option()]) -> supervisor:startchild_ret().

start(Defns, Opts) ->
    Id = ?id(),
    ChildSpec = #{
      id => Id,
      start => {gen_server, start_link, [?MODULE, {Id, Defns, undefined, Opts}, []]},
      restart => temporary,
      type => worker
     },
    supervisor:start_child(wrek_sup, ChildSpec).

-spec start_link(dag_map()) -> {ok, {pid(), pid()}}.
start_link(Defns) ->
    start_link(Defns, []).

-spec start_link(dag_map(), [option()]) -> {ok, {pid(), pid()}}.
start_link(Defns, Opts) ->
    Id = ?id(),
    Names = maps:keys(Defns),
    RunnerSup = case proplists:get_value(runner_sup, Opts) of
                    undefined ->
                        {ok, RS} = wrek_runner_sup:start_link(Names),
                        RS;
                    RS when is_pid(RS) ->
                        RS
                end,
    ChildSpec = #{
      id => Id,
      start => {gen_server, start_link, [?MODULE, {Id, Defns, RunnerSup, Opts}, []]},
      restart => temporary,
      type => worker
     },
    {ok, WrekPid} = supervisor:start_child(wrek_sup, ChildSpec),
    {ok, {WrekPid, RunnerSup}}.

%% callbacks

-spec code_change(_, _, state()) -> {ok, state()}.

code_change(_Req, _From, State) ->
    {ok, State}.


-spec handle_call(_, _, state()) -> {reply, _, state()} | {stop, _, state()}.

handle_call({put_sandbox, Dir}, {From, _}, State = #state{wrek = Wrek}) ->
    {ok, {Name, Vert}} = wrek_t:child(Wrek, From),
    Vert2 = wrek_vert_t:set_dir(Vert, Dir),
    Wrek2 = wrek_t:add_vertex(Wrek, Name, Vert2),
    {reply, {ok, Vert2}, State#state{wrek = Wrek2}};

handle_call(sandbox, _From, State) ->
    {reply, State#state.sandbox, State};

handle_call(_Req, _From, State) ->
    {reply, ok, State}.


-spec handle_cast(_, state()) -> {noreply, state()}.

handle_cast(_Req, State) ->
    {noreply, State}.


-spec handle_info(_, state()) -> {noreply, state()}.

handle_info(timeout, #state{
       event_mgr = EvMgr,
       id = Id
      } = State) ->
    wrek_event:wrek_error(EvMgr, Id, timeout),
    {stop, timeout, State};

handle_info({'EXIT', Pid, {shutdown, {ok, Data}}}, State) ->
    #state{wrek = Wrek} = State,

    Wrek2 = wrek_t:child_succeeded(Wrek, Pid, Data),
    start_verts_or_exit(State#state{wrek = Wrek2});

handle_info({'EXIT', Pid, {shutdown, Reason}}, State) ->
    #state{
       event_mgr = EvMgr,
       failure_mode = FailMode,
       id = Id,
       wrek = Wrek
      } = State,
    {ok, {Name, _Vert}} = wrek_t:child(Wrek, Pid),
    wrek_event:wrek_error(EvMgr, Id, {vert, Name}),
    case FailMode of
        total ->
            {stop, {error, Reason}, State};
        partial ->
            Wrek2 = wrek_t:child_failed(Wrek, Pid, Reason),
            State2 = State#state{wrek = Wrek2},
            State3 = propagate_partial_failure(State2, Name),
            start_verts_or_exit(State3)
    end;

handle_info(_Req, State) ->
    {noreply, State}.


-spec init({dag_id(), dag_map(), pid() | undefined, [option()]}) -> {ok, state()} | {stop, _}.

init({Id, DagMap, RunnerSup, Opts}) ->
    process_flag(trap_exit, true),

    {ok, Wrek} = wrek_t:from_verts(DagMap),

    EvMgr = proplists:get_value(event_manager, Opts, undefined),
    FailMode = proplists:get_value(failure_mode, Opts, total),

    case proplists:get_value(global_timeout, Opts, undefined) of
        T when is_integer(T) ->
                {ok, _TRef} = timer:send_after(T, timeout);
        undefined -> ok
    end,

    Sandbox = make_dag_sandbox(Id),

    State = #state{
        event_mgr = EvMgr,
        failure_mode = FailMode,
        id = Id,
        sandbox = Sandbox,
        wrek = Wrek,
        runner_sup = RunnerSup
     },

    wrek_event:wrek_start(EvMgr, Id, DagMap),

    {ok, State2} = start_verts(State),

    case wrek_t:is_active(State2#state.wrek) of
        false ->
            Reason = {unable_to_start, DagMap},
            wrek_event:wrek_error(EvMgr, Id, Reason),
            {stop, Reason};
        true ->
            {ok, State2}
    end.


-spec terminate(_, state()) -> ok.

terminate(_Reason, _State) ->
    ok.

%% private

-define(DIRNAME,
        lists:flatten(
          io_lib:format(
            "~B-~2..0B-~2..0B-~2..0B:~2..0B:~2..0B-~b",
            [Year, Month, Day, Hour, Min, Sec, Id]
           ))).

-spec make_dag_sandbox(dag_id()) -> file:filename_all().

make_dag_sandbox(Id) ->
    BaseDir = application:get_env(wrek, sandbox_dir, "/tmp"),
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    wrek_utils:sandbox(BaseDir, ?DIRNAME).


-spec make_vert_data(state(), _) -> any().

make_vert_data(#state{wrek = Wrek}, Name) ->
    Dependencies =
        [wrek_t:vertex(Wrek, N) || N <- wrek_t:dependencies(Wrek, Name)],
    maps:from_list([wrek_t:vertex(Wrek, Name) | Dependencies]).


-spec propagate_partial_failure(state(), digraph:vertex()) -> state().

propagate_partial_failure(State, Name) ->
    #state{
      event_mgr = EvMgr,
      id = Id,
      wrek = Wrek
    } = State,
    Wrek2 = lists:foldl(fun(VertName, Acc) ->
        wrek_event:wrek_msg(EvMgr, Id, {vert_cancelled, VertName}),
        wrek_t:cancel_vertex(Acc, VertName)
    end, Wrek, wrek_t:dependants(Wrek, Name)),
    State#state{wrek = Wrek2}.


-spec start_verts(state()) -> {ok, state()} | {error, _}.

start_verts(State = #state{wrek = Wrek}) ->
    ReadyVerts = wrek_t:ready_verts(Wrek),
    Wrek2 = lists:foldl(fun(Name, Acc) ->
        {ok, Pid} = start_vert(State, Name),
        wrek_t:child_started(Acc, Name, Pid)
    end, Wrek, ReadyVerts),
    State2 = State#state{wrek = Wrek2},
    {ok, State2}.


-spec start_vert(state(), digraph:vertex()) -> {ok, pid()}.

start_vert(State, Name) ->
    #state{
        event_mgr = EventMgr,
        id = DagId,
        wrek = Wrek,
        runner_sup = RunnerSup
    } = State,
    VertId = {DagId, ?id()},
    Wrek2 = wrek_t:set_vert_id(Wrek, Name, VertId),
    {Name, Vert} = wrek_t:vertex(Wrek2, Name),

    wrek_event:wrek_msg(EventMgr, DagId, {starting_vert, VertId}),

    Runner = get_runner(Name, RunnerSup),
    Data = make_vert_data(State, Name),
    Args = {Vert, Runner, Data, EventMgr, self()},
    gen_server:start_link(wrek_vert, Args, []).

get_runner(_Name, undefined) -> undefined;
get_runner(Name, RunnerSup) ->
    Children = supervisor:which_children(RunnerSup),
    case lists:keyfind(Name, 1, Children) of
        false ->
            undefined;
        {Name, Runner, _, _} ->
            Runner
    end.

-spec start_verts_or_exit(state()) -> {noreply, state()} | {stop, normal, state()}.

start_verts_or_exit(State = #state{wrek = Wrek}) ->
    case wrek_t:is_finished(Wrek) of
        true ->
            #state{
                event_mgr = EvMgr,
                id = Id
            } = State,
            wrek_event:wrek_done(EvMgr, Id),
            {stop, normal, State};
        false ->
            {ok, State2} = start_verts(State),
            {noreply, State2}
    end.
