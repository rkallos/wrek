-module(wrek).

-export([put_data/2,
         start/1,
         start/2]).

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

-type option() :: {event_manager, pid()} | {failure_mode, partial | total}.

-export_type([
    dag_id/0,
    dag_map/0,
    option/0,
    vert_defn/0,
    vert_id/0
]).

-record(state, {
    children     = #{}       :: #{pid() => any()},
    dag          = undefined :: digraph:graph() | undefined,
    event_mgr    = undefined :: pid() | undefined,
    failure_mode = total     :: partial | total,
    id           = ?id()     :: dag_id(),
    sandbox      = undefined :: file:filename_all() | undefined
}).

-type state() :: #state{}.


-spec put_data(pid(), any()) -> ok.

put_data(Pid, Data) ->
    gen_server:call(Pid, {put_data, Data}).


-spec start(dag_map()) -> supervisor:startchild_ret().

start(Defns) ->
    start(Defns, []).


-spec start(dag_map(), [option()]) -> supervisor:startchild_ret().

start(Defns, Opts) ->
    Id = ?id(),
    ChildSpec = #{
      id => Id,
      start => {gen_server, start_link, [?MODULE, {Id, Defns, Opts}, []]},
      restart => temporary,
      type => worker
     },
    supervisor:start_child(wrek_sup, ChildSpec).


%% callbacks

-spec code_change(_, _, state()) -> {ok, state()}.

code_change(_Req, _From, State) ->
    {ok, State}.


-spec handle_call(_, _, state()) -> {reply, _, state()} | {stop, _, state()}.

handle_call({put_data, Data}, {From, _}, State) ->
    #state{
       children = #{From := Name},
       dag = Dag
      } = State,
    {Name, OldLabel} = digraph:vertex(Dag, Name),
    Label = maps:merge(OldLabel, Data),
    digraph:add_vertex(Dag, Name, Label),
    {reply, ok, State};

handle_call(sandbox, _From, State) ->
    {reply, State#state.sandbox, State};

handle_call(_Req, _From, State) ->
    {reply, ok, State}.


-spec handle_cast(_, state()) -> {noreply, state()}.

handle_cast(_Req, State) ->
    {noreply, State}.


-spec handle_info(_, state()) -> {noreply, state()}.

handle_info({'EXIT', Pid, {shutdown, {ok, Data}}}, State0) ->
    #state{
       children = #{Pid := Name},
       dag = Dag
      } = State0,

    {Name, OldLabel} = digraph:vertex(Dag, Name),
    Label = maps:merge(OldLabel, Data),
    digraph:add_vertex(Dag, Name, Label),

    State = mark_vert_done(State0, Pid),
    start_verts_or_exit(State);

handle_info({'EXIT', Pid, {shutdown, Reason}}, State) ->
    #state{
       children = Children,
       event_mgr = EvMgr,
       failure_mode = FailMode,
       id = Id
      } = State,
    #{Pid := Name} = Children,
    wrek_event:wrek_error(EvMgr, Id, {vert, Name}),
    case FailMode of
        total ->
            {stop, {error, Reason}, State};
        partial ->
            State2 = mark_vert_failed(State, Pid),
            propagate_partial_failure(State2, Name),
            start_verts_or_exit(State)
    end;

handle_info(_Req, State) ->
    {noreply, State}.


-spec init({dag_id(), dag_map(), [option()]}) -> {ok, state()} | {stop, _}.

init({Id, DagMap, Opts}) ->
    process_flag(trap_exit, true),

    {ok, Dag} = wrek_utils:from_verts(DagMap),

    EvMgr = proplists:get_value(event_manager, Opts, undefined),
    FailMode = proplists:get_value(failure_mode, Opts, total),

    Sandbox = make_dag_sandbox(Id),

    State = #state{
        dag = Dag,
        event_mgr = EvMgr,
        failure_mode = FailMode,
        id = Id,
        sandbox = Sandbox
     },

    wrek_event:wrek_start(EvMgr, Id, DagMap),

    {ok, State2} = start_verts(State),

    case maps:size(State2#state.children) of
        0 ->
            wrek_event:wrek_error(EvMgr, Id, {unable_to_start, DagMap}),
            {stop, {unable_to_start, DagMap}};
        _ ->
            {ok, State2}
    end.


-spec terminate(_, state()) -> ok.

terminate(_Reason, _State) ->
    ok.

%% private

-spec has_done_key({digraph:vertex(), digraph:label()}) -> boolean().

has_done_key({_, #{done := _}}) -> true;
has_done_key(_) -> false.


-spec is_dag_done(state()) -> boolean().

is_dag_done(#state{dag = Dag}) ->
    Verts = [digraph:vertex(Dag, V) || V <- digraph:vertices(Dag)],
    lists:all(fun has_done_key/1, Verts).


-spec is_vert_ready(digraph:graph(), digraph:vertex()) -> boolean().

is_vert_ready(Dag, Vertex) ->
    Deps = [digraph:vertex(Dag, V) || V <- wrek_utils:in_vertices(Dag, Vertex)],
    lists:all(fun
        ({_, #{done := done}}) -> true;
        (_) -> false
    end, Deps).


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

make_vert_data(#state{dag = Dag}, Name) ->
    Reaching =
        [digraph:vertex(Dag, V) || V <- digraph_utils:reaching([Name], Dag)],
    maps:from_list(Reaching).


-spec mark_vert(state(), pid(), done | failed) -> state().

mark_vert(State = #state{children = Children, dag = Dag}, Pid, Status) ->
    #{Pid := Name} = Children,
    Children2 = maps:remove(Pid, Children),
    {Name, Label} = digraph:vertex(Dag, Name),
    Label2 = Label#{done => Status},
    digraph:add_vertex(Dag, Name, Label2),
    State#state{children = Children2}.


-spec mark_vert_failed(state(), pid()) -> state().

mark_vert_failed(State, Pid) -> mark_vert(State, Pid, failed).


-spec mark_vert_done(state(), pid()) -> state().

mark_vert_done(State, Pid) -> mark_vert(State, Pid, done).


-spec propagate_partial_failure(state(), digraph:vertex()) -> ok.

propagate_partial_failure(State, Name) ->
    #state{
      dag = Dag,
      event_mgr = EvMgr,
      id = Id
    } = State,
    Reachable = digraph_utils:reachable_neighbours([Name], Dag),
    Fun = fun(Vert) ->
        {Vert, Label} = digraph:vertex(Dag, Vert),
        digraph:add_vertex(Dag, Vert, Label#{done => cancelled}),
        wrek_event:wrek_msg(EvMgr, Id, {vert_cancelled, Vert}),
        ok
    end,
    lists:foreach(Fun, Reachable).


-spec ready_verts(state()) -> [digraph:vertex()].

ready_verts(#state{dag = Dag, children = Children}) ->
    [Vertex || Vertex <- digraph:vertices(Dag),
        is_vert_ready(Dag, Vertex),
        not has_done_key(digraph:vertex(Dag, Vertex)),
        not lists:member(Vertex, maps:values(Children))].


-spec start_verts(state()) -> {ok, state()} | {error, _}.

start_verts(State = #state{children = Children}) ->
    ReadyVerts = ready_verts(State),
    Children2 =
        lists:foldl(
          fun(Name, Acc) ->
              {ok, Pid} = start_vert(State, Name),
              Acc#{Pid => Name}
          end, Children, ReadyVerts),
    State2 = State#state{children = Children2},
    {ok, State2}.


-spec start_vert(state(), digraph:vertex()) -> {ok, pid()}.

start_vert(State = #state{dag = Dag, id = DagId}, Name) ->
    #state{
       dag = Dag,
       event_mgr = EventMgr,
       id = DagId
     } = State,
    VertId = {DagId, ?id()},
    {Name, Label0} = digraph:vertex(Dag, Name),
    Label = Label0#{id => VertId},
    digraph:add_vertex(Dag, Name, Label),

    wrek_event:wrek_msg(EventMgr, DagId, {starting_vert, VertId}),

    Data = make_vert_data(State, Name),
    gen_server:start_link(wrek_vert, {Data, EventMgr, VertId, Name, self()}, []).


-spec start_verts_or_exit(state()) -> {noreply, state()} | {stop, normal, state()}.

start_verts_or_exit(State) ->
    case is_dag_done(State) of
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
