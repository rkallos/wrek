-module(wrek_t).

-export([
    add_vertex/3,
    cancel_vertex/2,
    child/2,
    child_failed/3,
    child_started/3,
    child_succeeded/3,
    dependants/2,
    dependencies/2,
    edge/2,
    edges/1,
    is_active/1,
    is_finished/1,
    format/1,
    from_verts/1,
    ready_verts/1,
    remove_child/2,
    set_vert_id/3,
    vertex/2,
    vertices/1
]).

-type dag_t() :: digraph:graph().
-type defn_t() :: wrek:vert_defn().
-type edge_t() :: digraph:edge().
-type label_t() :: digraph:label().
-type name_t() :: any().
-type vert_t() :: digraph:vertex().

-define(T, ?MODULE).

-record(?T, {
    children = #{}   :: #{pid() => vert_t()},
    dag      = new() :: dag_t()
}).

-type t() :: #?T{}.

-export_type([
    t/0
]).


-spec add_vertex(t(), vert_t(), wrek_vert_t:t()) -> t().

add_vertex(T = #?T{dag = Dag}, Name, Vert) ->
    digraph:add_vertex(Dag, Name, Vert),
    T.


-spec cancel_vertex(t(), vert_t()) -> t().

cancel_vertex(T = #?T{}, Name) ->
    {Name, Vert} = vertex(T, Name),
    Vert2 = wrek_vert_t:cancel(Vert),
    add_vertex(T, Name, Vert2).


-spec child(t(), pid()) -> {ok, {vert_t(), wrek_vert_t:t()}} | false.

child(T = #?T{children = Children}, Pid) ->
    case Children of
        #{Pid := Name} ->
            case vertex(T, Name) of
                false ->
                    false;
                Vert ->
                    {ok, Vert}
            end;
        _ ->
            false
    end.


-spec child_failed(t(), pid(), any()) -> t().

child_failed(T = #?T{}, Pid, Reason) ->
    {ok, {Name, Vert}} = child(T, Pid),
    Vert2 = wrek_vert_t:fail(Vert, Reason),
    T2 = add_vertex(T, Name, Vert2),
    T3 = remove_child(T2, Pid),
    T3.


-spec child_started(t(), vert_t(), pid()) -> t().

child_started(T = #?T{children = Children}, Name, Pid) ->
    Children2 = Children#{Pid => Name},
    T#?T{children = Children2}.


-spec child_succeeded(t(), pid(), map()) -> t().

child_succeeded(T = #?T{}, Pid, Result) ->
    {ok, {Name, Vert}} = child(T, Pid),
    Vert2 = wrek_vert_t:succeed(Vert, Result),
    T2 = add_vertex(T, Name, Vert2),
    T3 = remove_child(T2, Pid),
    T3.


-spec dependants(t(), vert_t()) -> [vert_t()].

dependants(#?T{dag = Dag}, Name) ->
    digraph_utils:reachable_neighbours([Name], Dag).


-spec dependencies(t(), vert_t()) -> [vert_t()].

dependencies(#?T{dag = Dag}, Name) ->
    digraph_utils:reaching_neighbours([Name], Dag).


-spec edge(t(), vert_t()) -> {edge_t(), vert_t(), vert_t(), label_t()} | false.

edge(#?T{dag = Dag}, Edge) ->
    digraph:edge(Dag, Edge).


-spec edges(t()) -> [edge_t()].

edges(#?T{dag = Dag}) ->
    digraph:edges(Dag).


-spec format(t()) -> string().

format(T = #?T{}) ->
    Verts = [vertex(T, V) || V <- vertices(T)],
    Edges = [edge(T, E) || E <- edges(T)],
    io_lib:format("~p~n", [{Verts, Edges}]).


-spec from_verts(map() | [{name_t(), wrek:vert_map()}]) ->
    {ok, t()} | {error, any()}.

from_verts(Verts) when is_map(Verts) ->
    from_verts(maps:to_list(Verts));

from_verts(Proplist) ->
    case make_vertices(Proplist) of
        {ok, Verts} ->
            T = #?T{},
            ok = add_vertices(T, Verts),
            case add_dependencies(T, Verts) of
                ok ->
                    {ok, T};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.


-spec is_active(t()) -> boolean().

is_active(#?T{children = Children}) ->
    maps:size(Children) > 0.


-spec is_finished(t()) -> boolean().

is_finished(T) ->
    lists:all(fun(VertName) ->
        {VertName, Vert} = vertex(T, VertName),
        wrek_vert_t:is_finished(Vert)
    end, vertices(T)).


-spec ready_verts(t()) -> [vert_t()].

ready_verts(T = #?T{dag = Dag, children = Children}) ->
    lists:filter(fun(Name) ->
        {Name, Vert} = vertex(T, Name),
        is_vert_ready(T, Name) andalso
        not wrek_vert_t:is_finished(Vert) andalso
        not lists:member(Name, maps:values(Children))
    end, digraph:vertices(Dag)).


-spec remove_child(t(), pid()) -> t().

remove_child(T = #?T{children = Children}, Pid) ->
    T#?T{children = maps:remove(Pid, Children)}.


-spec set_vert_id(t(), vert_t(), wrek:vert_id()) -> t().

set_vert_id(T = #?T{}, Name, Id) ->
    {Name, Vert} = vertex(T, Name),
    Vert2 = wrek_vert_t:set_id(Vert, Id),
    add_vertex(T, Name, Vert2),
    T.


-spec vertex(t(), vert_t()) -> {vert_t(), label_t()} | false.

vertex(#?T{dag = Dag}, Vert) ->
    digraph:vertex(Dag, Vert).


-spec vertices(t()) -> [vert_t()].

vertices(#?T{dag = Dag}) ->
    digraph:vertices(Dag).


%% private

-spec add_dependencies(t(), [wrek_vert_t:t()]) ->
    ok | {error, any()}.

add_dependencies(_T = #?T{}, []) ->
    ok;

add_dependencies(T = #?T{}, [Vert | Rest]) ->
    Name = wrek_vert_t:name(Vert),
    Deps = wrek_vert_t:deps(Vert),
    case add_edges(T, Name, Deps) of
        ok ->
            add_dependencies(T, Rest);
        {error, _} = Err ->
            Err
    end.


-spec add_edges(t(), name_t(), [name_t()]) ->
    ok | {error, any()}.

add_edges(_T, _To, []) ->
    ok;

add_edges(T = #?T{dag = Dag}, To, [From | Rest]) ->
    case digraph:add_edge(Dag, From, To) of
        {error, _} = Err ->
            Err;
        _ ->
            add_edges(T, To, Rest)
    end.


-spec add_vertices(t(), [wrek_vert_t:t()]) -> ok.

add_vertices(#?T{dag = Dag}, Defns) ->
    lists:foreach(fun(Vert) ->
        Name = wrek_vert_t:name(Vert),
        digraph:add_vertex(Dag, Name, Vert)
    end, Defns).


-spec is_vert_ready(t(), vert_t()) -> boolean().

is_vert_ready(T, Name) ->
    case vertex(T, Name) of
        false ->
            false;
        {Name, Vert} ->
            lists:all(fun(Dep) ->
                {_, DepVert} = vertex(T, Dep),
                wrek_vert_t:has_succeeded(DepVert)
            end, wrek_vert_t:deps(Vert))
    end.


-spec make_vertices([{name_t(), defn_t()}]) ->
    {ok, [wrek_vert_t:t()]} | {error, any()}.

make_vertices(Verts) ->
    make_vertices2(Verts, []).


-spec make_vertices2([{name_t(), defn_t()}], [wrek_vert_t:t()]) ->
    {ok, [wrek_vert_t:t()]} | {error, any()}.

make_vertices2([], Acc) ->
    {ok, Acc};

make_vertices2([{Name, Defn} | Rest], Acc) ->
    case wrek_vert_t:from_defn(Defn) of
        {ok, Vert} ->
            Vert2 = wrek_vert_t:set_name(Vert, Name),
            make_vertices2(Rest, [Vert2 | Acc]);
        {error, _} = Err ->
            Err
    end.


-spec new() -> dag_t().

new() ->
    digraph:new([acyclic, protected]).
