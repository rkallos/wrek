-module(wrek_utils).

-export([format_dag/1,
         from_verts/1,
         in_vertices/2,
         out_vertices/2,
         rm/1,
         rmdir/1,
         sandbox/2]).


-spec format_dag(digraph:graph()) -> string().

format_dag(Dag) ->
    Verts = [digraph:vertex(Dag, V) || V <- digraph:vertices(Dag)],
    Edges = [digraph:edge(Dag, E) || E <- digraph:edges(Dag)],
    io_lib:format("~p~n", [{Verts, Edges}]).


-spec from_verts(wrek:dag_map()) -> {ok, digraph:graph()} | {error, any()}.

from_verts(Verts) when is_map(Verts) ->
    from_verts(maps:to_list(Verts));

from_verts(Verts) ->
    Dag = digraph:new([acyclic, protected]),
    add_vertices(Dag, Verts),
    case add_dependencies(Dag, Verts) of
        ok -> {ok, Dag};
        {error, Reason} -> {error, Reason}
    end.


-spec in_vertices(digraph:graph(), digraph:vertex()) -> [digraph:vertex()].

in_vertices(Dag, Name) ->
    InEdges = [digraph:edge(Dag, E) || E <- digraph:in_edges(Dag, Name)],
    [V || {_, V, _, _} <- InEdges].


-spec out_vertices(digraph:graph(), digraph:vertex()) -> [digraph:vertex()].

out_vertices(Dag, Name) ->
    OutEdges = [digraph:edge(Dag, E) || E <- digraph:out_edges(Dag, Name)],
    [V || {_, _, V, _} <- OutEdges].


-spec rm(file:filename_all()) -> ok | {error, atom()}.

rm(Path) ->
    case filelib:is_dir(Path) of
        true -> rmdir(Path);
        false ->
            case filelib:is_file(Path) of
                true -> file:delete(Path);
                false -> ok
            end
    end.


-spec rmdir(file:filename_all()) -> ok | {error, atom()}.

rmdir(Dir) ->
    case file:list_dir(Dir) of
        {error, enoent} -> ok;
        {ok, Files} ->
            lists:foreach(fun(F) -> rm(filename:join(Dir, F)) end, Files),
            ok = file:del_dir(Dir)
    end.


-spec sandbox(file:filename_all(), string()) -> file:filename_all().

sandbox(BaseDir, Name) ->
    Dir = filename:join([BaseDir, Name]),
    ok = rm(Dir),
    ok = filelib:ensure_dir(Dir),
    ok = file:make_dir(Dir),
    Dir.


% private

-spec add_dependencies(digraph:graph(), [{atom(), wrek:vert_defn()}]) ->
    ok | {error, any()}.

add_dependencies(_Dag, []) ->
    ok;

add_dependencies(Dag, [{Name, #{deps := Deps}} | Vs]) ->
    case add_edges(Dag, Name, Deps) of
        ok -> add_dependencies(Dag, Vs);
        {error, Reason} -> {error, Reason}
    end.


-spec add_edges(digraph:graph(), digraph:vertex(), [digraph:vertex()]) ->
    ok | {error, any()}.

add_edges(_Dag, _To, []) ->
    ok;

add_edges(Dag, To, [From | Froms]) ->
    case digraph:add_edge(Dag, From, To) of
        {error, Reason} -> {error, Reason};
        _ -> add_edges(Dag, To, Froms)
    end.


-spec add_vertices(digraph:graph(), [{any(), wrek:vert_defn()}]) -> ok.

add_vertices(Dag, Verts) ->
    lists:foreach(
      fun({Name, Defn}) -> digraph:add_vertex(Dag, Name, Defn) end,
      Verts
     ).
