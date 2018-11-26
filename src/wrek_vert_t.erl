-module(wrek_vert_t).

-export([
    new/0,
    cancel/1,
    fail/2,
    from_defn/1,
    has_succeeded/1,
    is_finished/1,
    succeed/2,
    % getters
    args/1,
    deps/1,
    dir/1,
    id/1,
    kv/1,
    module/1,
    name/1,
    reason/1,
    status/1,
    timeout/1,
    % setters
    set_args/2,
    set_deps/2,
    set_dir/2,
    set_id/2,
    set_kv/2,
    set_module/2,
    set_name/2,
    set_reason/2,
    set_status/2,
    set_timeout/2
]).

-type args_t() :: list().
-type deps_t() :: list().
-type dir_t() :: file:filename_all() | undefined.
-type id_t() :: wrek:vert_id() | undefined.
-type kv_t() :: map().
-type module_t() :: module() | undefined.
-type name_t() :: any().
-type reason_t() :: any().
-type status_t() :: failed | done | cancelled | undefined.
-type timeout_t() :: pos_integer() | undefined.

-record(t, {
    args    = []        :: args_t(),
    deps    = []        :: deps_t(),
    dir     = undefined :: dir_t(),
    id      = undefined :: id_t(),
    kv      = #{}       :: kv_t(),
    module  = undefined :: module_t(),
    name    = undefined :: name_t(),
    reason  = undefined :: reason_t(),
    status  = undefined :: status_t(),
    timeout = undefined :: timeout_t()
}).

-type t() :: #t{}.

-export_type([
    t/0
]).


-spec new() -> t().

new() ->
    #t{}.


-spec cancel(t()) -> t().

cancel(Vert = #t{}) ->
    set_status(Vert, cancelled).


-spec fail(t(), reason_t()) -> t().

fail(Vert = #t{}, Reason) ->
    Vert2 = set_reason(Vert, Reason),
    set_status(Vert2, failed).


-spec from_defn(map()) -> {ok, t()} | {error, any()}.

from_defn(Map0) when is_map(Map0) ->
    Res0 = #t{},

    MandatoryFields = [
        {module, fun set_module/2},
        {args, fun set_args/2},
        {deps, fun set_deps/2}
    ],

    OptionalFields = [
        {name, fun set_name/2},
        {timeout, fun set_timeout/2}
    ],

    case load_mandatory(Res0, Map0, MandatoryFields) of
        {error, _} = Err ->
            Err;
        {ok, {Res1, Map1}} ->
            {ok, {Res2, Map2}} = load_optional(Res1, Map1, OptionalFields),
            Res3 = set_kv(Res2, Map2),
            {ok, Res3}
    end;

from_defn(_) ->
    {error, not_a_map}.


-spec has_succeeded(t()) -> boolean().

has_succeeded(Vert = #t{}) ->
    case status(Vert) of
        done ->
            true;
        _ ->
            false
    end.


-spec is_finished(t()) -> boolean().

is_finished(Vert = #t{}) ->
    case status(Vert) of
        done ->
            true;
        failed ->
            true;
        cancelled ->
            true;
        _ ->
            false
    end.


-spec succeed(t(), map()) -> t().

succeed(Vert = #t{}, Result) ->
    Vert2 = set_kv(Vert, Result),
    set_status(Vert2, done).


-spec args(t()) -> args_t().

args(#t{args = Args}) ->
    Args.


-spec deps(t()) -> deps_t().

deps(#t{deps = Deps}) ->
    Deps.


-spec dir(t()) -> dir_t().

dir(#t{dir = Dir}) ->
    Dir.


-spec id(t()) -> id_t().

id(#t{id = Id}) ->
    Id.


-spec kv(t()) -> kv_t().

kv(#t{kv = Kv}) ->
    Kv.


-spec module(t()) -> module_t().

module(#t{module = Module}) ->
    Module.


-spec name(t()) -> name_t().

name(#t{name = Name}) ->
    Name.


-spec reason(t()) -> reason_t().

reason(#t{reason = Reason}) ->
    Reason.


-spec status(t()) -> status_t().

status(#t{status = Status}) ->
    Status.


-spec timeout(t()) -> timeout_t().

timeout(#t{timeout = Timeout}) ->
    Timeout.


-spec set_args(t(), args_t()) -> t().

set_args(T = #t{}, Args) ->
    T#t{args = Args}.


-spec set_deps(t(), deps_t()) -> t().

set_deps(T = #t{}, Deps) ->
    T#t{deps = Deps}.


-spec set_dir(t(), dir_t()) -> t().

set_dir(T = #t{}, Dir) ->
    T#t{dir = Dir}.


-spec set_id(t(), id_t()) -> t().

set_id(T = #t{}, Id) ->
    T#t{id = Id}.


-spec set_kv(t(), kv_t()) -> t().

set_kv(T = #t{}, Kv) ->
    T#t{kv = Kv}.


-spec set_module(t(), module_t()) -> t().

set_module(T = #t{}, Module) ->
    T#t{module = Module}.


-spec set_name(t(), name_t()) -> t().

set_name(T = #t{}, Name) ->
    T#t{name = Name}.


-spec set_reason(t(), reason_t()) -> t().

set_reason(T = #t{}, Reason) ->
    T#t{reason = Reason}.


-spec set_status(t(), status_t()) -> t().

set_status(T = #t{}, Status) ->
    T#t{status = Status}.


-spec set_timeout(t(), timeout_t()) -> t().

set_timeout(T = #t{}, Timeout) ->
    T#t{timeout = Timeout}.


% private

-type setter_t() :: fun((t(), any()) -> t()).

-spec load_mandatory(t(), map(), [{atom(), setter_t()}]) ->
    {ok, {t(), map()}} | {error, any()}.

load_mandatory(Vert, Map, FieldSetterPairs) ->
    load(Vert, Map, FieldSetterPairs, error).


-spec load_optional(t(), map(), [{atom(), setter_t()}]) ->
    {ok, {t(), map()}}.

load_optional(Vert, Map, FieldSetterPairs) ->
    load(Vert, Map, FieldSetterPairs, continue).


-spec load(t(), map(), [{atom(), setter_t()}], error | continue) ->
    {ok, {t(), map()}} | {error, any()}.

load(Vert = #t{}, Map, [], _FailMode) ->
    {ok, {Vert, Map}};

load(Vert = #t{}, Map, [{FieldName, Setter} | Rest], FailMode) ->
    case {Map, FailMode} of
        {#{FieldName := FieldVal}, _} ->
            Vert2 = Setter(Vert, FieldVal),
            Map2 = maps:remove(FieldName, Map),
            load(Vert2, Map2, Rest, FailMode);
        {_, error} ->
            {error, {missing_field, FieldName}};
        {_, continue} ->
            load(Vert, Map, Rest, FailMode)
    end.
