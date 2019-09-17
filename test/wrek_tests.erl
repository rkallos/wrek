-module(wrek_tests).
-include_lib("eunit/include/eunit.hrl").
-include("wrek_event.hrl").

from_verts_ok_test() ->
    Verts = #{
      foo => ok_v([]),
      bar => ok_v([foo]),
      baz => ok_v([bar])
     },
    {ok, Wrek} = wrek_t:from_verts(Verts),

    Dag = element(3, Wrek),

    ?assertEqual([foo, bar, baz],
                 digraph_utils:topsort(Dag)).

from_verts_cycle_test() ->
    Cycle = #{
      foo => ok_v([baz]),
      bar => ok_v([foo]),
      baz => ok_v([bar])
     },
    ?assertMatch({error, {bad_edge, _}},
                 wrek_t:from_verts(Cycle)).

from_verts_missing_dep_test() ->
    Verts = #{bar => ok_v([foo])},

    ?assertMatch({error, {bad_vertex, _}},
                 wrek_t:from_verts(Verts)).

ok_test() ->
    error_logger:tty(false),
    exec:start(),
    application:start(wrek),

    VertMap = #{
      one => ok_v([]),
      two => ok_v([one]),
      two_and_a_half => ok_v([one]),
      three => ok_v([two])
     },

    {ok, Pid} = wrek:start(VertMap),

    MonitorRef = erlang:monitor(process, Pid),

    Atom = receive
        {'DOWN', MonitorRef, process, Pid, A} -> A
    end,

    ?assertEqual(normal, Atom).

not_ok_test() ->
    error_logger:tty(false),
    exec:start(),
    application:start(wrek),

    VertMap = #{
      one => ok_v([]),
      two => ok_v([one]),
      two_and_a_half => ok_v([one]),
      three => bad_v([two])
     },

    {ok, Pid} = wrek:start(VertMap),

    MonitorRef = erlang:monitor(process, Pid),

    Atom = receive
        {'DOWN', MonitorRef, process, Pid, A} -> A
    end,

    ?assertMatch({error, _}, Atom).

event_ok_test() ->
    exec:start(),
    application:start(wrek),

    {ok, EvMgr} = gen_event:start_link({local, wrek_test_manager}),
    gen_event:add_handler(EvMgr, wrek_test_handler, [total, self()]),

    VertMap = #{
      one => ok_v([]),
      two => ok_v([one]),
      two_and_a_half => ok_v([one]),
      three => ok_v([two])
     },

    {ok, _Pid} = wrek:start(VertMap, [{event_manager, EvMgr}]),

    Events = receive
        #{evts := Evts} -> Evts
    end,

    gen_event:stop(EvMgr),

    WrekStarts =
        [E || E = #wrek_event{type = {wrek, start}} <- Events],
    StartingVerts =
        [E || E = #wrek_event{msg = {starting_vert, _}} <- Events],
    VertStarts =
        [E || E = #wrek_event{type = {vert, start}} <- Events],
    VertDones =
        [E || E = #wrek_event{type = {vert, done}} <- Events],
    WrekDones =
        [E || E = #wrek_event{type = {wrek, done}} <- Events],

    ?assertEqual(1, length(WrekStarts)),
    ?assertEqual(4, length(StartingVerts)),
    ?assertEqual(4, length(VertStarts)),
    ?assertEqual(4, length(VertDones)),
    ?assertEqual(1, length(WrekDones)).

event_bad_test() ->
    exec:start(),
    application:start(wrek),

    {ok, EvMgr} = gen_event:start_link({local, wrek_test_manager}),
    gen_event:add_handler(EvMgr, wrek_test_handler, [total, self()]),

    VertMap = #{
      one => ok_v([]),
      two => ok_v([one]),
      two_and_a_half => ok_v([one]),
      three => bad_v([two])
     },

    {ok, _Pid} = wrek:start(VertMap, [{event_manager, EvMgr}]),

    Events = receive
        #{evts := Evts} -> Evts
    end,

    gen_event:stop(EvMgr),

    WrekStarts =
        [E || E = #wrek_event{type = {wrek, start}} <- Events],
    StartingVerts =
        [E || E = #wrek_event{msg = {starting_vert, _}} <- Events],
    VertStarts =
        [E || E = #wrek_event{type = {vert, start}} <- Events],
    VertDones =
        [E || E = #wrek_event{type = {vert, done}} <- Events],
    WrekErrors =
        [E || E = #wrek_event{type = {wrek, error}} <- Events],

    ?assertEqual(1, length(WrekStarts)),
    ?assertEqual(4, length(StartingVerts)),
    ?assertEqual(4, length(VertStarts)),
    ?assertEqual(4, length(VertDones)),
    ?assertEqual(1, length(WrekErrors)).

event_time_diff_test() ->
    E1 = #wrek_event{},
    E2 = #wrek_event{},
    ?assert(wrek_event:time_diff(E1, E2) >= 0).


fail_cancels_transitive_closure_test() ->
    application:start(wrek),

    VertMap = #{
      one => bad_v([]),
      two => ok_v([one]),
      three => ok_v([two])
    },

    {ok, EvMgr} = gen_event:start_link({local, wrek_test_manager}),
    gen_event:add_handler(EvMgr, wrek_test_handler, [partial, self()]),

    Opts = [{event_manager, EvMgr}, {failure_mode, partial}],
    {ok, _Pid} = wrek:start(VertMap, Opts),

    Msgs = receive
        #{evts := M} -> M
    end,

    gen_event:stop(EvMgr),

    CancelMsgs = lists:filter(fun
        (#wrek_event{msg = {vert_cancelled, _}}) -> true;
        (_) -> false
    end, Msgs),

    ?assertEqual(2, length(CancelMsgs)).


partial_success_test() ->
    application:start(wrek),

    VertMap = #{
      one => ok_v([]),
      bad_two => bad_v([one]),
      bad_three => bad_v([bad_two]),
      ok_two => ok_v([one]),
      ok_three => ok_v([ok_two]),
      four => ok_v([ok_three, bad_three])
    },

    {ok, EvMgr} = gen_event:start_link({local, wrek_test_manager}),
    gen_event:add_handler(EvMgr, wrek_test_handler, [partial, self()]),

    Opts = [{event_manager, EvMgr}, {failure_mode, partial}],
    {ok, _Pid} = wrek:start(VertMap, Opts),

    Events = receive
        #{evts := M} -> M
    end,

    gen_event:stop(EvMgr),

    SuccessMsgs =
        [E || E = #wrek_event{type = {vert, done}, msg = {ok, _}} <- Events],

    CancelMsgs =
        [E || E = #wrek_event{msg = {vert_cancelled, _}} <- Events],

    FailMsgs =
        [E || E = #wrek_event{type = {_, error}, msg = {vert, _}} <- Events],

    % one, ok_two, ok_three
    ?assertEqual(3, length(SuccessMsgs)),
    % bad_two
    ?assertEqual(1, length(FailMsgs)),
    % bad_three, four
    ?assertEqual(2, length(CancelMsgs)).


timeout_test() ->
    VertDefn = #{module => wrek_sleep_vert, args => [], deps => [], timeout => 10},
    VertMap = #{cat => VertDefn},

    {ok, EvMgr} = gen_event:start_link({local, wrek_test_manager}),
    gen_event:add_handler(EvMgr, wrek_test_handler, [total, self()]),

    {ok, _Pid} = wrek:start(VertMap, [{event_manager, EvMgr}]),

    Map = receive
        M -> M
    end,

    gen_event:stop(EvMgr),

    FailMsgs = lists:filter(fun
        (#wrek_event{type = {wrek, error}, msg = {vert, _}}) -> true;
        (#wrek_event{type = {vert, done}, msg = timeout}) -> true;
        (_) -> false
    end, maps:get(evts, Map)),

    ?assertEqual(2, length(FailMsgs)).

get_test() ->
    application:start(wrek),

    IdVerts = [1, 2, 3],
    Pairs = [{result, From} || From <- IdVerts] ++ [{extra, 4}],

    VertMap = #{
      1 => id_v(1, []),
      2 => id_v(2, [1]),
      3 => id_v(3, [1]),
      4 => #{module => wrek_true_vert, args => [], deps => [1], extra => 4},
      get => get_v(Pairs)
    },

    {ok, EvMgr} = gen_event:start_link({local, wrek_test_manager}),
    gen_event:add_handler(EvMgr, wrek_test_handler, [total, self()]),

    Opts = [{event_manager, EvMgr}, {failure_mode, total}],
    {ok, _Pid} = wrek:start(VertMap, Opts),

    Msgs = receive
        #{evts := M} -> M
    end,

    gen_event:stop(EvMgr),

    IdNameMap = lists:foldl(fun
        (#wrek_event{id = Id, type = {vert, start}, msg = {Name, _, _}}, Acc) ->
            Acc#{Id => Name};
        (_, Acc) -> Acc
    end, #{}, Msgs),

    ReturnVals = lists:foldl(fun
        (#wrek_event{id = Id, type = {vert, done}, msg = {ok, Val}}, Acc) ->
            #{Id := Name} = IdNameMap,
            Acc#{Name => Val};
        (_, Acc) -> Acc
    end, #{}, Msgs),

    GetExpect = #{
      1 => 1,
      2 => 2,
      3 => 3,
      4 => 4
    },

    ?assertMatch(#{1 := #{result := 1}}, ReturnVals),
    ?assertMatch(#{2 := #{result := 2}}, ReturnVals),
    ?assertMatch(#{3 := #{result := 3}}, ReturnVals),
    ?assertMatch(#{get := GetExpect}, ReturnVals).

sandbox_test() ->
    application:start(wrek),

    VertMap = #{
        1 => #{module => wrek_make_sandbox_vert, args => [], deps => []},
        2 => #{module => wrek_get_sandbox_vert, args => [1], deps => [1]},
        3 => #{module => wrek_reuse_sandbox_vert, args => [1], deps => [1]},
        4 => #{module => wrek_get_sandbox_vert, args => [3], deps => [3]}
    },

    {ok, EvMgr} = gen_event:start_link({local, wrek_test_manager}),
    gen_event:add_handler(EvMgr, wrek_test_handler, [total, self()]),

    Opts = [{event_manager, EvMgr}, {failure_mode, total}],
    {ok, _Pid} = wrek:start(VertMap, Opts),

    Msgs = receive
        #{evts := M} -> M
    end,

    gen_event:stop(EvMgr),

    Notifs = lists:foldl(fun
        (#wrek_event{type = {vert, msg}, msg = {dir, D}}, Acc) ->
            [D | Acc];
        (_, Acc) ->
            Acc
    end, [], Msgs),

    ?assertEqual(4, length(Notifs)),
    ?assertEqual(lists:usort(Notifs), [lists:nth(1, Notifs)]).

custom_exec_callback_test() ->
    Self = self(),
    Callback = fun({_, Data}) ->
        Self ! {event_fun, Data}
    end,

    {ok, EvMgr} = gen_event:start_link(),
    gen_event:add_handler(EvMgr, wrek_cb_event_handler, [Self]),

    Dag = #{cb => #{module => wrek_echo_vert, args => [Callback, "good"], deps => []}},

    wrek:start(Dag, [{event_manager, EvMgr}]),

    EventFunMsg = receive
        {event_fun, Efm} -> Efm
    after
        500 -> bad
    end,

    EventHandlerMsg = receive
        {event_handler, Ehm} -> Ehm
    after
        500 -> bad
    end,

    gen_event:stop(EvMgr),

    ?assertEqual(EventFunMsg, <<"good">>),
    ?assertEqual(EventHandlerMsg, <<"good">>),
    ?assertEqual(EventFunMsg, EventHandlerMsg).

%% private

ok_v(Deps) ->
    #{module => wrek_true_vert, args => [], deps => Deps}.

bad_v(Deps) ->
    #{module => wrek_bad_vert, args => [], deps => Deps}.

id_v(Val, Deps) ->
    #{module => wrek_id_vert, args => [Val], deps => Deps}.

get_v(Pairs) ->
    Deps = [Dep || {_K, Dep} <- Pairs],
    #{module => wrek_get_vert, args => Pairs, deps => Deps}.
