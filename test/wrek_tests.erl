-module(wrek_tests).
-include_lib("eunit/include/eunit.hrl").

from_verts_ok_test() ->
    Verts = #{
      foo => ok_v([]),
      bar => ok_v([foo]),
      baz => ok_v([bar])
     },
    {ok, Dag} = wrek_utils:from_verts(Verts),

    ?assertEqual([foo, bar, baz],
                 digraph_utils:topsort(Dag)).

from_verts_cycle_test() ->
    Cycle = #{
      foo => ok_v([baz]),
      bar => ok_v([foo]),
      baz => ok_v([bar])
     },
    ?assertMatch({error, {bad_edge, _}},
                 wrek_utils:from_verts(Cycle)).

from_verts_missing_dep_test() ->
    Verts = #{bar => #{deps => [foo]}},

    ?assertMatch({error, {bad_vertex, _}},
                 wrek_utils:from_verts(Verts)).

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
    gen_event:add_handler(EvMgr, wrek_test_handler, [self()]),

    VertMap = #{
      one => ok_v([]),
      two => ok_v([one]),
      two_and_a_half => ok_v([one]),
      three => ok_v([two])
     },

    {ok, _Pid} = wrek:start(VertMap, [{event_manager, EvMgr}]),

    Count = receive
        Cnt -> Cnt
    end,

    gen_event:stop(EvMgr),

    % {wrek, start} +
    % ({starting_vert}, {vert, start}, {vert, done}) * #verts +
    % {wrek, done} =
    % (3 * #verts) + 2
    ?assertEqual((3 * maps:size(VertMap)) + 2, Count).

event_bad_test() ->
    exec:start(),
    application:start(wrek),

    {ok, EvMgr} = gen_event:start_link({local, wrek_test_manager}),
    gen_event:add_handler(EvMgr, wrek_test_handler, [self()]),

    VertMap = #{
      one => ok_v([]),
      two => ok_v([one]),
      two_and_a_half => ok_v([one]),
      three => bad_v([two])
     },

    {ok, _Pid} = wrek:start(VertMap, [{event_manager, EvMgr}]),

    Count = receive
        Cnt -> Cnt
    end,

    gen_event:stop(EvMgr),

    % {wrek, start} +
    % ({starting_vert}, {vert, start}, {vert, done}) * #ok_verts +
    % ({starting_vert}, {vert, start}, {exec, exit_status}, {vert, done}) * #bad_verts +
    % {wrek, done} =
    % (3 * #ok_verts) + (4 * #bad_verts) + 2
    ?assertEqual((3 * maps:size(VertMap)) + 3, Count).


%% private

ok_v(Deps) ->
    #{module => wrek_true_vert, args => [], deps => Deps}.

bad_v(Deps) ->
    #{module => wrek_bad_vert, args => [], deps => Deps}.
