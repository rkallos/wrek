wrek
=====

__Author:__ Richard Kallos

Lightweight concurrent DAG execution engine


Description
-----------

Wrek is designed to execute task dependency graphs concurrently.  A vertex `V`
in a task dependency graph can execute once all vertices with paths to `V` have
finished executing. This execution model is similar to tools like Concourse CI.

To retrieve events from wrek, you can pass in the pid of a `gen_event` process,
and add handlers as you see fit.


Requirements
------------

* Erlang 19.0+


Build
-----

    $ rebar3 compile


How to use
----------

1. Write callback modules that implement the `wrek_vert` behaviour.
2. Create a map `Map` reflecting the structure of the graph you want to run.
3. `wrek:start(Map)` or `wrek:start(Map, Opts)`.


Options
-------

- `{event_manager, pid()}`: Specify a `gen_event` process to forward events to
- `{failure_mode, partial | total}` (default: total): Switch between partial and total failure modes. Total failure will immediately shut down all running vertices within a DAG. Partial failure will cancel running all tasks reachable by any failed vertex, but will continue until all vertices finish running.


Example
-------
```erlang
-module(true_vert).
-behaviour(wrek_vert).

-export([run/2]).

run(_Args, Parent) ->
    {ok, Fun} = wrek_vert:exec(Parent, ".", "true"),
    ok = Fun(),
    {ok, #{}}.
```

```erlang
1> Map = #{
    one => #{module => true_vert, args => [], deps => []},
    two => #{module => true_vert, args => [], deps => [one]},
    three => #{module => true_vert, args => [], deps => [one]}}.
2> wrek:start(Map). % Runs one, then two+three concurrently
```

TODO
----

- Add timeout for DAG. It might be worthwhile to have timeouts for individual vertices, too, but having a timeout for the whole graph is a start.
