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


Example
-------
```erlang
-module(true_vert).
-behaviour(wrek_vert).

-export([run/2]).

run(_Args, Parent) ->
    Fun = wrek_vert:exec(Parent, ".", "true"),
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
