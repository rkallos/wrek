-module(wrek_sup).

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).


-spec start_link() -> {ok, pid()} | ignore | {error, term()}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% Callbacks

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    SupFlags = #{
      strategy => one_for_one,
      intensity => 0,
      period => 1
     },
    {ok, {SupFlags, []}}.
