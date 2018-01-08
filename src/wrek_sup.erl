-module(wrek_sup).

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% Callbacks

init([]) ->
    SupFlags = #{
      strategy => one_for_one,
      intensity => 0,
      period => 1
     },
    {ok, {SupFlags, []}}.
