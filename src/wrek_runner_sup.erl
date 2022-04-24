-module(wrek_runner_sup).

-export([start_link/1]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).


-spec start_link(Names :: list(atom())) -> {ok, pid()} | ignore | {error, term()}.

start_link(Names) ->
    {ok, Sup} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    [ supervisor:start_child(?MODULE,
                             #{id => X,
                               start => {wrek_vert_runner, start_link, [#{stop_on_completion => false}]},
                               restart => temporary,
                               type => worker})
      || X <- Names ],
    {ok, Sup}.

%% Callbacks

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    SupFlags = #{
      strategy => one_for_one,
      intensity => 0,
      period => 1
     },
    {ok, {SupFlags, []}}.
