-module(wrek_exec).

-export([exec/4]).

-type env() :: [{string(), string()}].


-spec exec(file:filename_all(), string(), env(), fun()) ->
    ok | {error, _}.

exec(Dir0, Cmd0, Env, EventFun) ->
    Fun = fun(Fd, _OsPid, Data) ->
        EventFun({Fd, Data})
    end,

    Dir = case Dir0 of
        D when is_binary(D) ->
            binary_to_list(D);
        D when is_list(D) ->
            D
    end,

    ExecOpts = [
        sync,
        {cd, Dir},
        {env, Env},
        {kill_timeout, 0},
        {stdout, Fun},
        {stderr, Fun}
     ],

    Cmd = lists:flatten(Cmd0),

    case exec:run_link(Cmd, ExecOpts) of
        {ok, []} -> ok;
        {error, [{exit_status, ExitStatus}]} ->
            EventFun({exit_status, ExitStatus}),
            {error, {exit_status, ExitStatus}}
    end.
