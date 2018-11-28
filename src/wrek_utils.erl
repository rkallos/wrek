-module(wrek_utils).

-export([
    rm/1,
    rmdir/1,
    sandbox/2
]).


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
