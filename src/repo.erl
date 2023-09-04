-module(repo).

-export([copy/2,
         get_diff/2,
         checkout/2,
         current_commit/0]).

-spec copy(string(), string()) -> string().
copy(ProjFolder, TmpFolder) ->
    file:make_dir(TmpFolder),
    os:cmd("git clone " ++ ProjFolder ++ " " ++ TmpFolder),
    TmpFolder.

-spec checkout(string(), string()) -> string().
checkout(Dir, Hash) ->
    {ok, CurrDir} = file:get_cwd(),
    file:set_cwd(Dir),
    os:cmd("git checkout " ++ Hash),
    file:set_cwd(CurrDir).

get_diff(OrigHash, RefacHash) ->
    os:cmd("git diff -U0 --no-ext-diff " ++ OrigHash ++ " " ++ RefacHash).

current_commit() ->
    os:cmd("git show --oneline -s --format=%H").
