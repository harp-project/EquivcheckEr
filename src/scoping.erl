%% This module finds the functions that will be tested with random data
%% based on the diff output of the two commits and the type of refactoring
%% that was done

-module(scoping).

-compile(export_all). % Exports all functions
-compile(debug_info).

% These all have to be called from the directory of the source code! TODO, pass the folder as argument?

diff_renaming(OrigHash, RefacHash) ->
    Out = os:cmd("git diff --no-ext-diff " ++ OrigHash ++ " " ++ RefacHash ++ " | \grep -o '^[-+][^[:space:]]*([^->]*)' | tr -d '+-'"),
    [OrigFunStr, RefacFunStr] = string:split(string:trim(Out), "\n"),
    {get_filename(RefacFunStr), {get_name(OrigFunStr), get_name(RefacFunStr)}, get_arity(OrigFunStr)}.

get_name(FunStr) ->
    [Name , _] = string:split(FunStr, "("),
    Name.

get_arity(FunStr) ->
    [_ , ArgStr] = string:split(FunStr, "("),
    length(string:split(ArgStr, ",", all)).

% TODO Use wrangler to get this?
% TODO Doesn't work with nested files
get_filename(FunName) ->
    string:trim(os:cmd("grep -l '^" ++ FunName ++ "' *")).

find_callers({FunName, Arity}) ->
    FileName = get_filename(FunName),
    FunNameAtom = list_to_atom(FunName),
    {_, Folder} = file:get_cwd(),
    {_, Funs} = wrangler_code_inspector_lib:calls_to_fun_1(FileName, FunNameAtom, Arity, [Folder], 4),
    lists:map(fun({{FileName, F, A}, _}) -> {FileName, erlang:atom_to_list(F), A} end, Funs).
