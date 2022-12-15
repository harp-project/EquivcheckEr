-module(general_refac).

-compile(export_all). % Exports all functions
-compile(debug_info).

% These all have to be called from the directory of the source code!

diffing(OrigHash, RefacHash) ->
    Out = os:cmd("git diff --no-ext-diff " ++ OrigHash ++ " " ++ RefacHash ++ " | \grep -o '^[-+][^[:space:]]*([^->]*)' | tr -d '+-'"),
    FunStrs = string:split(string:trim(Out), "\n"),
    lists:map(fun(X) -> format(X) end, FunStrs).

format(FunStr) ->
    [Name , ArgStr] = string:split(FunStr, "("),
    Arit = length(string:split(ArgStr, ",", all)),
    {Name, Arit}.

get_name(FunStr) ->
    [Name , _] = string:split(FunStr, "("),
    Name.

get_arity(FunStr) ->
    [_ , ArgStr] = string:split(FunStr, "("),
    length(string:split(ArgStr, ",", all)).

get_filename(FunName) ->
    string:trim(os:cmd("grep -l '^" ++ FunName ++ "' *")).

% TODO Doesn't work with nested files
find_callers({FunName, Arity}) ->
    FileName = get_filename(FunName),
    FunNameAtom = list_to_atom(FunName),
    {_, Folder} = file:get_cwd(),
    {_, Funs} = wrangler_code_inspector_lib:calls_to_fun_1(FileName, FunNameAtom, Arity, [Folder], 4),
    lists:map(fun({{FileName,_,_},_}) -> FileName end, Funs).

% TODO Functions can have the same name with different arity
get_spec({FunName, _}) ->
    FileName = get_filename(FunName),
    {_, File} = file:read_file(FileName),
    Source = erlang:binary_to_list(File),
    Lines = string:split(Source, "\n", all),
    hd(lists:dropwhile(fun(X) -> not(lists:prefix("-spec", X)) end, Lines)).
