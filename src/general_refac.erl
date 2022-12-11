-module(general_refac).

-compile(export_all). % Exports all functions
-compile(debug_info).

diffing(OrigHash, RefacHash) ->
    Out = os:cmd("git diff --no-ext-diff " ++ OrigHash ++ " " ++ RefacHash ++ " | \grep -o '^[-+][^[:space:]]*([^->]*)' | tr -d '+-'"),
    string:split(string:trim(Out), "\n").

get_arity(FunStr) ->
    [_ , ArgStr] = string:split(FunStr, "("),
    length(string:split(ArgStr, ",", all)).

get_filename(FunName) ->
    string:trim(os:cmd("grep -l '^" ++ FunName ++ "' *")).

find_callers(FunName, Arity) ->
    FileName = get_filename(FunName),
    FunNameAtom = list_to_atom(FunName),
    {_, Funs} = wrangler_code_inspector_lib:calls_to_fun_1(FileName, FunNameAtom, Arity, ["."], 4),
    Funs.
