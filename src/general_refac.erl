-module(general_refac).
-compile(export_all). % Exports all functions
-compile(debug_info).

diffing(OrigHash, RefacHash) ->
    Out = os:cmd("git diff --no-ext-diff " ++ OrigHash ++ " " ++ RefacHash ++ " | \grep -o '^[-+][^[:space:]]*([^->]*)' | tr -d '+-'"),
    string:split(string:trim(Out), "\n").
