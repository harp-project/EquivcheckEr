%% This module finds the functions that will be tested with random data
%% based on the diff output of the two commits and the type of refactoring
%% that was done
-module(scoping).

-compile(export_all). % Exports all functions
-compile(debug_info).

-type fun_info() :: {atom(), string(), integer()}.

% These all have to be called from the directory of the source code! TODO, pass the folder as argument?

-spec scope(string()) -> {[string()], [{atom(), atom(), proper_types:type()}]}.
scope(Diff) ->
    % TODO For new, we only consider function renaming,
    % but later other kinds of refactorings will be added
    Parsed = parse_diff(Diff),
    {Callee, Callers} = renaming(Parsed),
    Files = extract_files({Callee, Callers}),
    Functions = typing:add_types(Callers),
    {Files, Functions}.

-spec parse_diff(string()) -> {string(), [string()]}.
parse_diff(Diff) ->
    [_|Files] = string:split(Diff, "diff --git ", all),
    Lines = lists:map(fun(X) -> string:split(X, "\n", all) end, Files),
    lists:map(fun([H|T]) -> {extract_file(H),T} end, Lines).

-spec extract_file(string()) -> string().
extract_file(DiffLine) ->
    lists:nth(2, string:split(lists:nth(1, string:split(DiffLine, " ", all)), "/")).

-spec match_renaming(string()) -> boolean().
match_renaming(Line) ->
    % Regexp for finding top-level function definitions
    case re:run(Line, "^[\\+-][^-[:space:]].* ->", []) of
        nomatch -> false;
        _       -> true
    end.

-spec empty(list()) -> boolean().
empty(List) ->
    case List of
        [] -> true;
        _  -> false
    end.

-spec get_name(string()) -> string().
get_name(FunStr) ->
    [Name, _] = string:split(FunStr, "("),
    Name.

-spec get_arity(string()) -> string().
get_arity(FunStr) ->
    [_ , ArgStr] = string:split(FunStr, "("),
    length(string:split(ArgStr, ",", all)).

-spec renaming({string(), [string()]}) -> {fun_info(), [fun_info()]}.
renaming(Parsed_Diff) ->
    Changed = lists:map(fun({File,Lines}) -> {File, lists:filter(fun(Line) -> match_renaming(Line) end, Lines)} end, Parsed_Diff),
    [{File, Funs}] = lists:filter(fun({_,List}) -> not(empty(List)) end, Changed),
    [{OldName, Arity}, {NewName, _}] = lists:map(fun([_|FunStr]) -> {get_name(FunStr), get_arity(FunStr)} end, Funs),
    Callee = {get_module(File), NewName, Arity},
    Callers = find_callers({File, NewName, Arity}),
    {Callee, Callers}.

-spec find_callers({string(), string(), integer()}) -> [fun_info()].
find_callers({FileName, FunName, Arity}) ->
    FunNameAtom = list_to_atom(FunName),
    {_, Folder} = file:get_cwd(),
    {_, Funs} = wrangler_code_inspector_lib:calls_to_fun_1(FileName, FunNameAtom, Arity, [Folder], 4),
    lists:map(fun({{FileName, F, A}, _}) -> {get_module(FileName), erlang:atom_to_list(F), A} end, Funs).

-spec extract_files({fun_info(), [fun_info()]}) -> [string()].
extract_files({Callee, Callers}) ->
    Files = lists:map(fun({Module, _, _}) -> erlang:atom_to_list(Module) ++ ".erl" end, [Callee|Callers]),
    lists:uniq(Files).

-spec get_module(string()) -> atom().
get_module(FileName) ->
    erlang:list_to_atom(hd(string:split(lists:last(string:split(FileName,"/",all)),"."))).
