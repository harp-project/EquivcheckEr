%% This module finds the functions that will be tested with random data
%% based on the diff output of the two commits and the type of refactoring
%% that was done

-module(scoping).

-compile(export_all). % Exports all functions
-compile(debug_info).

% These all have to be called from the directory of the source code! TODO, pass the folder as argument?

scope(Diff) ->
    % TODO For new, we only consider function renaming,
    % but later other kinds of refactorings will be added
    Parsed = parse_diff(Diff),
    renaming(Parsed).

parse_diff(Diff) ->
    [_|Files] = string:split(Diff, "diff --git ", all),
    Lines = lists:map(fun(X) -> string:split(X, "\n", all) end, Files),
    lists:map(fun([H|T]) -> {extract_file(H),T} end, Lines).

extract_file(DiffLine) ->
    lists:nth(2,string:split(lists:nth(1,string:split(DiffLine," ", all)),"/")).

match_renaming(Line) ->
    case re:run(Line, "^[\\+-][^-[:space:]].* ->", []) of
        nomatch -> false;
        _       -> true
    end.

empty(List) ->
    case List of
        [] -> true;
        _  -> false
    end.

get_name(FunStr) ->
    [Name, _] = string:split(FunStr, "("),
    Name.

get_arity(FunStr) ->
    [_ , ArgStr] = string:split(FunStr, "("),
    length(string:split(ArgStr, ",", all)).

renaming(Parsed_Diff) ->
    Changed = lists:map(fun({File,Lines}) -> {File, lists:filter(fun(Line) -> match_renaming(Line) end, Lines)} end, Parsed_Diff),
    [{File, Funs}] = lists:filter(fun({_,List}) -> not(empty(List)) end, Changed),
    [{OldName, Arity}, {NewName, _}] = lists:map(fun([_|FunStr]) -> {get_name(FunStr), get_arity(FunStr)} end, Funs),
    Callee = {File, NewName, Arity},
    Callers = find_callers(Callee),
    {Callee, Callers}.

find_callers({FileName, FunName, Arity}) ->
    FunNameAtom = list_to_atom(FunName),
    {_, Folder} = file:get_cwd(),
    {_, Funs} = wrangler_code_inspector_lib:calls_to_fun_1(FileName, FunNameAtom, Arity, [Folder], 4),
    lists:map(fun({{FileName, F, A}, _}) -> {FileName, erlang:atom_to_list(F), A} end, Funs).
