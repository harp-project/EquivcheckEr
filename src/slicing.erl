%% This module finds the functions that will be tested with random data
%% based on the diff output of the two commits and the type of refactoring
%% that was done
-module(slicing).

-export([scope/2]).

-type fun_info() :: {atom(), string(), integer()}.

% These all have to be called from the directory of the source code! TODO, pass the folder as argument?

-spec scope(string(), [string()]) -> {[{atom(), atom(), proper_types:type()}]}.
scope(Diff, Sources) ->
    % TODO For new, we only consider function renaming,
    % but later other kinds of refactorings will be added
    Hunks = diff:diff(Diff, Sources),
    Functions = modified_funcs(Hunks),
    NewFuns = lists:map(fun({_,Fun}) -> Fun end, Functions),
    typing:add_types(NewFuns).
    % Diffs = extract_diffs(Diff),
    % {Callee, Callers} = renaming(Diffs),
    % Files = extract_files({Callee, Callers}),
    % Functions = typing:add_types(Callers),
    % {Files, Functions}.

modified_funcs(Hunks) ->
    NotFunc = fun({{_, X, _},_}) -> X =/= [] end, % Not a change in function
    lists:uniq(lists:filter(NotFunc, lists:map(fun diff:get_funcs/1, Hunks))).

% find_renaming(Diffs) ->
%     FunDefs = lists:filter(fun(Line) -> is_function_sig(Line) end, Diffs),
%     FunNames = lists:map(fun([_|FunDef]) -> {Name, _} = get_name_and_arity(FunDef), Name end, FunDefs),
%     [H|_] = lists:filter(fun([P1, P2]) -> P1 =/= P2 end, utils:group_by_two(FunNames)),
%     H.
%
% -spec renaming({string(), [string()]}) -> {fun_info(), [fun_info()]}.
% renaming(Diffs) ->
%     % Find files where function definitions have changed
%     ChangesByFile = lists:map(fun({File, Lines}) -> {File, lists:filter(fun(Line) -> is_function_sig(Line) end, Lines)} end, Diffs),
%
%     [{File, [Old,New|_]}] = lists:filter(fun({_,List}) -> not(List == []) end, ChangesByFile),
%
%     [{OldName, Arity}, {NewName, _}] = lists:map(fun([_|FunStr]) -> get_name_and_arity(FunStr) end, [Old,New]),
%
%     Callee = {get_module(File), NewName, Arity},
%     Callers = find_callers({File, NewName, Arity}),
%     {Callee, Callers}.

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
    erlang:list_to_atom(filename:basename(FileName, ".erl")).
