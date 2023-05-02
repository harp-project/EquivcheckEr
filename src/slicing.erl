%% This module finds the functions that will be tested with random data
%% based on the diff output of the two commits and the type of refactoring
%% that was done
-module(slicing).

-export([scope/2]).

-type fun_info() :: {atom(), string(), integer()}.

% These all have to be called from the directory of the source code! TODO, pass the folder as argument?

-spec scope(string(), [string()]) -> {[{atom(), atom(), proper_types:type()}]}.
scope(Diff, Sources) ->
    Hunks = diff:diff(Diff, Sources),

    NewFuncs = new(Hunks),
    RemovedFuncs = removed(Hunks),

    ModifiedFuncs = modified(Hunks),
    {Callees, Callers} = case lists:any(fun signature_change/1, ModifiedFuncs) of
                            true  -> has_sig_change(ModifiedFuncs);
                            false -> no_sig_change(ModifiedFuncs)
                        end,
    Files = extract_files(Callees, Callers),
    {Files, typing:add_types(Callers)}.

-spec find_callers({string(), string(), integer()}) -> [fun_info()].
find_callers({FileName, FunName, Arity}) ->
    FunNameAtom = list_to_atom(FunName),
    {_, Folder} = file:get_cwd(),
    {_, Funs} = wrangler_code_inspector_lib:calls_to_fun_1(FileName, FunNameAtom, Arity, [Folder], 4),
    lists:map(fun({{FileName, F, A}, _}) -> {get_module(FileName), erlang:atom_to_list(F), A} end, Funs).

has_sig_change(Funs) ->
    NewFuns = lists:map(fun({_, Fun}) -> Fun end, lists:filter(fun signature_change/1, Funs)),
    CallerSet = lists:foldr(fun(Func, Scope) ->
                        sets:add_element(find_callers(Func), Scope) end, sets:new(), NewFuns),
    {NewFuns, hd(sets:to_list(CallerSet))}. % TODO: No idea why it returns a list of lists

no_sig_change(Funs) ->
    NewFuns = lists:map(fun({_, Fun}) -> Fun end, Funs),
    CallerSet = lists:foldr(fun(Func, Scope) ->
                        sets:add_element(find_callers(Func), Scope) end, sets:new(), NewFuns),
    {NewFuns, hd(sets:to_list(CallerSet))}. % TODO: No idea why it returns a list of lists

signature_change({{_, OldName, OldArity}, {_, NewName, NewArity}}) ->
    (OldName =/= NewName) or (OldArity =/= NewArity).


modified(Hunks) ->
    Pred = fun(H) -> (diff:get_new(H) =/= {}) and (diff:get_old(H) =/= {}) end,
    lists:map(fun diff:get_funcs/1, lists:filter(Pred, Hunks)).
    % lists:uniq(lists:filter(NotFunc, lists:map(fun diff:get_funcs/1, Hunks))).

removed(Hunks) ->
    Pred = fun(H) -> diff:get_new(H) =:= {} end,
    lists:map(fun diff:get_old/1, lists:filter(Pred, Hunks)).

new(Hunks) ->
    Pred = fun(H) -> diff:get_old(H) =:= {} end,
    lists:map(fun diff:get_new/1, lists:filter(Pred, Hunks)).

-spec extract_files([fun_info()], [fun_info()]) -> [string()]. % TODO: This is too specific, and also horrible
extract_files(Callees, Callers) ->
    CalleesFiles = lists:map(fun({FileName, _, _}) -> FileName end, Callees),
    CallersFiles = lists:map(fun({Module, _, _}) -> erlang:atom_to_list(Module) ++ ".erl" end, Callers),
    lists:uniq(CalleesFiles ++ CallersFiles).

-spec get_module(string()) -> atom().
get_module(FileName) ->
    erlang:list_to_atom(filename:basename(FileName, ".erl")).
