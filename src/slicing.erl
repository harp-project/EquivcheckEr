%% This module finds the functions that will be tested with random data
%% based on the diff output of the two commits
-module(slicing).

-export([scope/2,
        find_callers/1]).

-type types()       :: [string()].
-type fun_info()    :: {mfa(), types()}.

-spec scope([fun_info()], [fun_info()]) -> [fun_info()].
scope(OrigFuns, RefacFuns) ->

    {SameSig, DiffSig} = lists:partition(fun(Fun) -> lists:member(Fun, OrigFuns) end, RefacFuns),
    SameSig.

    % NewFuncs = new(Hunks),
    % RemovedFuncs = removed(Hunks),

    % ModifiedFuncs = modified(Hunks),
    % {Callees, Callers} = case lists:any(fun signature_change/1, ModifiedFuncs) of
    %                         true  -> has_sig_change(ModifiedFuncs);
    %                         false -> no_sig_change(ModifiedFuncs)
    %                     end,
    % Files = extract_files(Callees, Callers),
    % {Files, typing:add_types(Callers)}.


-spec find_callers(mfa()) -> [mfa()].
find_callers({Module, FunName, Arity}) ->
    FileName = utils:module_to_filename(Module),
    {_, Folder} = file:get_cwd(),
    {_, Funs} = wrangler_code_inspector_lib:calls_to_fun_1(FileName, FunName, Arity, [Folder], 4),
    lists:map(fun({{FileName, F, A}, _}) -> {utils:filename_to_module(FileName), erlang:atom_to_list(F), A} end, Funs).

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

