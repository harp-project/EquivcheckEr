%% This module finds the initial set of functions that will be tested with random data
-module(slicing).

-export([scope/4]).

-type types()     :: [string()].
-type filename()  :: string().
-type fun_info()  :: {filename(), mfa()}.
-type fun_typed() :: {filename(), mfa(), types()}.

% Get all the modified functions, separates them into two groups
% based on whether their interface changed, and if so, finds the callers
% based on the callgraph
-spec scope([fun_info()], [fun_info()], fun(), fun()) -> [fun_typed()].
scope(OrigModFuns, RefacModFuns, CallGraph, Types) ->
    OrigFunsTyped = lists:map(fun({FileName, MFA}) -> {FileName, MFA, Types(MFA, original)} end, OrigModFuns),
    RefacFunsTyped = lists:map(fun({FileName, MFA}) -> {FileName, MFA, Types(MFA, refactored)} end, RefacModFuns),

    {SameSig, DiffSig} = lists:partition(fun(Fun) -> lists:member(Fun, RefacFunsTyped) end, OrigFunsTyped),

    Callers = lists:flatmap(fun({FileName, {_, F, A}, _}) -> CallGraph({FileName, F, A}, original) end, DiffSig),
    CallersTyped = lists:map(fun({FileName, MFA}) -> {FileName, MFA, Types(MFA, original)} end, Callers),

    lists:uniq(CallersTyped ++ SameSig).
