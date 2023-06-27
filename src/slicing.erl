%% This module finds the functions that will be tested with random data
-module(slicing).

-export([scope/4]).

-type types()       :: [string()].
-type fun_info()    :: {mfa(), types()}.

% Get all the modified functions, separates them into two groups
% based on whether their interface changed, and if so, finds the callers
% based on the callgraph
-spec scope([mfa()], [mfa()], fun(), fun()) -> [fun_info()].
scope(OrigMFAs, RefacMFAs, CallGraph, Types) ->
    OrigFuns = Types(OrigMFAs, original),
    RefacFuns = Types(RefacMFAs, refactored),

    {SameSig, DiffSig} = lists:partition(fun(Fun) -> lists:member(Fun, RefacFuns) end, OrigFuns),
    Callers = lists:flatmap(fun(Fun) -> CallGraph(element(1,Fun), original) end, DiffSig),
    lists:uniq(Types(Callers, original) ++ SameSig).
