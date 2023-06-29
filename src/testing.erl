-module(testing).

-type type() :: string().

-export([run_tests/5,
        run_tests/6,
         eval_proc/3,
         eval_func/4]).

-define(PEER_TIMEOUT, 1000).

-include_lib("proper/include/proper.hrl").

run_tests(Functions, OrigNode, RefacNode, Types, CallGraph) ->
    run_tests(Functions, OrigNode, RefacNode, Types, CallGraph, []).

run_tests([], _, _, _, _, Results) ->
    Results;
run_tests(Functions, OrigNode, RefacNode, Types, CallGraph, Results) ->
    % proper:quickchek/2 stops the server, so it has to be started every time
    proper_typeserver:start(),
    ProperOpts = [long_result, {on_output, fun(X,Y) -> utils:count_tests(X,Y) end}],
    % ProperOpts = [long_result, quiet],

    % Convert type information to PropEr type
    FunctionsTyped = lists:map(fun({FileName, {M,F,A}, ArgTypes}) -> {FileName, {M, F, A}, convert_type(M,ArgTypes)} end, Functions),

    % {{M, F, A},_} = hd(Functions),

    % A result is a tuple: {Module, Function, Counterexample}
    % If no counterexample is found, the third value is 'true' instead
    lists:map(fun(Function) ->
                      test_function(Function, OrigNode, RefacNode, ProperOpts, self())
              end, FunctionsTyped),
    Res = collect_results(length(FunctionsTyped), []),

    FailedFuns = lists:filter(fun({_, _, Eq}) -> Eq =/= true end, Res),
    FailedMFA = lists:map(fun({FileName, {M,F,A}, _}) -> {FileName, {M, F, A}} end, FailedFuns),

    Callers = lists:uniq(lists:flatmap(fun({FileName, {_,F,A}}) -> CallGraph({FileName, F, A}, refactored) end, FailedMFA)),
    CallersTyped = lists:map(fun({FileName, MFA}) -> {FileName, MFA, Types(MFA, refactored)} end, Callers),

    run_tests(CallersTyped, OrigNode, RefacNode, Types, CallGraph, FailedFuns ++ Results).

collect_results(0, Res) -> Res;
collect_results(Num, Res) ->
    receive
        Val -> collect_results(Num - 1, [Val|Res])
    end.

% -spec eval_func(pid(), atom(), atom(), [term()]) -> {atom(), term()}.
eval_func(M, F, A, Pid) ->
    Pid ! {self(), try erlang:apply(M, F, A) of
              Val -> {normal, Val}
          catch
              error:Error -> error
          end}.

eval_proc(M, F, A) ->
    Pid = spawn(testing, eval_func, [M, F, A, self()]),
    receive
        {Pid, Val} -> Val
    end.

% Spawns a process on each node that evaluates the function and
% sends back the result to this process
-spec prop_same_output(pid(), pid(), atom(), atom(), [term()]) -> boolean().
prop_same_output(OrigNode, RefacNode, M, F, A) ->
    
    Out1 = peer:call(OrigNode, testing, eval_proc, [M, F, A], ?PEER_TIMEOUT),
    Out2 = peer:call(RefacNode, testing, eval_proc, [M, F, A], ?PEER_TIMEOUT),

    Out1 =:= Out2.

test_function({FileName, {M,F,A}, Type}, OrigNode, RefacNode, Options, Pid) ->
    Pid ! {FileName, {M, F, A}, proper:quickcheck(?FORALL(Xs, Type, prop_same_output(OrigNode, RefacNode, M, F, Xs)), Options)}.

% Gets a single function and finds the PropEr types for its arguments
% -spec convert_type({mfa(), [type()]}) -> 
  % {atom(), atom(), [proper_types:rich_result(proper_types:fin_type())]}.
convert_type(M, ArgTypes) ->
    lists:map(fun(ArgType) -> get_type({M, ArgType}) end, ArgTypes).

% Convert type string to PropEr type
-spec get_type({atom(), string()}) ->
    proper_types:rich_result(proper_types:fin_type()).
get_type({Module, TypeStr}) ->
    {_, Type} = proper_typeserver:translate_type({Module, TypeStr}),
    Type.
