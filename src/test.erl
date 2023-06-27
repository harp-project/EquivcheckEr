-module(test).

-type type() :: string().

-export([run_tests/5]).

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
    FunctionsTyped = lists:map(fun convert_type/1, Functions),

    % A result is a tuple: {Module, Function, Counterexample}
    % If no counterexample is found, the third value is 'true' instead
    Res = lists:map(fun({M, F, Type}) ->
                            {M, F, test_function(M, F, Type, OrigNode, RefacNode, ProperOpts)}
                    end, FunctionsTyped),

    FailedFuns = lists:filter(fun({_, _, Eq}) -> Eq =/= true end, Res),
    FailedMFA = lists:map(fun({M, F, Eq}) -> {M, erlang:atom_to_list(F), length(Eq)} end, FailedFuns),
    Callers = Types(lists:uniq(lists:flatmap(fun(MFA) -> CallGraph(MFA, refactored) end, FailedMFA)), refactored),
    run_tests(Callers, OrigNode, RefacNode, Types, CallGraph, FailedFuns ++ Results).


-spec eval_func(pid(), atom(), atom(), [term()]) -> {atom(), term()}.
eval_func(Node, M, F, A) ->
    try peer:call(Node, M, F, A, ?PEER_TIMEOUT) of
        Val -> {normal, Val}
    catch
        error:Error -> error
    end.

% Spawns a process on each node that evaluates the function and
% sends back the result to this process
-spec prop_same_output(pid(), pid(), atom(), atom(), [term()]) -> boolean().
prop_same_output(OrigNode, RefacNode, M, F, A) ->
    
    Out1 = eval_func(OrigNode, M, F, A),
    Out2 = eval_func(RefacNode, M, F, A),

    Out1 =:= Out2.

test_function(M, F, Type, OrigNode, RefacNode, Options) ->
    proper:quickcheck(?FORALL(Xs, Type, prop_same_output(OrigNode, RefacNode, M, F, Xs)), Options).

% Gets a single function and finds the PropEr types for its arguments
-spec convert_type({mfa(), [type()]}) -> 
  {atom(), atom(), [proper_types:rich_result(proper_types:fin_type())]}.
convert_type({{M, F, _}, ArgTypes}) ->
    {M, erlang:list_to_atom(F), lists:map(fun(ArgType) -> get_type({M, ArgType}) end, ArgTypes)}.

% Convert type string to PropEr type
-spec get_type({atom(), string()}) ->
    proper_types:rich_result(proper_types:fin_type()).
get_type({Module, TypeStr}) ->
    {_, Type} = proper_typeserver:translate_type({Module, TypeStr}),
    Type.
