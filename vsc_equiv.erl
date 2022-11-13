-module(vsc_equiv).
-compile(export_all). % Exports all functions
-compile(debug_info).

-import(lists,[map/2,uniq/1]).
 
-spec evaluate_expression(string) -> any().
evaluate_expression(Expression) ->
    {ok, Tokens, _} = erl_scan:string(Expression),    % scan the code into tokens
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),     % parse the tokens into an abstract form
    {value, Result, _} = erl_eval:exprs(Parsed, []),  % evaluate the expression, return the value
    Result.

% Probably we should use /tmp to store this

% 1. Create tmp dir and copy/symlink the .git folder (TODO libgit bindings instead of CLI)
% 2. Checkout the original code                      (TODO only checkout files that are necessary)
% 3. Run the tests (compile, run)                    (TODO compile only the necessary modules, for now I assumed that the functions don't use other modules)
% 4. Checkout the refactored code
% 5. Run the tests
% 6. Compare test results: (TODO result should be a counterexample if it fails)
% 7. Delete tmp


% Testing:
% We know which modules we need to compile

recreate_project(ProjFolder) ->
    % TODO tmp should be .tmp
    % {_, Dir} = file:get_cwd(),
    file:make_dir("tmp"),
    % file:make_symlink(Dir ++ "/.git", Dir ++ "/tmp/.git").
    os:cmd("git clone " ++ ProjFolder ++ " tmp").

checkout(Hash) ->
    os:cmd("git checkout " ++ Hash).

cleanup() ->
    file:del_dir_r("tmp").

% 1. Check where was the function used (assume we know the parameters)
% 2. Compile these modules (assume the function don't call anything from other modules)
% 3. Call each function with TestData
% 4. Return the list of results
run_tests(FunName, TestData) ->
    % TODO Give back the results?
    % But is QuickCheck deterministic? (I think not)
    {_, Funs} = wrangler_code_inspector_lib:calls_to_fun_1("test.erl", FunName, 1, ["/home/obabo/Projects/active/Labor/test"], 4),
    Modules = uniq(map(fun(X) -> element(1, element(1, X)) end, Funs)),
    Names = map(fun(X) -> element(2, element(1, X)) end, Funs),
    map(fun(X) -> compile:file(X) end, Modules),
    map(fun(X) -> test_fun(test, X, TestData) end, Names). % TODO module name is hardcoded for now

test_fun(ModuleName, Fun, TestData) -> 
    map(fun(X) -> erlang:apply(ModuleName, Fun, [X]) end, TestData).

compare_results(R1, R2) ->
    R1 == R2.

check_equiv(OrigHash, RefacHash, OrigName, RefacName) ->
    application:start(wrangler), % TODO
    {_, ProjFolder} = file:get_cwd(),
    debugger:start(),
    recreate_project(ProjFolder),
    file:set_cwd("tmp"),
    checkout(OrigHash),
    TestData = lists:map(fun(X) -> rand:uniform(X) end, lists:duplicate(10,100)),
    R1 = run_tests(OrigName, TestData),
    checkout(RefacHash),
    R2 = run_tests(RefacName, TestData),
    file:set_cwd(".."),
    cleanup(),
    compare_results(R1, R2),
    application:stop(wrangler). % TODO

demo() ->
    debugger:quick(vsc_equiv,check_equiv,["79207c7c3","1b0168",f_old,f_new]).

% forall(X, int(), test_old(X) == test_new(X))
% test_old(data):
%   checkout
%   run_test()
%
% generate list of numbers / generate single numbers
