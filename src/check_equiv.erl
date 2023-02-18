-module(check_equiv).

-compile(export_all). % Exports all functions
-compile(debug_info).

-include_lib("proper/include/proper.hrl").

-define(TEMP_FOLDER, "tmp"). % TODO use /tmp
-define(ORIGINAL_CODE_FOLDER, "orig").
-define(REFACTORED_CODE_FOLDER, "refac").
-define(PLT_DEFAULT_LOC, "/erlang/.dialyzer_plt").

-spec copy_project(string()) -> string().
copy_project(ProjFolder) ->
    file:make_dir(?TEMP_FOLDER),
    os:cmd("git clone " ++ ProjFolder ++ " " ++ ?TEMP_FOLDER).

-spec checkout(string()) -> string().
checkout(Hash) ->
    os:cmd("git checkout " ++ Hash).

cleanup() ->
    % TODO Handle error
    file:del_dir_r(?TEMP_FOLDER).

compile(Modules, DirName) ->
    % TODO Handle error
    file:make_dir(DirName),
    lists:map(fun(X) -> compile:file(X, [{outdir, DirName}, {warn_format, 0}]) end, Modules).

show_result({Res, Failed}) ->
    NumOfFail = length(Failed),
    NumOfSuccess = length(Res),
    io:format("Results: ~p~n", [Failed]),
    io:format("~p failed out of ~p~n", [NumOfFail, NumOfSuccess]).

start_nodes() ->
    % TODO Handle error, use other port if its already used
    {_, Orig, _} = peer:start(#{name => orig, connection => 33001, args => ["-pa", ?ORIGINAL_CODE_FOLDER]}),
    {_, Refac, _} = peer:start(#{name => refac, connection => 33002, args => ["-pa", ?REFACTORED_CODE_FOLDER]}),
    {Orig, Refac}.

stop_nodes(Orig, Refac) ->
    peer:stop(Orig),
    peer:stop(Refac).

-spec eval_func(pid(), atom(), atom(), [term()]) -> {atom(), term()}.
eval_func(Node, M, F, A) ->
    try peer:call(Node, M, F, A) of
        Val -> {normal, Val}
    catch
        error:Error -> Error
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

    % 1. Check if we have already generated it
    % 2. Check if DIALYZER_PLT is set in the env
    % 3. Check if there is a PLT in the default location (what if there is a very small PLT there?)


check_plt() ->
    % TODO Check the config file for custom location
    case os:getenv("DIALYZER_PLT") of
        false ->
            CacheDir = os:getenv("XDG_CACHE_HOME"), % TODO Not every distro supports it
            case filelib:is_file(CacheDir ++ ?PLT_DEFAULT_LOC) of
                true  -> found;
                false -> not_found
            end;
        _Otherwise -> found
    end.

prompt_for_plt() ->
    io:format("PLT not found at default locations.~n"),
    io:format("Either provide a valid location for an already existing PLT,~n"),
    io:format("or press enter to generate it!~n"),
    io:get_line("> ").

ensure_plt() ->
    case check_plt() of
        found     -> ok;
        not_found ->
            case prompt_for_plt() of
                "\n" -> 
                    io:format("Generating PLT. This could take a while..."),
                    os:cmd("dialyzer --build_plt --apps erts kernel stdlib mnesia");
                Loc  ->
                    os:putenv("DIALYZER_PLT", string:trim(Loc)), % TODO This has to be save into the config file
                    DialyzerOutput = os:cmd("dialyzer --check_plt"),
                    case re:run(DialyzerOutput, ".*Could not find the PLT.*") of
                        nomatch   -> ok;
                        {match, _} -> prompt_for_plt()
                    end
            end
    end.
    
check_equiv(OrigHash, RefacHash) ->
    ensure_plt(),
    application:start(wrangler), % TODO
    proper_typeserver:start(),
    {_, ProjFolder} = file:get_cwd(),

    copy_project(ProjFolder),
    file:set_cwd(?TEMP_FOLDER),
    checkout(RefacHash), % Scoping needs the repo to be at the commit containing the refactored code

    Diff_Output = os:cmd("git diff --no-ext-diff " ++ OrigHash ++ " " ++ RefacHash),

    % Gets back the files that have to be compiled, and the functions that have to be tested
    {Files, Funs} = scoping:scope(Diff_Output),

    % Checkout and compile the necessary modules into two separate folders
    % This is needed because QuickCheck has to evaluate to old and the new
    % function repeatedly side-by-side
    checkout(OrigHash),
    compile(Files, ?ORIGINAL_CODE_FOLDER),

    checkout(RefacHash),
    compile(Files, ?REFACTORED_CODE_FOLDER),

    {OrigNode, RefacNode} = start_nodes(),

    Options = [quiet, long_result],

    % A result is a tuple: {Module, Function, Counterexample}
    % If no counterexample is found, the third value is 'true' instead
    Res = lists:map(fun({M, F, Type}) ->
                            {M, F, test_function(M, F, Type, OrigNode, RefacNode, Options)}
                    end, Funs),

    % Drop the passed results, we need the counterexamples
    FailedFuns = lists:filter(fun({_, _, Eq}) -> Eq =/= true end, Res),

    file:set_cwd(".."),
    cleanup(),
    stop_nodes(OrigNode, RefacNode),
    application:stop(wrangler), % TODO
    proper_typeserver:stop(),
    {Res, FailedFuns}.
