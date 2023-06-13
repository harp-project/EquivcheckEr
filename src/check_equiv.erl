-module(check_equiv).

-compile(export_all). % Exports all functions
-compile(debug_info).

-type filename()    :: string().
-type commit()      :: string().
-type line()        :: string().
-type source()      :: [line()].
-type ast()         :: erl_syntax:forms().
-type tokens()      :: erl_scan:tokens().
-type file_info()   :: {tokens(), ast()}. % TODO: This is a terrible name

-include_lib("proper/include/proper.hrl").

-define(TEMP_FOLDER, "tmp"). % TODO use /tmp
-define(ORIGINAL_CODE_FOLDER, "orig").
-define(REFACTORED_CODE_FOLDER, "refac").
-define(PEER_TIMEOUT, 1000).

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
    try peer:call(Node, M, F, A, ?PEER_TIMEOUT) of
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

% Gets the list of names for changed files based on the diff, and gives back
% the token list and AST for each
-spec read_sources([filename()], commit()) -> [{filename(), file_info()}].
read_sources(ChangedFiles, CommitHash) ->
    checkout(CommitHash),
    Sources = lists:map(fun utils:read/1, ChangedFiles),
    Tokens = lists:map(fun(Source) -> {_, Tokens, _} = erl_scan:string(Source), Tokens end, Sources),
    ASTs = lists:map(fun(FileName) -> {ok, Forms} = epp_dodger:quick_parse_file(FileName), Forms end, ChangedFiles),

    lists:zip(Tokens, ASTs).

check_equiv(OrigHash, RefacHash) ->
    Configs = config:load_config(),
    typing:ensure_plt(Configs),
    application:start(wrangler), % TODO
    proper_typeserver:start(),
    {_, ProjFolder} = file:get_cwd(),

    copy_project(ProjFolder),
    file:set_cwd(?TEMP_FOLDER),
    checkout(RefacHash), % Scoping needs the repo to be at the commit containing the refactored code

    DiffOutput = os:cmd("git diff -U0 --no-ext-diff " ++ OrigHash ++ " " ++ RefacHash),
    Diffs = diff:diff(DiffOutput),
    Files = diff:get_files(Diffs),

    FileInfos = lists:zip3(Files, read_sources(Files, OrigHash), read_sources(Files, RefacHash)),
    ModifiedFuns = functions:modified_functions(Diffs, FileInfos),

    % Gets back the functions that have to be tested
    FunsToTest = typing:add_types(slicing:scope(ModifiedFuns)),

    % Checkout and compile the necessary modules into two separate folders
    % This is needed because QuickCheck has to evaluate to old and the new
    % function repeatedly side-by-side
    checkout(OrigHash),
    % TODO Files should contain all the used modules, not just the ones affected by the change
    compile(Files, ?ORIGINAL_CODE_FOLDER),

    checkout(RefacHash),
    compile(Files, ?REFACTORED_CODE_FOLDER),

    {OrigNode, RefacNode} = start_nodes(),

    ProperOpts = [long_result, {on_output, fun(X,Y) -> utils:count_tests(X,Y) end}],
    % ProperOpts = [long_result, quiet],

    % A result is a tuple: {Module, Function, Counterexample}
    % If no counterexample is found, the third value is 'true' instead
    Res = lists:map(fun({M, F, Type}) ->
                            {M, F, test_function(M, F, Type, OrigNode, RefacNode, ProperOpts)}
                    end, FunsToTest),

    % Drop the passed results, we need the counterexamples
    FailedFuns = lists:filter(fun({_, _, Eq}) -> Eq =/= true end, Res),

    file:set_cwd(".."),
    cleanup(),
    stop_nodes(OrigNode, RefacNode),
    application:stop(wrangler), % TODO
    proper_typeserver:stop(),
    {Res, FailedFuns}.
