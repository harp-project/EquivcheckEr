-module(check_equiv).

-compile(export_all). % Exports all functions
-compile(debug_info).

-define(TEMP_FOLDER, "tmp"). % TODO use /tmp

-type filename()    :: string().
-type commit()      :: string().
-type ast()         :: erl_syntax:forms().
-type tokens()      :: erl_scan:tokens().
-type file_info()   :: {tokens(), ast()}. % TODO: This is a terrible name

-define(ORIGINAL_CODE_FOLDER, "orig").
-define(REFACTORED_CODE_FOLDER, "refac").

cleanup() ->
    % TODO Handle error
    file:del_dir_r(?TEMP_FOLDER).

compile(Modules, DirName) ->
    % TODO Handle error
    file:make_dir(DirName),
    lists:map(fun(X) -> compile:file(X, [export_all, {outdir, DirName}, {warn_format, 0}]) end, Modules).

start_nodes() ->
    % TODO Handle error, use other port if its already used
    {_, Orig, _} = peer:start(#{name => orig, connection => 33001, args => ["-pa", ?ORIGINAL_CODE_FOLDER]}),
    {_, Refac, _} = peer:start(#{name => refac, connection => 33002, args => ["-pa", ?REFACTORED_CODE_FOLDER]}),
    {Orig, Refac}.

stop_nodes(Orig, Refac) ->
    peer:stop(Orig),
    peer:stop(Refac).

% Gets the list of names for changed files based on the diff, and gives back
% the token list and AST for each
-spec read_sources([filename()], commit()) -> [{filename(), file_info()}].
read_sources(ChangedFiles, CommitHash) ->
    repo:checkout(CommitHash),
    Sources = lists:map(fun utils:read/1, ChangedFiles),
    Tokens = lists:map(fun(Source) -> {_, Tokens, _} = erl_scan:string(Source), Tokens end, Sources),
    ASTs = lists:map(fun(FileName) -> {ok, Forms} = epp_dodger:quick_parse_file(FileName), Forms end, ChangedFiles),

    lists:zip(Tokens, ASTs).

get_typeinfo(OrigHash, RefacHash) ->
    repo:checkout(OrigHash),
    TyperOut = os:cmd("typer -I include -r ."),
    OrigTypes = typing:types(TyperOut),

    repo:checkout(RefacHash),
    TyperOut2 = os:cmd("typer -I include -r ."),
    RefacTypes = typing:types(TyperOut2),

    {OrigTypes, RefacTypes}.

check_equiv(OrigHash, RefacHash) ->
    Configs = config:load_config(),
    % typing:ensure_plt(Configs),
    application:start(wrangler), % TODO
    {_, ProjFolder} = file:get_cwd(),

    repo:copy(ProjFolder, ?TEMP_FOLDER),
    file:set_cwd(?TEMP_FOLDER),
    repo:checkout(RefacHash), % Scoping needs the repo to be at the commit containing the refactored code

    DiffOutput = repo:diff_output(OrigHash, RefacHash),
    Diffs = diff:diff(DiffOutput),
    Files = diff:modified_files(Diffs),
    % TODO Compile everything for now
    % Files = string:split(string:trim(os:cmd("find -name '*.erl'")), "\n", all),

    FileInfos = lists:zip3(Files, read_sources(Files, OrigHash), read_sources(Files, RefacHash)),
    {OrigTypeInfo, RefacTypeInfo} = get_typeinfo(OrigHash, RefacHash),
    {OrigModFuns, RefacModFuns} = functions:modified_functions(Diffs, FileInfos),

    CallGraph = functions:callgraph(OrigHash, RefacHash),
    Types = typing:add_types(OrigTypeInfo, RefacTypeInfo),

    % Gets back the functions that have to be tested
    FunsToTest = slicing:scope(OrigModFuns, RefacModFuns, CallGraph, Types),

    % Checkout and compile the necessary modules into two separate folders
    % This is needed because QuickCheck has to evaluate to old and the new
    % function repeatedly side-by-side
    repo:checkout(OrigHash),
    % TODO Files should contain all the used modules, not just the ones affected by the change
    compile(Files, ?ORIGINAL_CODE_FOLDER),

    repo:checkout(RefacHash),
    compile(Files, ?REFACTORED_CODE_FOLDER),

    {OrigNode, RefacNode} = start_nodes(),

    Result = testing:run_tests(FunsToTest, OrigNode, RefacNode, Types, CallGraph),
    file:set_cwd(".."),
    cleanup(),
    stop_nodes(OrigNode, RefacNode),
    application:stop(wrangler), % TODO
    Result.
