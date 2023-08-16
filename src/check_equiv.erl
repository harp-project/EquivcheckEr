-module(check_equiv).

-include("equivchecker.hrl").

-compile(export_all). % Exports all functions
-compile(debug_info).

-define(TEMP_FOLDER, "tmp"). % TODO use /tmp

-define(ORIGINAL_CODE_FOLDER, ?TEMP_FOLDER ++ "/orig").
-define(REFACTORED_CODE_FOLDER, ?TEMP_FOLDER ++ "/refac").

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
-spec read_sources(filename()) -> {filename(), file_info()}.
read_sources(FileName) ->
    Source = utils:read(FileName),
    {_, Tokens, _} = erl_scan:string(Source),
    {ok, AST} = epp_dodger:quick_parse_file(FileName),

    {Tokens, AST}.

get_typeinfo(Dir) ->
    TyperOut = os:cmd("typer -I include -r " ++ Dir),
    typing:types(TyperOut).

check_equiv(OrigDir, RefacDir) ->
    Configs = config:load_config(),
    % typing:ensure_plt(Configs),
    application:start(wrangler), % TODO
    {_, ProjFolder} = file:get_cwd(),

    DiffOutput = os:cmd("diff -x '.git' -u0 -br " ++ OrigDir ++ " " ++ RefacDir),
    Diffs = diff:diff(DiffOutput),
    ModFiles = diff:modified_files(Diffs),
    % TODO Compile everything for now
    % Files = string:split(string:trim(os:cmd("find -name '*.erl'")), "\n", all),

    FileInfos = lists:map(fun(FileName) ->
                                  {FileName,
                                   read_sources(OrigDir ++ "/" ++ FileName),
                                   read_sources(RefacDir ++ "/" ++ FileName)} end,
                          ModFiles),
    {OrigTypeInfo, RefacTypeInfo} = {get_typeinfo(OrigDir), get_typeinfo(RefacDir)},
    {OrigModFuns, RefacModFuns} = functions:modified_functions(Diffs, FileInfos),

    CallGraph = functions:callgraph(OrigDir, RefacDir),
    Types = typing:add_types(OrigTypeInfo, RefacTypeInfo),

    % Gets back the functions that have to be tested
    FunsToTest = slicing:scope(OrigModFuns, RefacModFuns, CallGraph, Types),

    % Compile the necessary modules into two separate folders
    % This is needed because QuickCheck has to evaluate to old and the new
    % function repeatedly side-by-side

    % TODO Files should contain all the used modules, not just the ones affected by the change
    OrigFiles = lists:map(fun(File) -> filename:join([OrigDir, File]) end, ModFiles),
    RefacFiles = lists:map(fun(File) -> filename:join([RefacDir, File]) end, ModFiles),

    file:make_dir(?TEMP_FOLDER),
    compile(OrigFiles, ?ORIGINAL_CODE_FOLDER),
    compile(RefacFiles, ?REFACTORED_CODE_FOLDER),

    {OrigNode, RefacNode} = start_nodes(),

    % This is needed because PropEr needs the source for constructing the generator
    file:set_cwd(OrigDir),

    Result = test:run_tests(FunsToTest, OrigNode, RefacNode, Types, CallGraph),

    file:set_cwd(".."),
    cleanup(),
    stop_nodes(OrigNode, RefacNode),
    application:stop(wrangler), % TODO
    Result.
