-module(cli).

-include("equivchecker.hrl").

-export([run/1]).

% TODO Refactor this monstrosity into something manageable
-spec handler(map()) -> none().
handler(#{target := Target, source := Source, json := Json, commit := Commit, stats := Stats}) when Commit ->
    if not Json -> io:format("Checking commit ~p against commit ~p~n", [Target, Source]); true -> ok end,
    {ok, ProjFolder} = file:get_cwd(),
    OrigRepo = repo:copy(ProjFolder, ?ORIGINAL_SOURCE_FOLDER),
    repo:checkout(OrigRepo, Target),
    RefacRepo = repo:copy(ProjFolder, ?REFACTORED_SOURCE_FOLDER),
    repo:checkout(RefacRepo, Source),
    Res = check_equiv:check_equiv(filename:absname(OrigRepo), filename:absname(RefacRepo)),
    show_result(Res, Json, Stats);
handler(#{target := Target, source := Source, json := Json, commit := Commit, stats := Stats}) when not Commit ->
    if not Json -> io:format("Checking folder ~p against folder ~p~n", [Target, Source]); true -> ok end,
    Res = check_equiv:check_equiv(filename:absname(Target), filename:absname(Source)),
    show_result(Res, Json, Stats);
handler(#{target := Target, json := Json, commit := Commit, stats := Stats}) when Commit ->
    if not Json -> io:format("Checking current folder against commit ~p~n", [Target]); true -> ok end,
    {ok, ProjFolder} = file:get_cwd(),
    Repo = repo:copy(ProjFolder, ?ORIGINAL_SOURCE_FOLDER),
    repo:checkout(Repo, Target),
    Res = check_equiv:check_equiv(filename:absname(ProjFolder), filename:absname(Repo)),
    show_result(Res, Json, Stats);
handler(#{target := Target, json := Json, commit := Commit, stats := Stats}) when not Commit ->
    if not Json -> io:format("Checking current folder against ~p~n", [Target]); true -> ok end,
    {ok, ProjFolder} = file:get_cwd(),
    Res = check_equiv:check_equiv(filename:absname(ProjFolder), filename:absname(Target)),
    show_result(Res, Json, Stats);
handler(#{json := Json, commit := _, stats := Stats}) ->
    if not Json -> io:format("Checking current folder against current commit~n"); true -> ok end,
    {ok, ProjFolder} = file:get_cwd(),
    Commit = repo:current_commit(),
    Repo = repo:copy(ProjFolder, ?ORIGINAL_SOURCE_FOLDER),
    repo:checkout(Repo, Commit),
    Res = check_equiv:check_equiv(filename:absname(ProjFolder), filename:absname(Repo)),
    show_result(Res, Json, Stats).

setup() ->
    % Sets the name of the master node
    net_kernel:start(master, #{name_domain => shortnames}),

    % These are needed for storing statistics
    ets:new(stat, [named_table, public, set, {keypos, 1}]),
    ets:insert(stat, {counts, []}),

    % Creating temporary folders for the source and bytecode
    file:make_dir(?TEMP_FOLDER),
    file:make_dir(?ORIGINAL_BIN_FOLDER),
    file:make_dir(?REFACTORED_BIN_FOLDER),

    application:start(wrangler).

cleanup() ->
    ets:delete(stat),
    file:del_dir_r(?TEMP_FOLDER), % TODO Handle error
    application:stop(wrangler).

-spec run(map()) -> none().
run(Args) ->
    setup(),
    handler(Args),
    cleanup().

% Second arg turns on json output, third shows statistics
-spec show_result([{filename(), mfa(), [any()]}], boolean(), boolean()) -> none().
show_result(Result, false, false) ->
    Formatted = format_results(Result,false),
    io:format("Results: ~p~n", [Formatted]);
show_result(Result, false, true) ->
    {FailCounts, Average} = utils:statistics(),
    Formatted = format_results(Result,false),
    io:format("Results: ~p~n", [Formatted]),
    io:format("Number of functions that failed: ~p~n", [length(FailCounts)]),
    io:format("Average no. tries before counterexample is found: ~p~n", [Average]);
show_result(Result, true, false) ->
    Formatted = format_results(Result,true),
    io:format("~s\n", [jsone:encode(Formatted,[{indent, 2}, {space, 1}])]);
show_result(Result, true, true) ->
    {FailCounts, Average} = utils:statistics(),
    Stats = #{failed_count => length(FailCounts), average_test_count => Average},
    Output = #{statistics => Stats, results => format_results(Result,true)},
    io:format("~s\n", [jsone:encode(Output,[{indent, 2}, {space, 1}])]).

-spec format_results([{filename(), mfa(), [any()]}], boolean()) -> none().
format_results(Results, Json) ->
    case Json of
        true  -> lists:map(fun format_json/1, Results);
        false -> lists:map(fun format_stdout/1, Results)
    end.

-spec format_stdout({filename(), mfa(), [any()]}) -> none().
format_stdout({FileName, MFA, [CounterExample]}) ->
    {FileName, MFA, CounterExample}.

-spec format_json({filename(), mfa(), [any()]}) -> none().
format_json({FileName, MFA, [CounterExample]}) ->
    #{filename => erlang:list_to_atom(FileName), mfa => MFA, counterexample => CounterExample}.
