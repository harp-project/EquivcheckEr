-module(cli).

-include("equivchecker.hrl").

-export([run/1]).

handler(#{target := Target, source := Source, json := Json, commit := Commit}) when Commit ->
    io:format("Checking commit ~p against commit ~p~n", [Target, Source]),
    {ok, ProjFolder} = file:get_cwd(),
    OrigRepo = repo:copy(ProjFolder, ?ORIGINAL_SOURCE_FOLDER),
    repo:checkout(OrigRepo, Source),
    RefacRepo = repo:copy(ProjFolder, ?REFACTORED_SOURCE_FOLDER),
    repo:checkout(RefacRepo, Target),
    Res = check_equiv:check_equiv(filename:absname(OrigRepo), filename:absname(RefacRepo)),
    utils:show_result(Res, Json);
handler(#{target := Target, source := Source, json := Json, commit := Commit}) when not Commit ->
    io:format("Checking folder ~p against folder ~p~n", [Target, Source]),
    Res = check_equiv:check_equiv(filename:absname(Target), filename:absname(Source)),
    utils:show_result(Res, Json);
handler(#{target := Target, json := Json, commit := Commit}) when Commit ->
    io:format("Checking current folder against commit ~p~n", [Target]),
    {ok, ProjFolder} = file:get_cwd(),
    Repo = repo:copy(ProjFolder, ?ORIGINAL_SOURCE_FOLDER),
    repo:checkout(Repo, Target),
    Res = check_equiv:check_equiv(filename:absname(ProjFolder), filename:absname(Repo)),
    utils:show_result(Res, Json);
handler(#{target := Target, json := Json, commit := Commit}) when not Commit ->
    io:format("Checking current folder against ~p~n", [Target]),
    {ok, ProjFolder} = file:get_cwd(),
    Res = check_equiv:check_equiv(filename:absname(ProjFolder), filename:absname(Target)),
    utils:show_result(Res, Json);
handler(#{json := Json, commit := _}) ->
    io:format("Checking current folder against current commit~n"),
    {ok, ProjFolder} = file:get_cwd(),
    Commit = repo:current_commit(),
    Repo = repo:copy(ProjFolder, ?ORIGINAL_SOURCE_FOLDER),
    repo:checkout(Repo, Commit),
    Res = check_equiv:check_equiv(filename:absname(ProjFolder), filename:absname(Repo)),
    utils:show_result(Res, Json).

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
    % file:del_dir_r(?TEMP_FOLDER), % TODO Handle error
    application:stop(wrangler).

run(Args) ->
    setup(),
    handler(Args),
    cleanup().



% fun (#{help := Help, mode := Mode, json := Json}) ->
%         case Mode of
%             "db" ->
%                 debugger:quick(check_equiv, check_equiv, ["master^", "master"]);
%             "stats" ->
%                 {Res, Failed} = check_equiv:check_equiv("master^", "master"),
%                 utils:statistics(),
%                 NumOfFail = length(Failed),
%                 NumOfSuccess = length(Res),
%                 io:format("~p failed out of ~p~n", [NumOfFail, NumOfSuccess]);
%             "none" ->
%                 Res = check_equiv:check_equiv("master^", "master"),
%                 utils:show_result(Res, Json)
%         end,
% end
