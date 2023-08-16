-module(cli).

-export([run/1]).

handler(#{target := Target, source := Source, json := Json, commit := Commit}) ->
    % debugger:quick(check_equiv, check_equiv, [Target, Source]);
    Res = check_equiv:check_equiv(filename:absname(Target), filename:absname(Source)),
    utils:show_result(Res, Json);
handler(#{target := Target, json := Json, commit := Commit}) ->
    io:format("~p~n", [Target]);
handler(#{json := Json, commit := Commit}) ->
    io:format("~p~n", [Json]).

run(Args) ->
    net_kernel:start(master, #{name_domain => shortnames}),
    ets:new(stat, [named_table, protected, set, {keypos, 1}]),
    ets:insert(stat, {counts, []}),
    handler(Args),
    ets:delete(stat).



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
