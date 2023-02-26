-module(utils).
-compile(export_all). % Exports all functions
-compile(debug_info).

show_result({_, Failed}) ->
    io:format("Results: ~p~n", [Failed]).

count_tests("Failed: After ~b test(s).~n", Args) ->
    [{_, Counts}] = ets:lookup(stat, counts),
    ets:insert(stat, {counts, Counts ++ Args});
count_tests(_, _) ->
    ok.

statistics() ->
    [{_, FailCounts}] = ets:lookup(stat, counts),
    % io:format("~p~n", [FailCounts]),
    Average = lists:sum(FailCounts) / length(FailCounts),
    io:format("Average no. tries before counterexample is found: ~p~n", [Average]).

bench(Count) ->
    run(Count) / Count.

run(0) -> 0;
run(Count) -> element(1, timer:tc(vsc_equiv, check_equiv, ["468f49", "0f07c3a", f_old, f_new])) + run(Count-1).
