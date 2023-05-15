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

split_args(SpecStr) -> args(SpecStr, "").

args([], Acc) -> [Acc];
args([H|T], _) when [H] =:= "(" and (T =:= ")") -> "";
args([H|T], Acc) when [H] =:= "{" -> curly(T, "{" ++ Acc, 0);
args([H|T], Acc) when [H] =:= "," -> [Acc|args(T, "")];
args([H|T], Acc) when [H] =:= "[" -> bracket(T, "[" ++ Acc, 0);
args([H|T], Acc) -> args(T, Acc ++ [H]).

curly([H|T], Acc, Level) when [H] =:= "{" -> curly(T, Acc ++ "{", Level + 1);
curly([H|[_|T]], Acc, Level) when ([H] =:= "}") and (Level =:= 0) -> [Acc ++ "}"|args(T, "")];
curly([H|[]], Acc, Level) when ([H] =:= "}") and (Level =:= 0) -> [Acc ++ "}"];
curly([H|T], Acc, Level) when ([H] =:= "}") and (Level =/= 0) -> curly(T, Acc ++ "}", Level - 1);
curly([H|T], Acc, Level) -> curly(T, Acc ++ [H], Level).

bracket([H|T], Acc, Level) when [H] =:= "[" -> bracket(T, Acc ++ "[", Level + 1);
bracket([H|[_|T]], Acc, Level) when ([H] =:= "]") and (Level =:= 0) -> [Acc ++ "]"|args(T, "")];
bracket([H|[]], Acc, Level) when ([H] =:= "]") and (Level =:= 0) -> [Acc ++ "]"];
bracket([H|T], Acc, Level) when ([H] =:= "]") and (Level =/= 0) -> bracket(T, Acc ++ "]", Level - 1);
bracket([H|T], Acc, Level) -> bracket(T, Acc ++ [H], Level).
