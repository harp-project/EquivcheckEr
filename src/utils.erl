-module(utils).

show_result(Failed) ->
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

read(FileName) ->
    {ok, F} = file:read_file(FileName),
    erlang:binary_to_list(F).

% Splits up a list containing an even number of items into groups of two
group_by_two([]) -> [];
group_by_two([H1,H2|T]) -> [[H1,H2]|group_by_two(T)].

% Split up arguments passed as a string into a list of the individual arguments
split_args(SpecStr) -> args(SpecStr, "").

args([], Acc) -> [Acc];
args([H|T], _) when [H] =:= "(" and (T =:= ")") -> "";
args([H|T], Acc) when [H] =:= "(" -> paren(T, Acc ++ "(", 0);
args([H|T], Acc) when [H] =:= "{" -> curly(T, "{" ++ Acc, 0);
args([H|T], Acc) when [H] =:= "," -> [Acc|args(T, "")];
args([H|T], Acc) when [H] =:= "[" -> bracket(T, "[" ++ Acc, 0);
args([H|T], Acc) -> args(T, Acc ++ [H]).

paren([H|T], Acc, Level) when [H] =:= "(" -> paren(T, Acc ++ "(", Level + 1);
paren([H|[_|T]], Acc, Level) when ([H] =:= ")") and (Level =:= 0) -> args(T, Acc ++ ") ");
paren([H|[]], Acc, Level) when ([H] =:= ")") and (Level =:= 0) -> [Acc ++ ")"];
paren([H|T], Acc, Level) when ([H] =:= ")") and (Level =/= 0) -> paren(T, Acc ++ ")", Level - 1);
paren([H|T], Acc, Level) -> paren(T, Acc ++ [H], Level).

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

-spec filename_to_module(string()) -> atom().
filename_to_module(FileName) ->
    erlang:list_to_atom(filename:basename(FileName, ".erl")).

-spec module_to_filename(atom()) -> string().
module_to_filename(Module) ->
    erlang:atom_to_list(Module) ++ ".erl".
