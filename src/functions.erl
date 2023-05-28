-module(functions).
-compile(export_all).

-type source() :: [string()].
-type filename() :: string().
-type fun_info() :: {atom(), string(), integer()}.
-type line_info() :: {integer(), integer()}.
-type boundaries() :: {fun_info(), line_info()}.


test() ->
    {ok, F} = file:read_file("diff.erl"),
    Source = string:split(erlang:binary_to_list(F), "\n", all),
    boundaries({"diff.erl", {Source, Source}}).

-spec boundaries({filename(), {source(), source()}}) -> [{boundaries(), boundaries()}].
boundaries({FileName, {OrigLines, RefacLines}}) ->
    OrigFuns = functions(FileName, OrigLines),
    RefacFuns = functions(FileName, RefacLines),
    lists:zip(OrigFuns, RefacFuns).

add2({CurrNum, CurrSig}, {NextStart, _, _}) ->
    [{CurrNum, NextStart - 1, CurrSig}];
add2({CurrNum, CurrSig}, [{NextStart, NextEnd, NextSig}|T]) ->
    [{CurrNum, NextStart - 1, CurrSig}, {NextStart, NextEnd, NextSig}|T].


-spec functions(filename(), source()) -> boundaries().
functions(FileName, Lines) ->
    LineNums = lists:seq(1, length(Lines)),
    IndLines = lists:zip(LineNums, Lines),
    Signatures = lists:filter(fun({_, Line}) -> diff:is_function_sig(Line) end, IndLines),
    lists:map(fun({StartLine, EndLine, Line}) -> function(FileName, StartLine, EndLine, Line) end,
              lists:foldr(fun add2/2, {length(Lines) + 1, 0, ""}, Signatures)).


function(FileName, StartLine, EndLine, Line) ->
    M = slicing:get_module(FileName),
    {F, A} = diff:get_name_and_arity(Line),
    {{M, F, A}, {StartLine, EndLine}}.

add({dot,Line}, [H|T]) ->
    [[],[{dot, Line}|H]|T];
add(Token, [H|T]) ->
    [[Token|H]|T].
