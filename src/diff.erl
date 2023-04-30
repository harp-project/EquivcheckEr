%% This module is for extracting information from the diff, and presenting it
%% The main unit of the differences between two versions is a hunk, which contains
%% all the information about a change that is needed for finding out what has to be testsd
-module(diff).

-export([diff/2]).

-type filename() :: string().
-type diff_line() :: string().
-type fun_info() :: {atom(), string(), integer()}.
-type line_number() :: integer().
-type hunk_info() :: {[diff_line()], fun_info(), {line_number(), integer()}}.
-type hunk() :: {filename(), {hunk_info(), hunk_info()}}.
-type source() :: [string()].


% Gets the diff output, and splits it into hunks
parse_diff(DiffStr) ->
    [_|Files] = string:split(DiffStr, "diff --git ", all),
    Lines = lists:map(fun(Str) -> string:split(Str, "\n", all) end, Files),
    LinesByFiles = lists:map(fun([H,_,_,_|T]) -> {extract_file(H), lists:droplast(T)} end, Lines),
    lists:map(fun({FileName, DiffLines}) ->
                                     {FileName,
                                      tl(lists:foldr(fun add/2, [[]], DiffLines))} end,
                             LinesByFiles).

% Used for separating the diff output into individual hunks
-spec add(string(), [string()]) -> [[string()]].
add(Line, [LastHunk|Hunks]) ->
    case string:prefix(Line, "@@") of
        nomatch     -> [lists:append([Line], LastHunk)|Hunks];
        _Otherwise  -> [[], lists:append([Line], LastHunk)|Hunks]
    end.

% Extracts the filename from the diff output
-spec extract_file(string()) -> string().
extract_file(DiffLine) ->
    Options = [global, {capture, [1], list}],
    {match, [[FileName]]} = re:run(DiffLine,".*/(.*\.erl).*", Options),
    FileName.

-spec is_function_sig(string()) -> boolean().
is_function_sig(Line) ->
    % Regexp for finding top-level function definitions
    case re:run(Line, "^[^-[:space:]%].*\\(.*?\\).*", []) of
        nomatch -> false;
        _       -> true
    end.

% Parses a function definition, and gives back its name and arity
-spec get_name_and_arity(string()) -> {string(), integer()}.
get_name_and_arity(FunStr) ->
    Options = [global, {capture, [1], list}],
    {match, [[Fun]]} = re:run(FunStr, "^(.*?\\(.*?\\)).*", Options),
    {ok, Toks, _} = erl_scan:string(Fun ++ "."),
    {ok, [{call, _, {atom, _, Name}, Args}]} = erl_parse:parse_exprs(Toks),
    {atom_to_list(Name), length(Args)}.


%%% Accessors %%%

get_funcs({_, {_, OrigFun, _}, {_, RefacFun, _}}) ->
    {OrigFun, RefacFun}. 


-spec hunks_by_file(filename(), [hunk()]) -> [hunk()].
hunks_by_file(FileName, Hunks) ->
    % TODO: this can be false
    lists:filter(fun({HunkFileName, _, _}) -> FileName =:= HunkFileName end, Hunks).

%%% Construct Hunks %%%

diff(DiffStr, Sources) ->
    HunkLinesByFile = parse_diff(DiffStr),
    lists:flatmap(fun({FileName,Hunks}) ->
                          {_, Source} = lists:keyfind(FileName, 1, Sources),
                          lists:map(fun(Hunk) -> hunk(Hunk, FileName, Source) end, Hunks) end,
                  HunkLinesByFile).

get_lines(Prefix, Lines) ->
    lists:map(fun erlang:tl/1,
              lists:filter(fun([FstChar|_]) -> [FstChar] =:= Prefix end, Lines)).

has_func_sig(Lines) ->
    lists:foldr(fun(Line, Bool) -> is_function_sig(Line) or Bool end,
                false,
                Lines).

line_numbers(OrigStart, OrigLen, RefacStart, RefacLen) ->
    OS = erlang:list_to_integer(OrigStart),
    RS = erlang:list_to_integer(RefacStart),
    case {OrigLen, RefacLen} of
        {[], []}   -> {OS, 0, RS, 0};
        {[], _}    -> {OS, 0, RS, erlang:list_to_integer(RefacLen)};
        {_, []}    -> {OS, erlang:list_to_integer(OrigLen), RS, 0};
        _          -> {OS, erlang:list_to_integer(OrigLen), RS, erlang:list_to_integer(RefacLen)}
    end.
    
added_lines(M, OS, RS, Lines, {OrigSource, RefacSource}) ->
    case has_func_sig(Lines) of
        true  -> {RF, RA} = find_function(RefacSource, RS), %% New function added
                 {{}, {M, RF, RA}};
        false -> modified_lines(M, OS, RS, {RefacSource, OrigSource}) %% Modified existing function
    end.

removed_lines(M, OS, RS, Lines, {OrigSource, RefacSource}) ->
    case has_func_sig(Lines) of
        true  -> {OF, OA} = find_function(OrigSource, OS), %% Function removed
                 {{M, OF, OA}, {}};
        false -> modified_lines(M, OS, RS, {RefacSource, OrigSource}) %% Modified existing function
    end.

modified_lines(M, OS, RS, {OrigSource, RefacSource}) ->
    {OF, OA} = find_function(OrigSource, OS),
    {RF, RA} = find_function(RefacSource, RS),
    {{M, OF, OA}, {M, RF, RA}}.

-spec hunk([string()], filename(), {source(), source()}) -> hunk().
hunk([Header|DiffLines], FileName, Sources) ->
    Options = [{capture, [1,2,3,4], list}],
    % Captures the line numbers and optionally the length for multiline hunks
    Pattern = "@@ -(\\d+),?(\\d?).*\\+(\\d+),?(\\d?).*",
    {match, [OrigStart, OrigLen, RefacStart, RefacLen]} = re:run(Header, Pattern, Options),
    {OS, OL, RS, RL} = line_numbers(OrigStart, OrigLen, RefacStart, RefacLen),
    M = erlang:list_to_atom(filename:rootname(FileName, ".erl")),
    OrigLines = get_lines("-", DiffLines),
    RefacLines = get_lines("+", DiffLines),
    case {OrigLines, RefacLines} of
        {[], _} -> {OrigFun, RefacFun} = added_lines(M, OS, RS, RefacLines, Sources);
        {_, []} -> {OrigFun, RefacFun} = removed_lines(M, OS, RS, OrigLines, Sources);
        _       -> {OrigFun, RefacFun} = modified_lines(M, OS, RS, Sources)
    end,
    {FileName, {OrigLines, OrigFun, {OS, OL}}, {RefacLines, RefacFun, {RS, RL}}}.
    
find_function(Source, LineNum) ->
    Signature = lists:dropwhile(fun(Line) -> not(is_function_sig(Line)) end,
                    lists:reverse(lists:sublist(Source, LineNum))),
    case Signature of
        []         -> {"", ""}; % TODO: Not a diff in a function, mostly export list elements
        _Otherwise -> get_name_and_arity(hd(Signature)) % TODO: Rename the signature, it's a list of lines now
    end.
