%% The purpose of this module is to find out the type of the input
%% of each function that's in the scope of the tests, and assign the
%% right PropEr type information to them (so that the right generator can be used)
-module(typing).

-export([add_types/1]).

-type fun_info() :: {atom(), string(), integer()}.

% Gets the list of every function that has to be tested, and
% pairs it with the right PropEr type information for data generation
-spec add_types([fun_info()]) ->
    [{atom(), atom(), [proper_types:rich_result(proper_types:fin_type())]}].
add_types(Funs) ->
    AllSpecs = parse_typer(os:cmd("typer -r .")),
    lists:map(fun(F) -> add_type(F, AllSpecs) end, Funs).

% Gets a single function and finds the types for its arguments
-spec add_type(fun_info(), [{string(), [string()]}]) -> 
  {atom(), atom(), [proper_types:rich_result(proper_types:fin_type())]}.
add_type({Module, F, A}, AllSpecs) ->
    FileName = erlang:atom_to_list(Module) ++ ".erl",
    {_, ModuleSpecs} = lists:keyfind(FileName, 1, AllSpecs),
    Args = get_args(ModuleSpecs, F, A),
    {Module,
     erlang:list_to_atom(F),
     lists:map(fun(Arg) -> get_type({Module, Arg}) end, Args)}.

-spec get_type({atom(), string()}) ->
    proper_types:rich_result(proper_types:fin_type()).
get_type({Module, TypeStr}) ->
    {_, Type} = proper_typeserver:translate_type({Module, TypeStr}),
    Type.

% Parses the output of typer into a list of tuples in the form of {Filename, [Spec lines]}
-spec parse_typer(string()) -> [{string(), [string()]}].
parse_typer(TyperOutput) ->
    Files = string:split(string:trim(TyperOutput), "\n\n", all),

    Options = [global, {capture, [1,2], list}, dotall],
    Matches = lists:map(fun(FileSpecs) ->
                        re:run(FileSpecs, ".*File: \"./(.*?)\"\n.*---\n(.*)", Options) end, Files),
    Specs = lists:map(fun({_, [[File , Specs]]}) ->
                          {File, Specs} end, Matches),

    lists:map(fun({File, SpecLines}) ->
                      {File, string:split(SpecLines, "\n", all)} end, Specs).

% Gets back the list of arguments for given function, using the -specs statements in the source
-spec get_args(string(), string(), integer()) -> [string()].
get_args(SpecStrings, F, A) ->
    Specs = lists:map(fun(X) -> parse_spec(X) end, SpecStrings),
    Funs = lists:search(fun({FunName,Args}) -> (FunName =:= F) and
                                               ((length(Args)) =:= A) end, Specs),
    case Funs of
        {value, {_, ArgList}} -> ArgList;
        false                 -> [] % TODO Handle this case
    end.

% Given a function spec, gives back the name and input types
-spec parse_spec(string()) -> {string(), [string()]}.
parse_spec(SpecStr) ->
    Options = [global, {capture, [1,2], list}],
    {match, [[FunName, ArgsStr]]} = re:run(SpecStr, "-spec (.*?)\\((.*)\\) ->.*", Options),
    
    case ArgsStr of
        []   -> {FunName, ""}; % Nullary function
        Args -> {FunName, string:split(Args, ",", all)}
    end.
