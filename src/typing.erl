%% The purpose of this module is to generate type information using Typer,
%% and provide an interface for querying this information
-module(typing).

-type type()        :: string().
-type type_info()   :: [{module(), [{atom(), arity()}]}].

-export([types/1,
         add_types/2,
         ensure_plt/1]).

-include("dialyzer.hrl").

%% Wrapper for parse_typer
-spec types(string()) -> [{module(), [{string(), arity()}]}] | atom().
types(TyperOutput) ->
    parse_typer(TyperOutput).

%% Returns the list of the types of the arguments
-spec get_type(mfa(), type_info() | atom()) -> [type()].
get_type({_,_,A}, typer_error) ->
    lists:duplicate(A, "any()"); % If the type is not found, use any()
get_type({M,F,A}, TypeInfo) ->
    % TODO Get rid of the nested case blocks
    case lists:keyfind(M, 1, TypeInfo) of
        {_, ModuleTypes} ->
            case lists:keyfind(F, 1, lists:filter(fun({_, Args}) -> length(Args) =:= A end, ModuleTypes)) of
                false  -> lists:duplicate(A, "any()"); % If the type is not found, use any()
                {_, T} -> T
            end;
        false -> lists:duplicate(A, "any()") % If the type is not found, use any()
    end.

% Returns a closure that contains the type information needed
% to type the functions
-spec add_types(type_info(), type_info()) -> fun().
add_types(OrigTypeInfo, RefacTypeInfo) ->
    fun(Fun, Version) ->
            case Version of
                original   -> get_type(Fun, OrigTypeInfo);
                refactored -> get_type(Fun, RefacTypeInfo)
            end
    end.


% Parses the output of typer into a list of tuples in the form of {Filename, [Spec lines]}
-spec parse_typer(string()) -> [type_info()] | atom().
parse_typer(TyperOutput) ->
    case re:run(TyperOutput, ".*failed.*\n") of
        {match, _} -> typer_error; % TODO Why does this match `nomatch`???
        _Otherwise ->
            Files = string:split(string:trim(TyperOutput), "\n\n", all),

            Options = [global, {capture, [1,2], list}, dotall],
            Re = lists:map(fun(FileSpecs) ->
                                   re:run(FileSpecs, ".*File: \"(.*?)\"\n.*---\n(.*)", Options)
                           end, Files),

            Matches = lists:filter(fun(X) -> X =/= nomatch end, Re),

            Specs = lists:map(fun({_, [[File , Specs]]}) ->
                                {File, Specs} end, Matches),

            lists:map(fun({File, SpecLines}) ->
                              {utils:filename_to_module(File),
                               lists:filter(fun(Spec) -> Spec =/= noparse end, % Get rid of invalid specs
                                            lists:map(fun parse_spec/1,
                                                      string:split(SpecLines, "\n", all)))} end,
                      Specs)
    end.

% Given a function spec, gives back the name and input types or noparse in case of invalid input
-spec parse_spec(string()) -> {atom(), [string()]} | atom().
parse_spec(SpecStr) ->
    Options = [global, {capture, [1,2], list}],
    case re:run(SpecStr, "-spec (.*?)\\((.*?)\\) ->.*", Options) of
        {match, [[FunName, ArgsStr]]} -> 
            case ArgsStr of
                []   -> {list_to_atom(FunName), ""}; % Nullary function
                Args -> {list_to_atom(FunName), utils:split_args(Args)}
            end;
        nomatch -> noparse
    end.
        

% PLT (Persistent Lookup Table) related functions

prompt_for_plt() ->
    io:format("PLT not found!~n"),
    io:format("Either provide a valid location for an already existing PLT,~n"),
    io:format("or press enter to generate it!~n"),
    io:get_line("> ").

-spec check_plt() -> atom().
check_plt() ->
    Loc = dialyzer_iplt:get_default_iplt_filename(),
    case dialyzer:plt_info(Loc) of
        {ok,_}     -> found;
        _          -> not_found
    end.

-spec ensure_plt(config:config()) -> none().
ensure_plt(Configs) ->
    case config:lookup(Configs, "custom_plt_location") of
        false     -> ok;
        CustomLoc -> os:putenv("DIALYZER_PLT", string:trim(CustomLoc))
    end,
    case check_plt() of
        found     -> ok;
        not_found ->
            case prompt_for_plt() of
                "\n" -> 
                    io:format("Generating PLT. This could take a while...\n"),
                    Apps = ["erts", "kernel", "stdlib", "mnesia"],
                    Dirs = dialyzer_cl_parse:get_lib_dir(Apps),
                    dialyzer_cl:start(#options{analysis_type = plt_build, files = Dirs, get_warnings = false}),
                    ok;
                Loc  ->
                    os:putenv("DIALYZER_PLT", string:trim(Loc)),
                    NewConfig = config:update_config(Configs, "custom_plt_location", Loc),
                    ensure_plt(NewConfig)
            end
    end,
    config:save_config(Configs).
