-module(check_equiv).
-compile(export_all). % Exports all functions
-compile(debug_info).

-include_lib("proper/include/proper.hrl").

copy_project(ProjFolder) ->
    file:make_dir("tmp"), % TODO use /tmp
    os:cmd("git clone " ++ ProjFolder ++ " tmp").

checkout(Hash) ->
    os:cmd("git checkout " ++ Hash).

cleanup() ->
    file:del_dir_r("tmp").

comp(Modules, DirName) ->
    file:make_dir(DirName),
    lists:map(fun(X) -> compile:file(X, [{outdir, DirName}, {warn_format, 0}]) end, Modules).

show_result(Res) ->
    io:format("Results: ~p~n", [Res]).

get_module(FileName) ->
    erlang:list_to_atom(hd(string:split(lists:last(string:split(FileName,"/",all)),"."))).

start_nodes() ->
    {_, Orig, _} = peer:start(#{name => orig, connection => 33001, args => ["-pa", "orig"]}),
    {_, Refac, _} = peer:start(#{name => refac, connection => 33002, args => ["-pa", "refac"]}),
    {Orig, Refac}.

stop_nodes(Orig, Refac) ->
    peer:stop(Orig),
    peer:stop(Refac).

prop_same_output(OrigNode, RefacNode, M, F, A) ->
    % Spawns a process on each node that evaluates the function and
    % sends back the result to this process
    
    % TODO Try peer:cast function
    Out1 = peer:call(OrigNode, M, F, A),
    Out2 = peer:call(RefacNode, M, F, A),

    Out1 =:= Out2.

check_equiv(OrigHash, RefacHash) ->
    application:start(wrangler), % TODO
    {_, ProjFolder} = file:get_cwd(),

    copy_project(ProjFolder),
    file:set_cwd("tmp"),

    {ChangedFile, {OrigFun, RefacFun}, Arity} = general_refac:diff_renaming(OrigHash, RefacHash),
    Callers = general_refac:find_callers({RefacFun, Arity}),

    CallerFiles = lists:map(fun({FileName, _, _}) -> FileName end, Callers),

    Modules = lists:uniq([ChangedFile|CallerFiles]),

    % Checkout and compile the necessary modules into two separate folders
    % This is needed because QuickCheck has to evaluate to old and the new
    % function repeatedly side-by-side
    checkout(OrigHash),
    comp(Modules, "orig"),

    checkout(RefacHash),
    comp(Modules, "refac"),

    {OrigNode, RefacNode} = start_nodes(),

    % Contains all the functions that call the renamed one {Module, Function, PropEr Type}
    Funs = lists:map(fun({FileName, F, A}) -> {get_module(FileName), erlang:list_to_atom(F), typing:get_type(hd(general_refac:get_args(FileName, F, A)))} end, Callers),

    lists:map(fun({FileName, F, A}) -> general_refac:get_args(FileName, F, A) end, Callers),

    Options = [quiet, long_result],
    Res = lists:filter(fun({_, _, Eq}) -> Eq =/= true end, lists:map(fun({M, F, Type}) -> {M, F, proper:quickcheck(?FORALL(X, Type, prop_same_output(OrigNode, RefacNode, M, F, [X])), Options)} end, Funs)),

    file:set_cwd(".."),
    cleanup(),
    stop_nodes(OrigNode, RefacNode),
    application:stop(wrangler), % TODO
    Res.
