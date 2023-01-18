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

    Diff_Output = os:cmd("git diff --no-ext-diff " ++ OrigHash ++ " " ++ RefacHash),

    % Gets back the files that have to be compiled, and the functions that have to be tested
    {Files, Funs} = scoping:scope(Diff_Output),

    % Checkout and compile the necessary modules into two separate folders
    % This is needed because QuickCheck has to evaluate to old and the new
    % function repeatedly side-by-side
    checkout(OrigHash),
    comp(Files, "orig"),

    checkout(RefacHash),
    comp(Files, "refac"),

    {OrigNode, RefacNode} = start_nodes(),

    Options = [quiet, long_result],
    Res = lists:filter(fun({_, _, Eq}) -> Eq =/= true end, lists:map(fun({M, F, Type}) -> {M, F, proper:quickcheck(?FORALL(X, Type, prop_same_output(OrigNode, RefacNode, M, F, [X])), Options)} end, Funs)),

    file:set_cwd(".."),
    cleanup(),
    stop_nodes(OrigNode, RefacNode),
    application:stop(wrangler), % TODO
    Res.
