-module(utils).
-compile(export_all). % Exports all functions
-compile(debug_info).

demo() ->
    debugger:quick(vsc_equiv, check_equiv, ["79207c7c3", "1b0168", f_old, f_new]).

demo2() ->
    debugger:quick(vsc_equiv, check_equiv, ["79207c7c3", "5d434d", f_old, f_new]).

demo3() ->
    vsc_equiv:check_equiv("468f49", "be6b6", f_old, f_new).

demo3_debug() ->
    debugger:quick(vsc_equiv, check_equiv, ["468f49", "be6b6", g, g]).

demo4() ->
    debugger:quick(vsc_equiv, check_equiv, ["468f4", "be6b6"]).

demo5() ->
    debugger:quick(vsc_equiv, check_equiv, ["6f5871924ea", "2c5911a7847b"]).

bench(Count) ->
    run(Count) / Count.

run(0) -> 0;
run(Count) -> element(1, timer:tc(vsc_equiv, check_equiv, ["468f49", "0f07c3a", f_old, f_new])) + run(Count-1).
