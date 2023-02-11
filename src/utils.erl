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
    vsc_equiv:check_equiv("6f5871924ea", "cbf1e19c9").

demo5_debug() ->
    debugger:quick(vsc_equiv, check_equiv, ["6f5871924ea", "cbf1e19c9"]).

demo6() ->
    vsc_equiv:show_result(vsc_equiv:check_equiv("c147579ce7", "d568fc4")).

demo6_db() ->
    debugger:quick(vsc_equiv, check_equiv, ["c147579ce7", "d568fc4"]).

demo7() ->
    vsc_equiv:show_result(vsc_equiv:check_equiv("6139a061", "827c5b800")).

demo7_db() ->
    debugger:quick(vsc_equiv, check_equiv, ["6139a061", "827c5b800"]).

demo8() ->
    vsc_equiv:show_result(check_equiv:check_equiv("cb78ac4", "640c556e3d")).

demo8_db() ->
    debugger:quick(check_equiv, check_equiv, ["cb78ac43", "640c556e"]).

bench(Count) ->
    run(Count) / Count.

run(0) -> 0;
run(Count) -> element(1, timer:tc(vsc_equiv, check_equiv, ["468f49", "0f07c3a", f_old, f_new])) + run(Count-1).
