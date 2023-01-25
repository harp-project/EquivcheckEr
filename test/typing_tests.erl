-module(typing_tests).

-import(typing,
        [get_specs/1,
         get_args/3,
         parse_spec/1]).

-include_lib("eunit/include/eunit.hrl").

source() ->
"-module(test3).
-compile(export_all). % Exports all functions

i(Xs,X) -> test2:g(Xs) + test2:h(X).

-spec p() -> integer().
p(Xs) -> 
    test:f_new(Xs) - 4.

-spec q(list(integer())) -> integer().
q(Xs) -> 
    X = 4,
    test:f_new(Xs) + X.

-spec r(integer(),string()) -> integer().
r(X) ->
    test:f_new([X]) + 4.".

specs() ->
    ["-spec p() -> integer().",
     "-spec q(list(integer())) -> integer().",
     "-spec r(integer(),string()) -> integer()."].

get_specs_test() ->
  ?assertEqual(specs(), get_specs(source())).

get_args_test_() ->
  [?_assertEqual([], get_args(source(), "p", 0)),
   ?_assertEqual(["list(integer())"], get_args(source(), "q", 1)),
   ?_assertEqual(["integer()","string()"], get_args(source(), "r", 2))].

parse_spec_test_() ->
  [?_assertEqual({"p", []}, parse_spec("-spec p() -> integer().")),
   ?_assertEqual({"q", ["list(integer())"]}, parse_spec("-spec q(list(integer())) -> integer().")),
   ?_assertEqual({"r", ["integer()","string()"]}, parse_spec("-spec r(integer(),string()) -> integer()."))].
