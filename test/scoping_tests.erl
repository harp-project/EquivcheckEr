-module(scoping_tests).

-import(scoping,
        [get_name/1,
         get_arity/1,
         is_function_def/1,
         extract_file/1,
         get_module/1]).

-include_lib("eunit/include/eunit.hrl").

get_name_test_() ->
  [?_assertEqual("some_name", get_name("some_name()")),
   ?_assertEqual("some_name", get_name("some_name(args)")),
   ?_assertEqual("some_name", get_name("some_name(some,args)"))].

get_arity_test_() ->
  [?_assertEqual(1, get_arity("some_name()")),
   ?_assertEqual(1, get_arity("some_name(args)")),
   ?_assertEqual(2, get_arity("some_name(some,args)"))].

get_module_test_() ->
  [?_assertEqual(test1, get_module("test1.erl")),
   ?_assertEqual(test2, get_module("some/path/test2.erl")),
   ?_assertEqual(test3, get_module("some/longer/path/test3.erl"))].

extract_file_test_() ->
  [?_assertEqual("test.erl", extract_file("a/test.erl b/test.erl")),
   ?_assertEqual("test2.erl", extract_file("a/test2.erl b/test2.erl"))].

is_function_def_test_() ->
  [?_assert(is_function_def("-f_old(Xs) -> lists:sum(Xs).")),
   ?_assert(is_function_def("+f_new(Xs) ->")),
   ?_assert(is_function_def("-some_fun(some,args) ->")),
   ?_assertNot(is_function_def("+-spec f_new(list(integer())) -> integer().")),
   ?_assertNot(is_function_def("+    f_new([X]) + 4.")),
   ?_assertNot(is_function_def("-type fun_info() :: {atom(), string(), integer()}.")),
   ?_assertNot(is_function_def("-compile(export_all)."))].
