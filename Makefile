TARGET_DIR=ebin

COMPILE_OPTIONS=+debug_info +export_all
TEST_COMPILE_OPTIONS=+debug_info +export_all

REBAR3=/usr/bin/env rebar3

default: compile

all: compile test

compile:
	$(RM) ebin
	$(REBAR3) escriptize
	ln -s _build/default/bin/ ebin

clean:
	$(REBAR3) clean

test: FORCE
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/check_equiv.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/equivchecker_utils.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/typing.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/slicing.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/functions.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} test/scoping_tests.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} test/typing_tests.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/config.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/diff.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/equivchecker_testing.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/repo.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/cli.erl
	erl -eval 'scoping_tests:test(), typing_tests:test(), init:stop()' -noshell
FORCE:
