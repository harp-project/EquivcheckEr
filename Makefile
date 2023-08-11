TARGET_DIR=ebin

COMPILE_OPTIONS=+debug_info +export_all
TEST_COMPILE_OPTIONS=+debug_info +export_all

all: compile

compile:
	erlc ${COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/check_equiv.erl
	erlc ${COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/utils.erl
	erlc ${COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/typing.erl
	erlc ${COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/slicing.erl
	erlc ${COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/functions.erl
	erlc ${COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/config.erl
	erlc ${COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/diff.erl
	erlc ${COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/testing.erl
	erlc ${COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/repo.erl
	# erl -sname master -eval 'check_equiv:main(), init:stop()' -noshell

test: FORCE
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/check_equiv.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/utils.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/typing.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/slicing.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/functions.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} test/scoping_tests.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} test/typing_tests.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/config.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/diff.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/testing.erl
	erlc ${TEST_COMPILE_OPTIONS} -I include -o ${TARGET_DIR} src/repo.erl
	erl -eval 'scoping_tests:test(), typing_tests:test(), init:stop()' -noshell
FORCE:
