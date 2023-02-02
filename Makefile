TARGET_DIR=ebin

COMPILE_OPTIONS=+debug_info
TEST_COMPILE_OPTIONS=+debug_info +export_all

all: compile

compile:
	erlc ${COMPILE_OPTIONS} -o ${TARGET_DIR} src/check_equiv.erl
	erlc ${COMPILE_OPTIONS} -o ${TARGET_DIR} src/utils.erl
	erlc ${COMPILE_OPTIONS} -o ${TARGET_DIR} src/typing.erl
	erlc ${COMPILE_OPTIONS} -o ${TARGET_DIR} src/scoping.erl
	# erl -sname master -eval 'check_equiv:main(), init:stop()' -noshell

test: FORCE
	erlc ${TEST_COMPILE_OPTIONS} -o ${TARGET_DIR} src/check_equiv.erl
	erlc ${TEST_COMPILE_OPTIONS} -o ${TARGET_DIR} src/utils.erl
	erlc ${TEST_COMPILE_OPTIONS} -o ${TARGET_DIR} src/typing.erl
	erlc ${TEST_COMPILE_OPTIONS} -o ${TARGET_DIR} src/scoping.erl
	erlc ${TEST_COMPILE_OPTIONS} -o ${TARGET_DIR} test/scoping_tests.erl
	erlc ${TEST_COMPILE_OPTIONS} -o ${TARGET_DIR} test/typing_tests.erl
	erl -eval 'scoping_tests:test(), typing_tests:test(), init:stop()' -noshell
FORCE:
