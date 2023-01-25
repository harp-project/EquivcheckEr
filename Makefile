TARGET_DIR=ebin

all: compile

compile:
	erlc -o ${TARGET_DIR} src/check_equiv.erl
	erlc -o ${TARGET_DIR} src/utils.erl
	erlc -o ${TARGET_DIR} src/typing.erl
	erlc -o ${TARGET_DIR} src/scoping.erl
	erlc -o ${TARGET_DIR} test/scoping_tests.erl
	erlc -o ${TARGET_DIR} test/typing_tests.erl
	# erl -sname master -eval 'check_equiv:main(), init:stop()' -noshell

test: compile
	erl -eval 'scoping_tests:test(), typing_tests:test(), init:stop()' -noshell
