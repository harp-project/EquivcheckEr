TARGET_DIR=ebin

all: src/check_equiv.erl
	erlc -o ${TARGET_DIR} src/check_equiv.erl
	erlc -o ${TARGET_DIR} src/utils.erl
	erlc -o ${TARGET_DIR} src/typing.erl
	erlc -o ${TARGET_DIR} src/scoping.erl
