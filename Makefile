TARGET_DIR=ebin

all: src/vsc_equiv.erl
	erlc -o ${TARGET_DIR} src/vsc_equiv.erl
	erlc -o ${TARGET_DIR} src/utils.erl
