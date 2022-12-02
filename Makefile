TARGET_DIR=ebin

all: vsc_equiv.erl
	erlc -o ${TARGET_DIR} vsc_equiv.erl
