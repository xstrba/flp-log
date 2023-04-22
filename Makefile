TARGET=flp22-log
TARGET_CLI=${TARGET}-cli
SRC=./src/main.pl
COMP=swipl
GOAL=main

${TARGET}: ${SRC}
	${COMP} --goal=${GOAL} --stand_alone=true -o ${TARGET} -c ${SRC}

cli:
	make ${TARGET_CLI}

${TARGET_CLI}: ${SRC}
	${COMP} -q -o ${TARGET_CLI} -c ${SRC}

force:
	make clean
	make

clean:
	rm -rf ${TARGET} ${TARGET_CLI}