##########################################
# @file Makefile                         #
# @author Boris Štrbák (xstrba05)        #
#                                        #
# Compile program, run tests, make       #
# archive                                #
##########################################

TARGET=flp22-log
TARGET_CLI=${TARGET}-cli
SRC=./src/main.pl
SRC_CLI=./src/cli.pl
COMP=swipl
GOAL=main
ARCHIVE="flp-log-xstrba05.zip"

${TARGET}: ${SRC}
	${COMP} --goal=${GOAL} --stand_alone=true -o ${TARGET} -c ${SRC}

cli:
	make ${TARGET_CLI}

${TARGET_CLI}: ${SRC} ${SRC_CLI}
	${COMP} -q -o ${TARGET_CLI} -c ${SRC} ${SRC_CLI}

force:
	make clean
	make

clean:
	rm -rf ${TARGET} ${TARGET_CLI}

zip:
	rm -f ${ARCHIVE} || true
	zip ${ARCHIVE} ${SRC} ${SRC_CLI} ./test_in/* ./test_out/* ./test.sh ./Makefile ./README.md

test:
	@echo "Recompiling program"
	@make force > /dev/null
	@echo "Running tests"
	
	@./test.sh