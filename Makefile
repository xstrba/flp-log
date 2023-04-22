TARGET=flp22-log
SRC=./src/main.pl
COMP=swipl

${TARGET}: ${SRC}
	${COMP} -q -o ${TARGET} -c ${SRC}

force:
	make clean
	make

clean:
	rm -rf ${TARGET}