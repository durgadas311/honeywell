CC = cc
CFLAGS = -I../binutils -O ${DEBUG}
INSTDIR = /usr/local

OBJS = assem.o lexer.o machine.o output.o tables.o expr.o list.o main.o symbol.o hw200.o
SRCS = assem.c lexer.c machine.c output.c tables.c expr.c list.c main.c symbol.c hw200.c

all: as200

install: as200
	cp as200 ${INSTDIR}/bin/as200

clean:
	rm -f *.o as200

as200: ${OBJS}
	${CC} ${CFLAGS} -o $@ ${OBJS} -lm

assem.o: assem.c as.h
lexer.o: lexer.c as.h
machine.o: machine.c as.h
output.o: output.c as.h
tables.o: tables.c as.h
expr.o: expr.c as.h
list.o: list.c as.h
main.o: main.c as.h
symbol.o: symbol.c as.h
hw200.o: hw200.c
