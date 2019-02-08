INSTDIR	= /usr/local
CFLAGS	= -g -DFLEXNAMES ${CC_CFLAGS} ${DEBUG}

all: cpp

install: cpp
	cp cpp ${INSTDIR}/bin/cpp200

clean:
	rm -f *~ *.s *.o cpp

cpp: cpp.c cpy.c yylex.c
	$(CC) $(CFLAGS) -o cpp cpp.c cpy.c

cpy.c: cpy.y
	${YACC} cpy.y
	mv y.tab.c cpy.c

