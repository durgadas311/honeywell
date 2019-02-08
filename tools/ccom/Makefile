INSTDIR	= /usr/local
CFLAGS	= ${DEBUG}

all: c0 c1

install: c0 c1
	cp c0 ${INSTDIR}/lib/c0
	cp c1 ${INSTDIR}/lib/c1

clean:
	rm -f *~ *.o c0 c1 cvopt

c0: c00.c c01.c c02.c c03.c c04.c c05.c c0.h
	${CC} ${CFLAGS} -o c0 c00.c c01.c c02.c c03.c c04.c c05.c

c1: c10.c c11.c c12.c c13.c table.c c0.h
	${CC} $(CFLAGS) -o c1 c10.c c11.c c12.c c13.c table.c

table.c: optable cvopt
	./cvopt < optable > table.c

cvopt: cvopt.c
	${CC} $(CFLAGS) $(LDFLAGS) -o cvopt cvopt.c
