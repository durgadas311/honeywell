INSTDIR	= /usr/local
CFLAGS	= ${DEBUG}

all: c2

install: c2
	cp c2 ${INSTDIR}/lib

clean:
	rm -f *~ *.o c2

c2: c20.c c21.c c2.h
	$(CC) ${CFLAGS} -o c2 c20.c c21.c
