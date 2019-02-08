INSTDIR	= /usr/local
CFLAGS	= -O ${CC_CFLAGS}
CC = cc

all: cc200

install: cc200
	cp cc200 ${INSTDIR}/bin/cc200

clean:
	rm -f *~ *.o cc200

cc200:
	${CC} ${CFLAGS} -o cc200 cc.c

