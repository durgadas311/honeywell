CFLAGS = -O ${DEBUG}

EXES = ld200 nm200 ar200 strip200 size200 # out2lmc out2cas

all: ${EXES}

install: 
	cp ld200 ${INSTDIR}/bin/ld200
	cp nm200 ${INSTDIR}/bin/nm200
	cp ar200 ${INSTDIR}/bin/ar200
	cp strip200 ${INSTDIR}/bin/strip200
	cp size200 ${INSTDIR}/bin/size200
	cp out2lmc ${INSTDIR}/bin/out2lmc
	cp out2cas ${INSTDIR}/bin/out2cas

clean:
	rm -f ${EXES}

% : %.c
	${CC} ${CFLAGS} -o $@ $<

ld200: ld.c
	${CC} ${CFLAGS} -o $@ $<
nm200: nm.c
	${CC} ${CFLAGS} -o $@ $<
ar200: ar.c
	${CC} ${CFLAGS} -o $@ $<
strip200: strip.c
	${CC} ${CFLAGS} -o $@ $<
size200: size.c
	${CC} ${CFLAGS} -o $@ $<

out2lmc: out2lmc.c
out2cas: out2cas.c
