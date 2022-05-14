CFLAGS = -O ${DEBUG}
INSTDIR = /usr/local

EXES = ld200 nm200 ar200 strip200 size200 objdump200 \
	adr2line200 out2brf out2boot # out2lmc out2cas

all: ${EXES}

install:  all
	cp ld200 ${INSTDIR}/bin/ld200
	cp nm200 ${INSTDIR}/bin/nm200
	cp ar200 ${INSTDIR}/bin/ar200
	cp strip200 ${INSTDIR}/bin/strip200
	cp size200 ${INSTDIR}/bin/size200
	cp objdump200 ${INSTDIR}/bin/objdump200
	cp adr2line200 ${INSTDIR}/bin/adr2line200
	cp out2boot ${INSTDIR}/bin/out2boot
	cp out2brf ${INSTDIR}/bin/out2boot
#	cp out2lmc ${INSTDIR}/bin/out2lmc
#	cp out2cas ${INSTDIR}/bin/out2cas

clean:
	rm -f ${EXES}

% : %.c
	${CC} ${CFLAGS} -o $@ $<

ld200: ld.c ar.h a.out.h
	${CC} ${CFLAGS} -o $@ $< ../as/hw200.o
nm200: nm.c a.out.h ../as/hw200.o
	${CC} ${CFLAGS} -o $@ $<
ar200: ar.c ar.h a.out.h
	${CC} ${CFLAGS} -o $@ $<
strip200: strip.c a.out.h
	${CC} ${CFLAGS} -o $@ $<
size200: size.c a.out.h
	${CC} ${CFLAGS} -o $@ $<
objdump200: objdump.c a.out.h
	${CC} ${CFLAGS} -o $@ $<
adr2line200: adr2line.c a.out.h
	${CC} ${CFLAGS} -o $@ $<
out2boot: out2boot.o ../as/hw200.o
	$(CC) -o $@ $< ../as/hw200.o
out2brf: out2brf.o ../as/hw200.o
	$(CC) -o $@ $< ../as/hw200.o

../as/hw200.o:
	$(MAKE) -C ../as hw200.o
