#
# Make h200io part of std C library.
#
INSTDIR	= /usr/local

CC = ${INSTDIR}/bin/cc200
AS = ${INSTDIR}/bin/as200
AR = ${INSTDIR}/bin/ar200

SRCS = $(wildcard *.c)
ASMS = $(wildcard *.s)
OBJS = $(subst .s,.o,$(ASMS)) # also $(subst .c,.o,$(SRCS))

all: libhw

install: $(OBJS)
	${AR} r ${INSTDIR}/lib/libhw.a ${OBJS}

clean:
	rm -f *.o

libhw: ${OBJS}

.s.o:
	$(AS) -u -o $@ $<

.c.o:
	${CC} -c -o $@ $<
