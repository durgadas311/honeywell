#
# Make h200io part of std C library.
#
INSTDIR	= /usr/local
INSTLIB = libhw.a

CC = cc200
AS = as200
AR = ar200

SRCS = $(wildcard *.c)
ASMS = $(wildcard *.s)
OBJS = $(subst .s,.o,$(ASMS)) # also $(subst .c,.o,$(SRCS))

all: libhw

install: $(OBJS)
	$(AR) cr $(INSTLIB) $(OBJS)

clean:
	rm -f *.o a.out

libhw: $(OBJS)

.s.o:
	$(AS) -o $@ $<

.c.o:
	$(CC) -c -o $@ $<
