#
# Make std part of C library.
#
INSTDIR	= /usr/local
INSTLIB = libc.a

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
	rm -f *.o

libhw: $(OBJS)

.s.o:
	$(AS) -u -o $@ $<

.c.o:
	$(CC) -c -o $@ $<
