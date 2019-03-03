SRCS = $(wildcard *.c)
ASMS = $(subst .c,.s,$(SRCS))

CC = cc200
AS = as200
LD = ld200

all: mod1.os

mod1.os: trap.o mod1comm.o start.o syscall.o tick.o
	$(CC) -n -o $@ $^

%.o: %.s
	$(AS) -u -o $@ $<

%.o: %.c
	$(CC) -c $<

clean:
	rm -f mod1.os *.o a.out
