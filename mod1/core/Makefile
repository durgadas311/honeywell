SRCS = $(wildcard *.c)
ASMS = $(subst .c,.s,$(SRCS))

# 'trap' must be first!
MOD1 = trap.o mod1comm.o start.o syscall.o tick.o superv.o panic.o sgets.o

CC = cc200
AS = as200
LD = ld200

all: mod1.os

asm:
	for i in $(SRCS); do \
	$(CC) -S -g $$i; \
	done

mod1.os: $(MOD1)
	$(CC) -n -o $@ $^ -lc

%.o: %.s
	$(AS) -u -o $@ $<

%.o: %.c task.h
	$(CC) -c $<

clean:
	rm -f mod1.os *.o a.out $(ASMS)
