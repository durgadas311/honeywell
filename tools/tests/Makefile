SRCS = $(wildcard *.c)
ASMS = $(subst .c,.s,$(SRCS))
OHWS = $(subst .c,.ohw,$(SRCS))

CC = cc200
AS = as200
LD = ld200

all: $(OHWS)

asm: $(ASMS)

%.ohw: %.c /usr/local/lib/libc.a /usr/local/lib/libcrt.a
	$(CC) -o $@ $<

%.o: %.s
	$(AS) -u -o $@ $<

%.o: %.c
	$(CC) -c $<

%.s: %.c
	$(CC) -S -g $<

clean:
	rm -f $(OHWS) $(ASMS) *.o a.out
