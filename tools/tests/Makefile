SRCS = $(wildcard *.c)
OHWS = $(subst .c,.ohw,$(SRCS))

CC = cc200
AS = as200
LD = ld200

all: $(OHWS)

%.ohw: %.c /usr/local/lib/libc.a /usr/local/lib/libcrt.a
	$(CC) -o $@ $<

%.o: %.s
	$(AS) -u -o $@ $<

%.o: %.c
	$(CC) -c $<

clean:
	rm -f *.ohw *.o a.out
