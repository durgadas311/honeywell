CRT	= crt libc
TOOLS	= as binutils cc ccom $(CRT)

MAN1 = $(wildcard */*.1)
MAN2 = $(wildcard */*.2)
MAN3 = $(wildcard */*.3)
MAN5 = $(wildcard */*.5)
MAN7 = $(wildcard */*.7)

TARGETS = tools # tools-inst

CC_CFLAGS = -DUNIX -DLINUX $(DEBUG)

all: $(TARGETS)

tools:
	for i in ${TOOLS}; do $(MAKE) -C $$i; done

install: tools
	for i in ${TOOLS}; do $(MAKE) -C $$i install; done

distclean:
	for i in ${TOOLS}; do $(MAKE) -C $$i clean; done

libc: __FRC__
	$(MAKE) -C libc install

crt: __FRC__
	for i in ${CRT}; do $(MAKE) -C $$i install; done

%.o: %.s
	./as/as200 -l -o $@ $? >$*.l

man:
	cp -uv $(MAN1) /usr/local/man/man1
	cp -uv $(MAN2) /usr/local/man/man2
	cp -uv $(MAN3) /usr/local/man/man3
	cp -uv $(MAN5) /usr/local/man/man5
	cp -uv $(MAN7) /usr/local/man/man7

__FRC__:
