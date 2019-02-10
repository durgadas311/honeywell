TOOLS	= as binutils cc ccom crt libc

TARGETS = tools # tools-inst

CC_CFLAGS = -DUNIX -DLINUX $(DEBUG)

all: $(TARGETS)

tools:
	for i in ${TOOLS}; do $(MAKE) -C $$i; done

install: tools
	for i in ${TOOLS}; do $(MAKE) -C $$i install; done

distclean:
	for i in ${TOOLS}; do $(MAKE) -C $$i clean; done

%.o: %.s
	./as/as200 -l -o $@ $? >$*.l

man:
	@for i in nm ld ar objdump size strip; do \
		cp -uv binutils/$${i}.1 /usr/local/man/man1/$${i}200.1; \
	done
	@for i in as; do \
		cp -uv as/$${i}.1 /usr/local/man/man1/$${i}200.1; \
	done
	@for i in a.out; do \
		cp -uv binutils/$${i}.5 /usr/local/man/man5/$${i}.200.5; \
	done
