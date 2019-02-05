TOOLS	= as binutils

TARGETS = tools # tools-inst

CC_CFLAGS = -DUNIX -DLINUX $(DEBUG)

all: $(TARGETS)

tools:
	for i in ${TOOLS}; do $(MAKE) -C $$i; done

distclean:
	for i in ${TOOLS}; do $(MAKE) -C $$i clean; done

%.o: %.s
	./as/as200 -l -o $@ $? >$*.l
