
all:

%.o: %.s
	as200 -l -u -o $@ $? >$*.l

%.ss: %.o
	objdump200 -S -a 3 $< >$@

%.so: %.ss
	as200 -l -u -o $@ $? >$*.sl

pcdboot.out: boot.o pcdboot1.so pcdboot1.o pcdboot2.so pcdboot2.o brdldr.so brdldr.o
	ld200 -o $@ -a 0 $^

# probably doesn't require two-stage bootstrap
# brtboot.out: boot.o brtboot1.so brtboot1.o brtboot2.so brtboot2.o brtldr.so brtldr.o

.PRECIOUS: %.ss

__FRC__:
