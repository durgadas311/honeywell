
all: pcdboot.pcd # mtiboot.mti or brtboot.mti?

%.o: %.s
	as200 -l -u -o $@ $? >$*.l

%.ss: %.o
	objdump200 -S -a 3 $< >$@

brdldr.ss: brdldr.o
	objdump200 -S -N 0224 -a 3 $< >$@

%.so: %.ss
	as200 -l -u -o $@ $? >$*.sl

%.mti: %.out
	out2boot -o $@ $<

%.pcd: %.out
	out2boot -c -o $@ $<

pcdboot.out: boot.o pcdboot1.so pcdboot1.o pcdboot2.so pcdboot2.o brdldr.so
	ld200 -o $@ -a 01620 $^

brdldr.out: brdldr.o
	ld200 -o $@ -a 0224 $^

brdboot.pcd: pcdboot.pcd brdldr.pcd
	cat $^ >$@

# probably doesn't require two-stage bootstrap
# brtboot.out: boot.o brtboot1.so brtboot1.o brtboot2.so brtboot2.o brtldr.so brtldr.o

.PRECIOUS: %.ss

__FRC__:
