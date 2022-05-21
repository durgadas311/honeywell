BRDLDR_ORG = 0234	# ORG for card monitor code
PCDBOOT_ORG = 01620	# ORG for card bootstrap code

all: brdboot.pcd # mtiboot.mti or brtboot.mti?

%.o: %.s
	as200 -l -o $@ $? >$*.l

%.ss: %.o
	objdump200 -S -a 2 $< >$@

brdldr.ss: brdldr.o
	objdump200 -S -N $(BRDLDR_ORG) -a 2 $< >$@

%.so: %.ss
	as200 -l -u -o $@ $? >$*.sl

%.mti: %.out
	out2boot -o $@ $<

%.pcd: %.out
	out2boot -c -o $@ $<

pcdboot.out: boot.o pcdboot0.so pcdboot0.o pcdboot1.so pcdboot1.o pcdboot2.so pcdboot2.o brdldr.so
	ld200 -o $@ -a $(PCDBOOT_ORG) $^

brdldr.out: brdldr.o
	ld200 -o $@ -a $(BRDLDR_ORG) $^

brdboot.pcd: pcdboot.pcd brdldr.pcd
	cat $^ >$@

# probably doesn't require two-stage bootstrap
# brtboot.out: boot.o brtboot1.so brtboot1.o brtboot2.so brtboot2.o brtldr.so brtldr.o

.PRECIOUS: %.ss

__FRC__:
