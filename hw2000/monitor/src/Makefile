BRDLDR_ORG = 0234	# ORG for card monitor code
PCDBOOT_ORG = 01620	# ORG for card bootstrap code

PCDS = brdboot.pcd

all: $(PCDS) # mtiboot.mti or brtboot.mti?

ship: all
	cp -uv $(PCDS) ../bin

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

mtiboot-a.out: mtiboot0.so mtiboot0.o mtiboot1.so mtiboot1.o
	ld200 -o $@ -a 0 $^
#	objdump200 -d -a2 $@

mtiboot-b.out: mtildr0.o
	ld200 -o $@ -a 1340 $^

mtiboot-b.mti: mtiboot-b.out
	out2brf -b -o $@ $<

tapeboot.mti: mtiboot-a.mti mtiboot-b.mti
	cat $^ >$@

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
