# Makefile for building programs for H200/2000

all: recurs.pcd mcetests.pcd hellos.pcd

%.o: %.s
	as200 -l -u -o $@ $? >$*.l

%.mti: %.out
	out2boot -o $@ $<

%.pcd: %.out
	out2boot -c -o $@ $<

# TODO: do all monitors use org >= 1000?
%.out: %.o
	ld200 -o $@ -a 1000 $^

# assemble a bootable card deck
#%.pcd: %.out brdboot.pcd
#	out2brf -c $*.out | cat brdboot.pcd - >$@
# Just make BRD deck, user assembles decks in hopper
%.pcd: %.out
	out2brf -c -o $@ $<

hello%.pcd: hello.out
	out2brf -c -o $@ -P HELLO$* $^

hellos.pcd: hello1.pcd hello2.pcd hello3.pcd hello4.pcd hello5.pcd
	cat $^ >$@

.PRECIOUS: %.out

__FRC__:
