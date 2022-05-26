# Makefile for building programs for H200/2000

PCDS = recurs.pcd mcetests.pcd hellos.pcd chain.pcd
MTIS = recurs.mti mcetests.mti hellos.mti

# override with "make ... ORG=nnnn"
ORG = 1340 # min for Tape Monitors, OK for Card Monitor (min 1000)

all: $(PCDS) $(MTIS)

ship: all
	cp -uv $(PCDS) $(MTIS) ../bin

%.o: %.s
	as200 -l -u -o $@ $? >$*.l

%.out: %.o
	ld200 -o $@ -a $(ORG) $^

# Just make BRD deck, user assembles decks in hopper
%.pcd: %.out
	out2brf -c -o $@ $<

%.mti: %.out
	out2brf -o $@ $<

hello%.pcd: hello.out
	out2brf -c -o $@ -P HELLO$* $^

hello%.mti: hello.out
	out2brf -o $@ -P HELLO$* $^

hellos.pcd: hello1.pcd hello2.pcd hello3.pcd hello4.pcd hello5.pcd
	cat $^ >$@

hellos.mti: hello1.mti hello2.mti hello3.mti hello4.mti hello5.mti
	cat $^ >$@

.PRECIOUS: %.out

__FRC__:
