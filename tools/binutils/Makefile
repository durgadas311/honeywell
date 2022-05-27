CFLAGS = -O $(DEBUG)
INSTDIR = /usr/local

EXES = ld200 nm200 ar200 strip200 size200 objdump200 \
	adr2line200 out2brf out2boot pcddump brfdump brtcat

all: $(EXES)

install:  all
	cp -uv $(EXES) $(INSTDIR)/bin/

clean:
	rm -f $(EXES)

% : %.c
	$(CC) $(CFLAGS) -o $@ $<

ld200: ld.c ar.h a.out.h
	$(CC) $(CFLAGS) -o $@ $< ../as/hw200.o
nm200: nm.c a.out.h ../as/hw200.o
	$(CC) $(CFLAGS) -o $@ $<
ar200: ar.c ar.h a.out.h
	$(CC) $(CFLAGS) -o $@ $<
strip200: strip.c a.out.h
	$(CC) $(CFLAGS) -o $@ $<
size200: size.c a.out.h
	$(CC) $(CFLAGS) -o $@ $<
objdump200: objdump.c a.out.h
	$(CC) $(CFLAGS) -o $@ $<
adr2line200: adr2line.c a.out.h
	$(CC) $(CFLAGS) -o $@ $<
out2boot: out2boot.o ../as/hw200.o
	$(CC) -o $@ $< ../as/hw200.o
out2brf: out2brf.o ../as/hw200.o
	$(CC) -o $@ $< ../as/hw200.o
pcddump: pcddump.o
	$(CC) -o $@ $<
brfdump: brfdump.o
	$(CC) -o $@ $<
brtcat: brtcat.o ../as/hw200.o
	$(CC) -o $@ $^

../as/hw200.o:
	$(MAKE) -C ../as hw200.o
