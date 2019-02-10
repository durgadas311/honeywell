INSTDIR = /usr/local

SUBDIRS = h200io

all:
	for d in $(SUBDIRS); do \
		$(MAKE) -C $$d; \
	done

install:
	$(MAKE) -C include install
	for d in $(SUBDIRS); do \
		$(MAKE) -C $$d INSTLIB=$(CURDIR)/libc.a install; \
	done
	cp libc.a $(INSTDIR)/lib/libc.a

clean:
	for d in $(SUBDIRS); do \
		$(MAKE) -C $$d clean; \
	done
	rm -rf libc.a
