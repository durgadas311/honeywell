#
# Make run time library.
#
INSTDIR	= /usr/local

AS = $(INSTDIR)/bin/as200
AR = $(INSTDIR)/bin/ar200

ASMS = $(wildcard *.s)
OBJS = $(subst .s,.o,$(ASMS))
CRTO = crt0.o
LIBO = $(filter-out $(CRTO),$(OBJS))

all: $(OBJS)

install: all
	@rm -f $(INSTDIR)/lib/libcrt.a
	$(AR) cr $(INSTDIR)/lib/libcrt.a $(LIBO)
	cp crt0.o $(INSTDIR)/lib/crt0.o
#	cp mcrt0.o $(INSTDIR)/lib/mcrt0.o
#	cp crtx0.o $(INSTDIR)/lib/crtx0.o

clean:
	rm -f *.o a.out


crt: $(OBJS)

.s.o:
	$(AS) -o $@ $<
