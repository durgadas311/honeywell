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
	$(AR) cr $(INSTDIR)/lib/libcrt.a $(LIBO)
	cp crt0.o $(INSTDIR)/lib/crt0.o
#	mv mcrt0.o $(INSTDIR)/lib/mcrt0.o
#	mv crtx0.o $(INSTDIR)/lib/crtx0.o

clean:
	rm -f *.o a.out


crt: $(OBJS)

#crt0: crt0.o crtx0.o mcrt0.o
		
.s.o:
	$(AS) -o $@ $<
