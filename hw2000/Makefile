JAVAS = $(wildcard *.java)
CLASSES = $(subst .java,.class,$(JAVAS))

CPCLASSES =	CharConverter.class

F4CLASSES =	FortranLibrary.class

EZCLASSES =	Assembler*.class \
		MacroDef.class \
		Loader.class \
		BRTLoader.class \
		CardLoader.class \
		TapeLoader.class

EXTCLASSES = $(CPCLASSES) $(EZCLASSES) $(F4CLASSES)

FONTS = $(wildcard fonts/*.ttf)

ICONS = $(wildcard icons/*.png)

JARES = bringup/bootmt.mti

PROGS = machin.ezc mcetests.ezc monitor.ezc \
	bootstrap.ezc brtloader.ezc \
	rdrtest.ezc puntest.ezc puntest2.ezc mtitest.ezc \
	dsktest1.ezc dsktest2.ezc itvtest.ezc \
	barberpole.mti prog3.pcd

SHIPS = H2000.jar hwprogs.tgz h2000.tgz

all: $(EXTCLASSES) $(CLASSES)

www: __FRC__
	rsync -Wuv www/*.* durgadas.com:/http/durgadas.com/hw2000

ship: $(SHIPS)
	rsync -Wuv $(SHIPS) ../cardpunch/CardPunch.jar \
		../fortran4/Fortran4Embedded.jar \
		../easycoder/EasyCoder.jar \
		durgadas.com:/http/durgadas.com/sebhc/hw2000
	rsync -rWuv www/downloads/. durgadas.com:/http/durgadas.com/sebhc/hw2000/.

jar: H2000.jar

tgz: hwprogs.tgz

src: h2000.tgz

h2000.tgz: $(JAVAS) Makefile
	cd ..; tar -czhf hw2000/h2000.tgz \
		*/Makefile \
		*/mkAbout \
		*/*.java \
		hw2000/fonts/*.ttf \
		hw2000/icons/*.png \
		*/docs \
		*/www \
		hw2000/bringup/bootmt.mti \
		fortran4/progs \
		cardpunch/PunchCard.png \
		cardpunch/*.ttf

H2000.jar: all docs/About.html
	/bin/echo -e "Main-Class: System2000\nClass-Path: Fortran4Embedded.jar" >Manifest.txt
	jar cfm $@ Manifest.txt *.class icons/*.png fonts/*.ttf \
		docs/* $(JARES)

docs/About.html: __FRC__
	./mkAbout >$@

hwprogs.tgz: $(addprefix bringup/,$(PROGS))
	tar -czf $@ -C bringup $(PROGS)

hwprogs.zip: $(addprefix bringup/,$(PROGS))
	zip -j $@ $(addprefix bringup/,$(PROGS))

%.class: %.java
	javac -source 1.7 -target 1.7 $?

$(CPCLASSES):
	ln -s ../cardpunch/$@ .

$(F4CLASSES):
	ln -s ../fortran4/$@ .

$(EZCLASSES):
	ln -s ../easycoder/$@ .

__FRC__:
