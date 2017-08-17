JAVAS = $(wildcard *.java)
CLASSES = $(subst .java,.class,$(JAVAS))

CPCLASSES =	CharConverter.class

F4CLASSES =	FortranLibrary.class

EZCLASSES =	Assembler*.class \
		MacroDef.class \
		Loader.class \
		BRTLoader.class \
		BRTDataField.class \
		CardLoader.class \
		TapeLoader.class

EXTCLASSES = $(CPCLASSES) $(EZCLASSES) $(F4CLASSES)

FONTS = $(wildcard fonts/*.ttf)

ICONS = $(wildcard icons/*.png)

JARES = bringup/bootmt.mti bringup/brfloader.out bringup/mod1loader.out

PROGS = machin.ezc mcetests.ezc monitor.ezc \
	bootstrap.ezc brtloader.ezc \
	rdrtest.ezc puntest.ezc puntest2.ezc mtitest.ezc \
	dsktest1.ezc dsktest2.ezc itvtest.ezc \
	mod1test1.ezc mod1test2.ezc mod1test3.ezc \
	mmbtest1.ezc mmbtest2.ezc mmbtest3.ezc \
	brfloader.ezc mod1loader.ezc superv.ezc \
	barberpole.mti prog3.pcd 1eof.pcd example.pcd \
	master1.pcd detail1.pcd

SHIPS = H2000.jar hwprogs.tgz h2000.tgz

all: $(EXTCLASSES) $(CLASSES)

www: __FRC__
	rsync -Wuv www/*.* durgadas.com:/http/durgadas.com/hw2000

ship: $(SHIPS)
	rsync -Wuv $(SHIPS) ../cardpunch/*.jar \
		../fortran4/Fortran4Embedded.jar \
		../easycoder/EasyCoder.jar \
		durgadas.com:/http/durgadas.com/sebhc/hw2000
	rsync -rWuv www/downloads/. durgadas.com:/http/durgadas.com/sebhc/hw2000/.

jar: H2000.jar ../easycoder/EasyCoder.jar

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
		hw2000/bringup/*.out \
		fortran4/progs \
		cardpunch/PunchCard*.png \
		cardpunch/*.ttf

H2000.jar: all docs/About.html
	/bin/echo -e "Main-Class: System2000\nClass-Path: Fortran4Embedded.jar" >Manifest.txt
	jar cfm $@ Manifest.txt *.class icons/*.png fonts/*.ttf \
		docs/* $(JARES)

# If EasyCoder.jar was rebuilt, we need to add these classes.
# If any of these classes get rebuilt, we need to add them also.
# Basically, just always add them.
../easycoder/EasyCoder.jar: __FRC__
	jar uf $@ MacroLibMOD1MSIO.class MOD1MSIORunTime.class HW2000Trap.class

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
