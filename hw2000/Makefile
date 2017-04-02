JAVAS = $(wildcard *.java)
CLASSES = $(subst .java,.class,$(JAVAS))

EXTCLASSES =	Assembler.class \
		Loader.class \
		BRTLoader.class \
		CoreLoader.class \
		CharConverter.class

FONTS = $(wildcard fonts/*.ttf)

ICONS = $(wildcard icons/*.png)

JARES = bringup/bootmt.mti

PROGS = machin.ezc mcetests.ezc monitor.ezc \
	bootstrap.ezc brtloader.ezc \
	rdrtest.ezc puntest.ezc puntest2.ezc mtitest.ezc \
	dsktest1.ezc dsktest2.ezc itvtest.ezc \
	barberpole.mti prog3.pcd

all: $(CLASSES)

ship: jar tgz

jar: H2000.jar

tgz: hwprogs.tgz

H2000.jar: all docs/About.html
	echo "Main-Class: System2000" >Manifest.txt
	jar cfm $@ Manifest.txt *.class icons/*.png fonts/*.ttf \
		docs/* $(JARES)

docs/About.html: __FRC__
	./mkAbout >$@

hwprogs.tgz: $(addprefix bringup/,$(PROGS))
	tar -czf $@ -C bringup $(PROGS)

hwprogs.zip: $(addprefix bringup/,$(PROGS))
	zip -j $@ $(addprefix bringup/,$(PROGS))

%.class: %.java
	javac $?

$(EXTCLASSES):
	ln -s ../*/$@ .

__FRC__:
