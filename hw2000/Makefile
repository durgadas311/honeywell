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

SHIPS = H2000.jar hwprogs.tgz hwprogs.zip h2000.tgz

all: $(CLASSES)

www: __FRC__
	rsync -Wuv www/*.* durgadas.com:/http/durgadas.com/hw2000

ship: $(SHIPS)
	rsync -Wuv $(SHIPS) ../cardpunch/CardPunch.jar \
		durgadas.com:/http/durgadas.com/sebhc/hw2000
	rsync -rWuv www/downloads/. durgadas.com:/http/durgadas.com/sebhc/hw2000/.

jar: H2000.jar

tgz: hwprogs.tgz hwprogs.zip

src: h2000.tgz

h2000.tgz:
	cd ..; tar -czhf hw2000/h2000.tgz \
		hw2000/Makefile \
		hw2000/mkAbout \
		hw2000/*.java \
		hw2000/fonts/*.ttf \
		hw2000/icons/*.png \
		hw2000/docs/* \
		hw2000/bringup/bootmt.mti \
		$(addprefix hw2000/,$(EXTCLASSES))

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
