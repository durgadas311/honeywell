JAVAS = $(wildcard *.java)
CLASSES = $(subst .java,.class,$(JAVAS))

all: $(CLASSES)

%.class: %.java
	javac -source 1.7 -target 1.7 $?

jar: CardPunch.jar Sorter.jar Accounting.jar Viewer.jar

CardPunch.jar: all docs/About.html
	echo "Main-Class: CardPunch" >Manifest.txt
	jar cfm $@ Manifest.txt *.ttf PunchCard*.png *.class \
		icons/*.png docs/*

Sorter.jar: all docs/About2.html
	echo "Main-Class: Sorter" >Manifest.txt
	jar cfm $@ Manifest.txt *.ttf *.class \
		icons/*.png docs/*

Accounting.jar: all docs/About3.html
	echo "Main-Class: Accounting" >Manifest.txt
	jar cfm $@ Manifest.txt *.ttf *.class \
		icons/*.png docs/*

Viewer.jar: all docs/About2.html
	echo "Main-Class: Viewer" >Manifest.txt
	jar cfm $@ Manifest.txt *.ttf *.class \
		icons/*.png docs/*

docs/About.html: __FRC__
	./mkAbout >$@

docs/About2.html: __FRC__
	./mkAbout2 >$@

docs/About3.html: __FRC__
	./mkAbout3 >$@

www: __FRC__
	rsync -Wuv www/*.* durgadas.com:/http/durgadas.com/sims/punchcards

clobber:
	rm -f *.jar *.class

__FRC__:
