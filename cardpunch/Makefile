JAVAS = $(wildcard *.java)
CLASSES = $(subst .java,.class,$(JAVAS))

all: $(CLASSES)

%.class: %.java
	javac -source 1.7 -target 1.7 $?

jar: CardPunch.jar

CardPunch.jar: all docs/About.html
	echo "Main-Class: CardPunch" >Manifest.txt
	jar cfm $@ Manifest.txt *.ttf PunchCard.png *.class \
		icons/*.png docs/*

docs/About.html: __FRC__
	./mkAbout >$@

www: __FRC__
	rsync -Wuv www/*.* durgadas.com:/http/durgadas.com/sims/punchcards

clobber:
	rm -f *.jar *.class

__FRC__:
