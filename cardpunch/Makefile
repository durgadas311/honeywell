SPATH = -sourcepath .:../hw2000
JAVAS = $(wildcard *.java)
CLASSES = $(subst .java,.class,$(JAVAS))

HWCLASSES = PopupFactory.class

all: $(HWCLASSES) $(CLASSES)

%.class: %.java
	javac $(SPATH) $?

jar: CardPunch.jar Viewer.jar DataCenter.jar

CardPunch.jar: all docs/About.html
	echo "Main-Class: CardPunch" >Manifest.txt
	jar cfm $@ Manifest.txt fonts/*.ttf PunchCard*.png *.class \
		icons/*.png docs/*

Sorter.jar: all docs/About2.html
	echo "Main-Class: Sorter" >Manifest.txt
	jar cfm $@ Manifest.txt fonts/*.ttf *.class \
		icons/*.png docs/*

Accounting.jar: all docs/About3.html
	echo "Main-Class: Accounting" >Manifest.txt
	jar cfm $@ Manifest.txt fonts/*.ttf *.class \
		icons/*.png docs/*

Collator.jar: all docs/About4.html
	echo "Main-Class: Collator" >Manifest.txt
	jar cfm $@ Manifest.txt fonts/*.ttf *.class \
		icons/*.png docs/*

Viewer.jar: all
	echo "Main-Class: Viewer" >Manifest.txt
	jar cfm $@ Manifest.txt fonts/*.ttf *.class \
		icons/*.png docs/*

# All apps from a single control center...
DataCenter.jar: all docs/About2.html docs/About3.html docs/About4.html docs/About5.html
	echo "Main-Class: DataCenter" >Manifest.txt
	jar cfm $@ Manifest.txt fonts/*.ttf PunchCard*.png *.class \
		icons/*.png docs/*

docs/About.html: __FRC__
	./mkAbout >$@

docs/About2.html: __FRC__
	./mkAbout2 >$@

docs/About3.html: __FRC__
	./mkAbout3 >$@

docs/About4.html: __FRC__
	./mkAbout4 >$@

docs/About5.html: __FRC__
	./mkAbout5 >$@

www: __FRC__
	rsync -Wuv www/*.* durgadas.com:/var/www/html/sims/punchcards

$(HWCLASSES): %.class: ../hw2000/%.java
	$(MAKE) -C ../hw2000 $@
	ln -sf ../hw2000/$**.class .

clobber:
	rm -f *.jar *.class

__FRC__:
