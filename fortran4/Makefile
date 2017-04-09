JAVAS = $(wildcard *.java)
CLASSES = $(subst .java,.class,$(JAVAS))

CPCLASSES =	CharConverter.class

HWCLASSES =	CoreMemory.class

EZCLASSES =	Assembler.class \
		Loader.class \
		BRTLoader.class \
		CoreLoader.class \
		CardLoader.class \
		TapeLoader.class

EXTCLASSES = $(CPCLASSES) $(HWCLASSES)

all: $(EXTCLASSES) $(CLASSES)

jar: FortranIV.jar

FortranIV.jar: all
	echo "Main-Class: FortranIV" >Manifest.txt
	jar cfm $@ Manifest.txt *.class

%.class: %.java
	javac -source 1.7 -target 1.7 $?

$(CPCLASSES):
	ln -s ../cardpunch/$@ .

$(HWCLASSES):
	ln -s ../hw2000/$@ .

$(EZCLASSES):
	ln -s ../easycoder/$@ .

__FRC__:
