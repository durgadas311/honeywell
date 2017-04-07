JAVAS = $(wildcard *.java)
CLASSES = $(subst .java,.class,$(JAVAS))

all: Assembler.class ezc.class EasyCoder.class

jar: EasyCoder.jar

JAVAS = Assembler.java TapeLoader.java CardLoader.java RawLoader.java \
	BRTLoader.java CoreLoader.java Loader.java

Assembler.class: $(JAVAS)
	javac -source 1.7 -target 1.7 Assembler.java

ezc.class: ezc.java Assembler.class
	javac -source 1.7 -target 1.7 ezc.java

EasyCoder.class: EasyCoder.java Assembler.class
	javac -source 1.7 -target 1.7 EasyCoder.java

EasyCoder.jar: all
	echo "Main-Class: EasyCoder" >Manifest.txt
	jar cfm $@ Manifest.txt *.class

__FRC__:
