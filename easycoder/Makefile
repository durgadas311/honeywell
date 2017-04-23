JAVAS = $(wildcard *.java)
CLASSES = $(subst .java,.class,$(JAVAS))

CPCLASSES =     CharConverter.class
HWCLASSES =     CoreMemory.class \
		CoreLoader.class \
		InstrDecode.class

EXTCLASSES = $(CPCLASSES) $(HWCLASSES)

all: $(EXTCLASSES) $(CLASSES)

jar: EasyCoder.jar

%.class: %.java
	javac -source 1.7 -target 1.7 $?

Assembler.class: $(JAVAS)
	javac -source 1.7 -target 1.7 Assembler.java

ezc.class: ezc.java Assembler.class
	javac -source 1.7 -target 1.7 ezc.java

EasyCoder.class: EasyCoder.java Assembler.class
	javac -source 1.7 -target 1.7 EasyCoder.java

EasyCoder.jar: all bootmt.mti
	echo "Main-Class: EasyCoder" >Manifest.txt
	jar cfm $@ Manifest.txt *.class bootmt.mti

bootmt.mti:
	ln -s ../hw2000/bringup/bootmt.mti .

$(CPCLASSES):
	ln -s ../cardpunch/$@ .

$(HWCLASSES):
	ln -s ../hw2000/$@ .

__FRC__:
