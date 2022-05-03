JAVAS = $(wildcard *.java)
CLASSES = $(subst .java,.class,$(JAVAS))

CPCLASSES =     CharConverter.class
HWCLASSES =     CoreMemory.class \
		CoreLoader.class \
		RWChannel.class \
		HW2000.class \
		LightedButton.class \
		Errors.class \
		Peripheral.class \
		PopupFactory.class \
		PeriphLoader.class \
		SequentialRecordIO.class \
		P_MagneticTape*.class \
		P_CardReaderPunch*.class \
		InstrDecode.class

EXTCLASSES = $(CPCLASSES) $(HWCLASSES)

all: $(EXTCLASSES) $(CLASSES)

jar: EasyCoder.jar

%.class: %.java
	javac $?

Assembler.class: $(JAVAS)
	javac Assembler.java

ezc.class: ezc.java Assembler.class
	javac ezc.java

EasyCoder.class: EasyCoder.java Assembler.class
	javac EasyCoder.java

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
