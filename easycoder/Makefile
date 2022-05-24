SPATH = -sourcepath .:../cardpunch:../hw2000
JAVAS = $(wildcard *.java)
CLASSES = $(subst .java,.class,$(JAVAS))

CPCLASSES =     CharConverter.class \
		CardHandler.class \
		CardStacker.class \
		CardHopper.class

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
		P_MagneticTape.class \
		P_CardReaderPunch.class \
		InstrDecode.class

EXTCLASSES = $(CPCLASSES) $(HWCLASSES)

all: $(EXTCLASSES) $(CLASSES)

jar: EasyCoder.jar

%.class: %.java
	javac $(SPATH) $?

Assembler.class: $(JAVAS)
	javac $(SPATH) Assembler.java

ezc.class: ezc.java Assembler.class
	javac $(SPATH) ezc.java

EasyCoder.class: EasyCoder.java Assembler.class
	javac $(SPATH) EasyCoder.java

EasyCoder.jar: all bootmt.mti
	echo "Main-Class: EasyCoder" >Manifest.txt
	jar cfm $@ Manifest.txt *.class bootmt.mti

bootmt.mti:
	ln -s ../hw2000/bringup/bootmt.mti .

$(CPCLASSES): %.class: ../cardpunch/%.java
	$(MAKE) -C ../cardpunch $@
	ln -sf ../cardpunch/$**.class .

$(HWCLASSES): %.class: ../hw2000/%.java
	$(MAKE) -C ../hw2000 $@
	ln -sf ../hw2000/$**.class .

__FRC__:
