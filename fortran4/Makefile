JAVAS = $(wildcard *.java)
CLASSES = $(subst .java,.class,$(JAVAS))
EMBCLS = $(subst .java,*.class,$(filter-out f4.java,$(JAVAS)))

CPCLASSES =	CharConverter.class

HWCLASSES =	CoreMemory.class \
		InstrDecode.class \
		Compiler.class

EZCLASSES =	Assembler.class \
		Loader.class \
		BRTLoader.class \
		RawLoader.class \
		CoreLoader.class \
		CardLoader.class \
		TapeLoader.class

EXTCLASSES = $(CPCLASSES) $(HWCLASSES) $(EZCLASSES)

all: $(EXTCLASSES) $(CLASSES)

jar: Fortran4Embedded.jar # FortranIV.jar

FortranIV.jar: all
	echo "Main-Class: FortranIV" >Manifest.txt
	jar cfm $@ Manifest.txt *.class

Fortran4Embedded.jar: all
	echo "Main-Class: none" >Manifest.txt
	jar cfm $@ Manifest.txt $(EMBCLS)

%.class: %.java
	javac -source 1.7 -target 1.7 $?

$(CPCLASSES):
	ln -s ../cardpunch/$@ .

$(HWCLASSES):
	ln -s ../hw2000/$@ .

$(EZCLASSES):
	ln -s ../easycoder/$@ .

__FRC__:
