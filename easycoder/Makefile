all: Assembler.class ezc.class

JAVAS = Assembler.java TapeLoader.java CardLoader.java RawLoader.java \
	BRTLoader.java CoreLoader.java Loader.java

Assembler.class: $(JAVAS)
	javac -source 1.7 -target 1.7 Assembler.java

ezc.class: ezc.java Assembler.class
	javac -source 1.7 -target 1.7 ezc.java
