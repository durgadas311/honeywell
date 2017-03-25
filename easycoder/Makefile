all: Assembler.class ezc.class

Assembler.class: Assembler.java BRTLoader.java CoreLoader.java Loader.java
	javac -source 1.7 -target 1.7 Assembler.java

ezc.class: ezc.java Assembler.class
	javac -source 1.7 -target 1.7 ezc.java
