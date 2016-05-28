all: h19_fe.class h89_fe.class

D630_CLASSES =	\
		PunchCard.class \
		CardPunch.class \
		SuffFileChooser.class \
		SuffFileFilter.class

SUPP_CLASSES = \
		SuffFileChooser$$TapeProt.class

%.class: %.java
	javac $?

jar: CardPunch.jar

CardPunch.jar: $(D630_CLASSES) $(SUPP_CLASSES)
	echo "Main-Class: CardPunch" >Manifest.txt
	jar cfm $@ Manifest.txt *.png $(foreach x,$^,'$(x)')

clobber:
	rm -f *.jar *.class
