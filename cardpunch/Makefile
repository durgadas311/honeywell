all: h19_fe.class h89_fe.class

D630_CLASSES =	\
		CardPunchOptions.class \
		PunchCard.class \
		PunchCardDeck.class \
		CardPunch.class \
		Punch.class \
		PunchPS.class \
		CharConverter.class \
		SuffFileChooser.class \
		SuffFileFilter.class

SUPP_CLASSES = \
		SuffFileChooser$$TapeProt.class \
		PunchPS$$DocPaperSize.class \
		PunchPS$$ReqPaperSize.class \
		sun/print/PSxStreamPrintService.class \
		sun/print/PSxStreamPrinterFactory.class \
		sun/print/PSxStreamPrintJob.class

%.class: %.java
	javac $?

jar: CardPunch.jar

CardPunch.jar: $(D630_CLASSES) $(SUPP_CLASSES)
	echo "Main-Class: CardPunch" >Manifest.txt
	jar cfm $@ Manifest.txt *.ttf PunchCard.png $(foreach x,$^,'$(x)')

clobber:
	rm -f *.jar *.class
