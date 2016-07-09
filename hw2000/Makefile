
EXTCLASSES =	Assembler.class \
		CharConverter.class

CLASSES =	CoreMemory.class \
		EIException.class \
		FaultException.class \
		FormTapeEditor.class \
		FrontPanel.class \
		HW2000CCR.class \
		HW2000.class \
		HW2000FrontPanel.class \
		IIException.class \
		InstrDecode.class \
		Instruction.class \
		I_A.class \
		I_BA.class \
		I_B_BCT.class \
		I_BBE.class \
		I_BCC.class \
		I_BCE.class \
		I_BIM.class \
		I_BMS.class \
		I_BS.class \
		I_CAM.class \
		I_C.class \
		I_CI.class \
		I_CSM.class \
		I_CW.class \
		I_D.class \
		I_EXM.class \
		I_EXT.class \
		I_FAA.class \
		I_FMA.class \
		I_HA.class \
		I_H.class \
		I_IIC.class \
		I_LCA.class \
		I_LCR.class \
		I_LIB.class \
		I_MAT.class \
		I_MC.class \
		I_MCE.class \
		I_M.class \
		I_MCW.class \
		I_MIT.class \
		I_MOS.class \
		I_NOP.class \
		I_PCB.class \
		I_PDT.class \
		I_RNM.class \
		I_RVI.class \
		I_S.class \
		I_SCR.class \
		I_SIB.class \
		I_SI.class \
		I_SST.class \
		I_SVI.class \
		I_SW.class \
		I_TLU.class \
		I_ZA.class \
		I_ZS.class \
		LightedButton.class \
		P_Console.class \
		PeriphDecode.class \
		Peripheral.class \
		P_LinePrinter.class \
		P_MagneticTape.class \
		P_CardReaderPunch.class \
		RWChannel.class \
		SuffFileChooser.class \
		SuffFileFilter.class \
		System2000.class

ADDCLASSES =	FormTapeEditor$$TapePunch.class \
		SuffFileChooser$$ChkBox.class \
		P_Console$$ConsoleStatus.class \
		P_Disk$$DiskStatus.class


FONTS = $(wildcard fonts/*.ttf)

ICONS = $(wildcard icons/*.png)

JARES = bringup/bootmt.mti

PROGS = machin.ezc mcetests.ezc monitor.ezc bootstrap.ezc \
	rdrtest.ezc puntest.ezc prog3.pcd mtitest.ezc \
	dsktest1.ezc dsktest2.ezc

ship: jar tgz

jar: H2000.jar

tgz: hwprogs.tgz

H2000.jar: $(CLASSES) $(FONTS) $(ICONS) $(EXTCLASSES) $(JARES)
	echo "Main-Class: System2000" >Manifest.txt
	jar cfm $@ Manifest.txt *.class icons/*.png fonts/*.ttf \
		$(JARES)

hwprogs.tgz: $(addprefix bringup/,$(PROGS))
	tar -czf $@ -C bringup $(PROGS)

%.class: %.java
	javac $?

$(EXTCLASSES):
	ln -s ../*/$@ .
