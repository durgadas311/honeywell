public class I_LIB implements Instruction {
	// Load Index/Barricade register(s)
	public void execute(HW2000 sys) {
		byte a;
		a = sys.readChar(sys.AAR);
		sys.incrAAR(-1);
		a = (byte)((sys.readChar(sys.AAR) << 6) | a);
		sys.incrAAR(-1);
		sys.IBR = a;
		sys.BRR = 0;
		if (sys.hadB()) {
			a = sys.readChar(sys.BAR);
			sys.incrBAR(-1);
			a = (byte)((sys.readChar(sys.BAR) << 6) | a);
			sys.incrBAR(-1);
			sys.BRR = a;
			if (sys.numXtra() > 0) {
				a = sys.getXtra(0);
				sys.CTL.setLIB(a);
			}
		}
		// adr_min/max updated by PROTECT bit...
	}
}
