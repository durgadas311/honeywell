public class I_LIB implements Instruction {
	// Load Index/Barricade register(s)
	public void execute(HW2000 sys) {
		byte a;
		a = sys.readChar(sys.AAR);
		sys.incrAAR(-1);
		a = (a << 6) | sys.readChar(sys.AAR);
		sys.incrAAR(-1);
		sys.IBR = a;
		sys.BRR = 0;
		if (sys.hadB()) {
			a = sys.readChar(sys.AAR);
			sys.incrAAR(-1);
			a = (a << 6) | sys.readChar(sys.AAR);
			sys.incrAAR(-1);
			sys.BRR = a;
			if (sys.op_xtra.length > 0) {
				a = sys.op_xtra[0];
				sys.CTL.setLIB(a);
			}
		}
		// adr_min/max updated by PROTECT bit...
	}
}
