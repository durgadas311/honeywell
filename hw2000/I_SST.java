public class I_SST implements Instruction {
	// SubSTitute
	public void execute(HW2000 sys) {
		if (sys.op_xtra.length > 0) {
			sys.CTL.setV(sys.op_xtra[0]);
		}
		byte a = sys.readMem(sys.AAR);
		sys.incrAAR(-1);
		byte b = sys.readMem(sys.BAR);
		byte c = sys.CTL.getV();
		a &= c;
		b &= ~c;
		c = a | b;
		sys.writeChar(sys.BAR, c);
		sys.incrBAR(-1);
	}
}
