public class I_SIB implements Instruction {
	// Store Index/Barricade register(s)
	public void execute(HW2000 sys) {
		byte a;
		a = sys.IBR;
		sys.writeChar(sys.AAR, (byte)(a >> 6));
		sys.incrAAR(-1);
		sys.writeChar(sys.AAR, a);
		sys.incrAAR(-1);
		if (sys.hadB()) {
			a = sys.BRR;
			sys.writeChar(sys.BAR, (byte)(a >> 6));
			sys.incrBAR(-1);
			sys.writeChar(sys.BAR, a);
			sys.incrBAR(-1);
		}
	}
}
