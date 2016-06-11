public class I_BBE implements Instruction {
	// Branch if Bit(s) Equal
	public void execute(HW2000 sys) {
		if (sys.op_xtra.length > 0) {
			sys.CTL.setV(sys.op_xtra[0]);
		}
		byte v = sys.CTL.getV();
		byte b = sys.readChar(sys.BAR);
		sys.incrBAR(-1);
		byte c = (byte)(b & v);
		boolean taken = (c != 0);
		if (taken) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
		}
	}
}
