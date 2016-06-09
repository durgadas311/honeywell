public class I_LCA implements Instruction {
	// Load Characters to A-field word mark
	public void execute(HW2000 sys) {
		byte a;
		do {
			a = sys.readMem(sys.AAR);
			sys.incrAAR(-1);
			sys.writeMem(sys.BAR, a);
			sys.incrBAR(-1);
		} while ((a & 0100) == 0);
	}
}
