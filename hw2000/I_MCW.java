public class I_MCW implements Instruction {
	// Move Characters to Word mark
	public void execute(HW2000 sys) {
		byte a;
		byte b;
		do {
			a = sys.readMem(sys.AAR);
			sys.incrAAR(-1);
			b = sys.readMem(sys.BAR);
			b = (byte)((b & 0100) | (a & ~0100));
			sys.writeMem(sys.BAR, b);
			sys.incrBAR(-1);
		} while ((a & 0100) == 0 && (b & 0100) == 0);
	}
}
