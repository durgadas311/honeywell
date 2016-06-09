public class I_CI implements Instruction {
	// Clear Item mark
	public void execute(HW2000 sys) {
		byte b = sys.readMem(sys.AAR);
		sys.writeMem(sys.AAR, (byte)(b & ~0200));
		sys.incrAAR(-1);
		if (sys.hadA() && !sys.hadB()) {
			return;
		}
		byte b = sys.readMem(sys.BAR);
		sys.writeMem(sys.BAR, (byte)(b & ~0200));
		sys.incrBAR(-1);
	}
}
