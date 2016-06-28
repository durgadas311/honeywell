public class I_BIM implements Instruction {
	// Binary Integer Multiply

	public void execute(HW2000 sys) {
		sys.incrAAR(-4);
		sys.incrBAR(-4);
		long a = I_BA.hwToNative(sys, sys.AAR + 4, sys.AAR + 1);
		long b = I_BA.hwToNative(sys, sys.BAR + 4, sys.BAR + 1);
		b *= a;
		long s = b & ~0x07fffff;
		if (s != 0 && s != ~0x07fffff) {
			// sign in bit 63 should still be correct.
			sys.CTL.setOVR(true);
			b &= 0x07fffff;
			b |= (s >> 40) & ~0x07fffff;
		}
		I_BA.nativeToHw(sys, b, sys.BAR + 4, sys.BAR + 1);
		sys.addTics(16); // some approximation
	}
}
