public class I_TLU implements Instruction {
	// Table Look-Up
	public void execute(HW2000 sys) {
		if (sys.hadB() && sys.numXtra() > 0) {
			sys.CTL.setV(sys.getXtra(0));
		}
		byte v = sys.CTL.getV();
		v &= 007;	// ensure we don't test OVR (destructive) or ZB
		int aar = sys.AAR;
		boolean done = false;
		while (true) {
			done = I_C.compare(sys, true);
			if (done) {
				break;
			}
			boolean found = I_B_BCT.check(sys, v);
			if (found) {
				break;
			}
			sys.AAR = aar;	// reset search argument pointer
		}
		if (done) { // end of table...
			// ambiguous "B>A" condition...
			sys.CTL.setCompare(true, false);
		}
	}
}
