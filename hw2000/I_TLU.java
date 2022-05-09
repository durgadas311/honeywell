// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_TLU implements Instruction {
	public String mnem() { return "TLU"; }
	// Table Look-Up
	public void execute(HW2000 sys) {
		if (sys.hadB() && sys.numXtra() > 0) {
			sys.CTL.setV(sys.getXtra(0));
		}
		byte bw;
		byte v = sys.CTL.getV();
		v &= 007;	// ensure we don't test OVR (destructive) or ZB
		int aar = sys.AAR;
		while (true) {
			bw = I_C.compare(sys, true);
			if (bw == 077) {
				break;
			}
			boolean found = I_B_BCT.check(sys, v);
			if (found) {
				break;
			}
			sys.AAR = aar;	// reset search argument pointer
			// skip to BAR WM, if not already there.
			while (bw == 0) {
				bw = (byte)(sys.readMem(sys.BAR) & 0100);
				sys.incrBAR(-1);
			}
		}
		if (bw == 077) { // end of table...
			// ambiguous "B>A" condition...
			sys.CTL.setCompare(true, false);
		}
	}
}
