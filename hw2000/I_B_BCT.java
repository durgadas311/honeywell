public class I_B_BCT implements Instruction {
	// Branch or Branch on Condition Test (actually, Branch and Link)
	public void execute(HW2000 sys) {
		boolean taken = true;
		if (!sys.hadA() || sys.op_xtra.length > 0) {
			// BCT...
			taken = false;
			if (sys.hadA()) {
				// must have V then...
				sys.CTL.setV(sys.op_xtra[0]);
			}
			byte v = sys.CTL.getV();
			if ((v & 040) != 0) {
				// Arith/logic conditions -
				// Note: AIR bits are "A<=B" and "A=B"

				// 001 = "B<A"
				if ((v & 001) != 0 && !sys.CTL.isLE()) {
					taken = true;
				}
				// 002 = "B=A"
				if ((v & 002) != 0 && sys.CTL.isEQ()) {
					taken = true;
				}
				// 004 = "B>A"
				if ((v & 004) != 0 && sys.CTL.isLE() && !sys.CTL.isEQ()) {
					taken = true;
				}
				// 010 = Overflow
				if ((v & 010) != 0 && sys.CTL.isOVR()) {
					taken = true;
				}
				// 020 = Zero Balance
				if ((v & 020) != 0 && sys.CTL.isZB()) {
					taken = true;
				}
			} else {
				// SENSE - privileged
				if (sys.CTL.inStdMode() && sys.CTL.isPROTECT() &&
						!sys.CTL.isPROCEED() &&
						!sys.CTL.privBCT()) {
					throw new RuntimeException("BCT Violation");
				}
				// for now, sensors are never active...
			}
		}
		if (taken) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
		}
	}
}
