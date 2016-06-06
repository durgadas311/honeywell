public class I_B_BCT implements Instruction {
	// Branch (actually, Branch and Link)
	public void execute(HW2000 sys) {
		boolean taken = true;
		if (!sys.hadA() || sys.op_xtra.length > 0) {
			// BCT...
			taken = false;
			if (sys.hadA()) {
				// must have V then...
				sys.setV(sys.op_xtra[0]);
			}
			byte v = sys.getV();
			if (v & 040) != 0) {
				// Arith/logic conditions
				if ((v & 001) != 0 && sys.CTL.isLE() && !sys.CTL.isEQ()) {
					taken = true;
				}
				if ((v & 002) != 0 && sys.CTL.isEQ()) {
					taken = true;
				}
				if ((v & 004) != 0 && !sys.CTL.isLE()) {
					taken = true;
				}
				if ((v & 010) != 0 && sys.CTL.isOVR()) {
					taken = true;
				}
				if ((v & 020) != 0 && sys.CTL.isZB()) {
					taken = true;
				}
			} else {
				// SENSE - privileged
				if (!sys.privBCT()) {
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
