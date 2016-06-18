public class I_B_BCT implements Instruction {
	// Branch or Branch on Condition Test (actually, Branch and Link)

	public static boolean check(HW2000 sys, byte v) {
		boolean yes = false;

		// Arith/logic conditions -
		// Note: AIR bits are "A<=B" and "A=B"

		// 001 = "B<A"
		if ((v & 001) != 0 && !sys.CTL.isLE()) {
			yes = true;
		}
		// 002 = "B=A"
		if ((v & 002) != 0 && sys.CTL.isEQ()) {
			yes = true;
		}
		// 004 = "B>A"
		if ((v & 004) != 0 && sys.CTL.isLE() && !sys.CTL.isEQ()) {
			yes = true;
		}
		// 010 = Overflow
		if ((v & 010) != 0 && sys.CTL.isOVR()) {
			yes = true;
		}
		// 020 = Zero Balance
		if ((v & 020) != 0 && sys.CTL.isZB()) {
			yes = true;
		}
		return yes;
	}

	public void execute(HW2000 sys) {
		boolean taken = true;
		if (!sys.hadA() || sys.numXtra() > 0) {
			// BCT...
			taken = false;
			if (sys.hadA()) {
				// must have V then...
				sys.CTL.setV(sys.getXtra(0));
			}
			byte v = sys.CTL.getV();
			if ((v & 040) != 0) {
				boolean yes = I_B_BCT.check(sys, (byte)(v & 037));
				if (yes) {
					taken = true;
				}
			} else if ((v & 017) == 0) {
				taken = true;
			} else {
				// SENSE - privileged
				if (sys.CTL.inStdMode() && sys.CTL.isPROTECT() &&
						!sys.isProceed() &&
						!sys.CTL.privBCT()) {
					throw new IIException("BCT Violation", HW2000CCR.IIR_OPVIO);
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
