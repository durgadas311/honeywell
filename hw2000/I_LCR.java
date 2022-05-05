// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_LCR implements Instruction {
	// Load Control Register
	public void execute(HW2000 sys) {
		if (sys.hadA() && sys.numXtra() > 0) {
			sys.CTL.setV(sys.getXtra(0));
		}
		byte v = sys.CTL.getV();
		boolean allowed = (v == 064 || v == 067 || v == 070 || v == 077);
		if (sys.CTL.inStdMode() && sys.CTL.isPROTECT() &&
				!sys.isProceed()) {
			if (!sys.CTL.allowLCR() || !allowed) {
				throw new IIException("LCR violation", HW2000CCR.IIR_OPVIO);
			}
		}
		boolean restore = true;
		int reg = sys.loadFromAAR();
		reg &= 01777777;
		sys.setCtrlReg(v, reg);
		if (restore) {
			sys.restoreAAR();
		}
		sys.addTics(4 - sys.am_na);
	}
}
