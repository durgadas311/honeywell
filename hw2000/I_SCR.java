// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_SCR implements Instruction {
	public String mnem() { return "SCR"; }
	// Store Control Register
	public void execute(HW2000 sys) {
		if (sys.hadA() && sys.numXtra() > 0) {
			sys.CTL.setV(sys.getXtra(0));
		}
		byte v = sys.CTL.getV();
		int reg = 0;
		if (v < 040) {
			// CLC or SLC
			if (sys.CTL.inStdMode() && sys.CTL.isPROTECT() &&
					!sys.isProceed()) {
				throw new IIException("SCR violation", HW2000CCR.IIR_OPVIO);
			}
		}
		sys.saveAAR();
		reg = sys.getCtrlReg(v);
		sys.storeToAAR(reg);
		sys.restoreAAR();
		sys.addTics(4 - sys.am_na);
	}
}
