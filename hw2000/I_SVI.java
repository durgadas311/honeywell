// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_SVI implements Instruction {
	public String mnem() { return "SVI"; }
	// Store Variant and Indicators
	public void execute(HW2000 sys) {
		if (sys.numXtra() == 0) {
			throw new FaultException("SVI malformed");
		}
		// must get address of char after variant...
		int sr = sys.oSR + 2;
		byte v = sys.getXtra(0);
		if ((v & 001) != 0) {
			sys.writeMem(sr++, sys.CTL.getCR(HW2000CCR.VR));
		}
		if ((v & 002) != 0) {
			sys.writeMem(sr++, sys.CTL.getCR(HW2000CCR.AIR));
		}
		if ((v & 004) != 0) {
			sys.writeMem(sr++, sys.CTL.getCR(HW2000CCR.XIR));
		}
		if ((v & 010) != 0) {
			sys.writeMem(sr++, sys.CTL.getCR(HW2000CCR.IOR));
		}
		if ((v & 020) != 0) {
			sys.writeMem(sr++, sys.CTL.getCR(HW2000CCR.PIR));
			if (sys.fp != null) {
				sys.fp.setProtect(0);
			}
		}
		if ((v & 040) != 0) {
			sys.writeMem(sr++, sys.CTL.getCR(HW2000CCR.EIR));
		}
		sys.addTics(1);
	}
}
