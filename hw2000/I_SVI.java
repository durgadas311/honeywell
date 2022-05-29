// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_SVI implements Instruction {
	public String mnem() { return "SVI"; }
	// Store Variant and Indicators
	public void execute(HW2000 sys) {
		// must get address of char after variant...
		// variant is NOT stored in VR.
		int sr = sys.incrAdr(sys.oSR, 2); // should be same as sys.SR if WM correct
		byte v = sys.getXtra(0);
		if ((v & 001) != 0) {
			sys.writeMem(sr, sys.CTL.getCR(HW2000CCR.VR));
			sr = sys.incrAdr(sr, 1);
		}
		if ((v & 002) != 0) {
			sys.writeMem(sr, sys.CTL.getCR(HW2000CCR.AIR));
			sr = sys.incrAdr(sr, 1);
		}
		if ((v & 004) != 0) {
			sys.writeMem(sr, sys.CTL.getCR(HW2000CCR.XIR));
			sr = sys.incrAdr(sr, 1);
		}
		if ((v & 010) != 0) {
			sys.writeMem(sr, sys.CTL.getCR(HW2000CCR.IOR));
			sr = sys.incrAdr(sr, 1);
		}
		if ((v & 020) != 0) {
			sys.writeMem(sr, sys.CTL.getCR(HW2000CCR.PIR));
			sr = sys.incrAdr(sr, 1);
			if (sys.fp != null) {
				sys.fp.setProtect(0);
			}
		}
		if ((v & 040) != 0) {
			sys.writeMem(sr, sys.CTL.getCR(HW2000CCR.EIR));
			sr = sys.incrAdr(sr, 1);
		}
		sys.SR = sys.nextWM(sr, 0x80000); // needs to scan for WM...
		sys.addTics(1);
	}
}
