// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_RVI implements Instruction {
	public String mnem() { return "RVI"; }
	// Restore Variant and Indicators
	public void execute(HW2000 sys) {
		sys.saveAAR();
		byte v = sys.CTL.getV();
		byte a;
		if ((v & 001) != 0) {
			a = sys.readMem(sys.AAR);
			sys.incrAAR(1);
			sys.CTL.putCR(HW2000CCR.VR, a);
		}
		if ((v & 002) != 0) {
			a = sys.readMem(sys.AAR);
			sys.incrAAR(1);
			sys.CTL.putCR(HW2000CCR.AIR, a);
			sys.setAM(sys.CTL.getAM());
		}
		if ((v & 004) != 0) {
			a = sys.readMem(sys.AAR);
			sys.incrAAR(1);
			sys.CTL.putCR(HW2000CCR.XIR, a);
		}
		if ((v & 010) != 0) {
			a = sys.readMem(sys.AAR);
			sys.incrAAR(1);
			sys.CTL.putCR(HW2000CCR.IOR, a);
		}
		if ((v & 020) != 0) {
			a = sys.readMem(sys.AAR);
			sys.incrAAR(1);
			sys.CTL.putCR(HW2000CCR.PIR, a);
			// TODO: delay this until RNM?
			a &= HW2000CCR.PIR_PROTECT;
			if (sys.fp != null) {
				sys.fp.setProtect(a);
			}
			if (a != 0) {
				sys.adr_min = sys.BRR << 12;
				sys.adr_max = sys.adr_min + (sys.IBR << 12);
			} else {
				sys.adr_min = 0;
				sys.adr_max = 0x80000;
			}
		}
		sys.restoreAAR();
		sys.addTics(1);
	}
}
