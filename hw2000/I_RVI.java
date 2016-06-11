public class I_RVI implements Instruction {
	// Restore Variant and Indicators
	public void execute(HW2000 sys) {
		if (sys.op_xtra.length == 0) {
			throw new RuntimeException("SVI malformed");
		}
		byte v = sys.op_xtra[0];
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
			if ((a & HW2000CCR.PIR_PROTECT) != 0) {
				sys.adr_min = sys.BRR << 12;
				sys.adr_max = sys.IBR << 12;
			} else {
				sys.adr_min = 0;
				sys.adr_max = 0x80000;
			}
		}
	}
}
