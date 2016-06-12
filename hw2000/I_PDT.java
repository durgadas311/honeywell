public class I_PDT implements Instruction {
	// Peripheral Data Transfer

	// Format: PDT/A,C1,C2,C3,...
	// Format: PDT/A,C1,CE,C2,C3,...
	//
	// CE vs. C2 can be determined by bits 011000. For CE, those bits will be 01.
	// Otherwise, it is C2.
	public void execute(HW2000 sys) {
		if (sys.op_xtra.length < 2 ||
				((sys.op_xtra[1] & 030) == 010 && sys.op_xtra.length < 3)) {
			throw new RuntimeException("PDT malformed");
		}
		byte c2;
		if ((sys.op_xtra[1] & 030) == 010) {
			c2 = sys.op_xtra[2];
		} else {
			c2 = sys.op_xtra[1];
		}
		Peripheral p = sys.getPeriph(c2);
		p.io(sys);
	}
}
