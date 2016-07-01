public class I_PDT implements Instruction {
	// Peripheral Data Transfer

	// Format: PDT/A,C1,C2,C3,...
	// Format: PDT/A,C1,CE,C2,C3,...
	//
	// CE vs. C2 can be determined by bits 011000. For CE, those bits will be 01.
	// Otherwise, it is C2.
	public void execute(HW2000 sys) {
		boolean ce = PeriphDecode.isEsc(sys.getXtra(1));
		if (sys.numXtra() < 2 || (ce && sys.numXtra() < 3)) {
			throw new FaultException("PDT malformed");
		}
		sys.validAdr(sys.AAR);
		byte c1 = sys.getXtra(0);
		byte c2;
		if (ce) {
			c2 = sys.getXtra(2);
		} else {
			c2 = sys.getXtra(1);
		}
		Peripheral p = sys.getPeriph(c2);
		RWChannel c = sys.getChannel(c1);
		if (c.busy() || p.busy()) {
System.err.println("PDT while busy");
			sys.SR = sys.oSR;
			// TODO: go to sleep if waiting too long?
			// If this channel was being used elsewhere for input,
			// esp. console input, it would be wrong to issue
			// another PDT on it.
			return;
		}
		c.io(sys, p);
		sys.addTics(3);
		// TODO: compute transfer time...
	}
}
