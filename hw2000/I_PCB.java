public class I_PCB implements Instruction {
	// Peripheral Control and Branch

	// Format: PCB/A,C1,C2,C3,...
	// Format: PCB/A,C1,CE,C2,C3,...
	//
	// CE vs. C2 can be determined by bits 011000. For CE, those bits will be 01.
	// Otherwise, it is C2.
	public void execute(HW2000 sys) {
		byte c1 = sys.getXtra(0);
		RWChannel c = sys.getChannel(c1);
		Peripheral p = null;
		byte c2;
		if (sys.numXtra() > 1) {
			if ((sys.getXtra(1) & 030) == 010) {
				c2 = sys.getXtra(2);
			} else {
				c2 = sys.getXtra(1);
			}
			p = sys.getPeriph(c2);
		}
		c.ctl(sys, p);
	}
}
