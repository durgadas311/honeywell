// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_PCB implements Instruction {
	public String mnem() { return "PCB"; }
	// Peripheral Control and Branch

	// Format: PCB/A,C1,C2,C3,...
	// Format: PCB/A,C1,CE,C2,C3,...
	//
	// CE vs. C2 can be determined by bits 011000. For CE, those bits will be 01.
	// Otherwise, it is C2.
	public void execute(HW2000 sys) {
		byte c1 = sys.getXtra(0);
		sys.CTL.setV(c1);
		boolean ce = PeriphDecode.isEsc(sys.getXtra(1));
		RWChannel c = sys.getChannel(c1);
		Peripheral p = null;
		if (sys.numXtra() > 1) {
			byte c2;
			if (ce) {
				c2 = sys.getXtra(2);
			} else {
				c2 = sys.getXtra(1);
			}
			p = sys.getPeriph(c2);
		}
		c.ctl(sys, p);
	}
}
