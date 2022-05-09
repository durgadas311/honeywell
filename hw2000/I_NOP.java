// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_NOP implements Instruction {
	public String mnem() { return "NOP"; }
	// No OPeration
	public void execute(HW2000 sys) {
		// Actually do nothing...
		// except store last character in VR...
		// TODO: what if no chars after NOP?
		sys.CTL.setV(sys.readMem(sys.SR - 1)); // ++tics
		// sys.addTics(1);
	}
}
