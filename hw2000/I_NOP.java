// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_NOP implements Instruction {
	public String mnem() { return "NOP"; }
	// No OPeration
	public void execute(HW2000 sys) {
		// Actually do nothing...
		// except store last character in VR...
		// which is done in HW2000.
		// sys.addTics(1);
	}
}
