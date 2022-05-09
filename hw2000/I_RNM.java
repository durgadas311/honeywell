// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_RNM implements Instruction {
	public String mnem() { return "RNM"; }
	// Return to Normal Mode
	public void execute(HW2000 sys) {
		sys.clearIntr();
		sys.addTics(2);
	}
}
