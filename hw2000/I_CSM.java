// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_CSM implements Instruction {
	public String mnem() { return "CSM"; }
	// Change Sequencing Mode
	public void execute(HW2000 sys) {
		int t = sys.SR;
		sys.SR = sys.CSR;
		sys.CSR = t;
		sys.addTics(2);
	}
}
