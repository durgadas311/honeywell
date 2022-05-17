// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_H implements Instruction {
	public String mnem() { return "H"; }
	// Halt
	public void execute(HW2000 sys) {
		if (sys.hadA() && !sys.hadB()) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
		}
		if (sys.fp != null) {
			sys.fp.setInterrupt(-1);
		}
		sys.halt = true;
		sys.addTics(1);
	}
}
