// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_CI implements Instruction {
	public String mnem() { return "CI"; }
	// Clear Item mark
	public void execute(HW2000 sys) {
		sys.clrItem(sys.AAR);
		sys.incrAAR(-1);
		if (sys.hadB()) {
			sys.clrItem(sys.BAR);
		}
		sys.incrBAR(-1);
		sys.addTics(2);
	}
}
