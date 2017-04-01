// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_CW implements Instruction {
	// Clear Word mark
	public void execute(HW2000 sys) {
		sys.clrWord(sys.AAR);
		sys.incrAAR(-1);
		if (sys.hadB()) {
			sys.clrWord(sys.BAR);
		}
		sys.incrBAR(-1);
		sys.addTics(2);
	}
}
