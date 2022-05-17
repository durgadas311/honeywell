// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_BCE implements Instruction {
	public String mnem() { return "BCE"; }
	// Branch on Character Equal
	public void execute(HW2000 sys) {
		byte v = sys.CTL.getV();
		byte b = sys.readChar(sys.BAR);
		sys.incrBAR(-1);
		if (sys.CTL.isS_MODE()) {
			sys.CTL.setCompare((b < v), (b == v));
		}
		boolean taken = (b == v);
		if (taken) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
		}
		sys.addTics(2);
	}
}
