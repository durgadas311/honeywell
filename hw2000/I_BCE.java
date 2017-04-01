// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_BCE implements Instruction {
	// Branch on Character Equal
	public void execute(HW2000 sys) {
		if (sys.numXtra() > 0) {
			sys.CTL.setV(sys.getXtra(0));
		}
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
