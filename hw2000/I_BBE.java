// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_BBE implements Instruction {
	// Branch if Bit(s) Equal
	public void execute(HW2000 sys) {
		if (sys.numXtra() > 0) {
			sys.CTL.setV(sys.getXtra(0));
		}
		byte v = sys.CTL.getV();
		byte b = sys.readChar(sys.BAR);
		sys.incrBAR(-1);
		byte c = (byte)(b & v);
		boolean taken = (c != 0);
		if (taken) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
		}
		sys.addTics(2);
	}
}
