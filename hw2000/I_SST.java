// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_SST implements Instruction {
	public String mnem() { return "SST"; }
	// SubSTitute
	public void execute(HW2000 sys) {
		if (sys.numXtra() > 0) {
			sys.CTL.setV(sys.getXtra(0));
		}
		byte a = sys.readMem(sys.AAR);
		sys.incrAAR(-1);
		byte b = sys.readMem(sys.BAR);
		byte c = sys.CTL.getV();
		a &= c;
		b &= ~c;
		c = (byte)(a | b);
		sys.writeChar(sys.BAR, c);
		sys.incrBAR(-1);
	}
}
