// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_MAT implements Instruction {
	public String mnem() { return "MAT"; }
	// Move characters to word mark And Translate
	public void execute(HW2000 sys) {
		int c = 0;
		if (sys.numXtra() >= 2) {
			c = (((sys.getXtra(0) & 077) << 6) | (sys.getXtra(1) & 077)) << 6;
			if (sys.am_na == 4) {
				c |= (sys.AAR & 0x40000);
			} else {
				c = (c & 0x7fff) | (sys.AAR & 0x78000);
			}
		} else {
			throw new FaultException("MAT malformed");
		}
		sys.CTL.setV((byte)077); // VR is "unspecified" after this
		byte a;
		byte b;
		do {
			a = sys.readMem(sys.AAR);
			sys.incrAAR(-1);
			b = sys.readMem(c | (a & 077));
			sys.writeMemMask(sys.BAR, b, (byte)0100);
			sys.incrBAR(-1);
		} while ((a & 0100) == 0 && (b & 0100) == 0);
	}
}
