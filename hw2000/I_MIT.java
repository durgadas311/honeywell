// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_MIT implements Instruction {
	public String mnem() { return "MIT"; }
	// Move characters to Item mark and Translate
	public void execute(HW2000 sys) {
		int c = 0;
		byte v = 0;
		if (sys.numXtra() == 3) {
			v = sys.getXtra(2);
			// TODO: how to tell between 2-char C-address and V1,V2
			c = (((sys.getXtra(0) & 077) << 6) | (sys.getXtra(1) & 077)) << 6;
			if (sys.am_na == 4) {
				c |= (sys.AAR & 0x40000);
			} else {
				c = (c & 0x7fff) | (sys.AAR & 0x78000);
			}
		} else if (sys.numXtra() == sys.am_na + 1) {
			v = sys.getXtra(sys.am_na);
			for (int x = 0; x < sys.am_na; ++x) {
				c = (c << 6) | (sys.getXtra(x) & 077);
			}
			// TODO: does C-address allow indirection/indexing?
			c = (sys.AAR & ~sys.am_mask) | (c & sys.am_mask);
			c &= ~077; // needed?
		} else {
			throw new FaultException("MIT malformed");
		}
		byte a;
		byte b;
		byte b2;
		byte ai;
		byte t;
		int ca;
		do {
			a = sys.readMem(sys.AAR);
			ai = (byte)(a & 0200);
			a &= 077;
			sys.incrAAR(1);
			if ((v & 001) != 0) {
				t = sys.readMem(sys.AAR);
				sys.incrAAR(1);
				ai |= (t & 0200);
				a = (byte)((a << 6) | (t & 077));
			}
			if ((v & 002) != 0) {
				ca = c | (a << 1);
				b = sys.readMem(ca);
				b2 = sys.readMem(ca + 1);
				sys.CTL.setV(b2); // or 'b'?
				if (((b | b2) & 0100) != 0) {
					ca = sys.SR;
					sys.SR = sys.CSR;
					sys.CSR = ca;
					break;
				}
				sys.writeMemMask(sys.BAR, b, (byte)0100);
				sys.incrBAR(1);
				sys.writeMemMask(sys.BAR, b2, (byte)0100);
				sys.incrBAR(1);
			} else {
				ca = c | a;
				b = sys.readMem(ca);
				sys.CTL.setV(b);
				if ((b & 0100) != 0) {
					ca = sys.SR;
					sys.SR = sys.CSR;
					sys.CSR = ca;
					break;
				}
				sys.writeMemMask(sys.BAR, b, (byte)0100);
				sys.incrBAR(1);
			}
		} while (ai == 0);
	}
}
