public class I_A implements Instruction {
	// Add (decimal)
	public static void add_sub(HW2000 sys, boolean sub) {
		int bar = sys.BAR;
		byte a = sys.readMem(sys.AAR);
		sys.incrAAR(-1);
		byte b = sys.readMem(sys.BAR);
		byte aw = (a & 0100);
		byte bw = (b & 0100);
		byte c = 0;
		boolean negA = ((a & 0060) == 0040);
		if (sub) {
			negA = !negA;
		}
		boolean negB = ((b & 0060) == 0040);
		a &= 017;
		b &= 017;
		boolean trueAdd = (negA == negB);
		byte s = (negB ? 040 : 000);
		byte cy = 0;
		if (!trueAdd) {
			cy = 1;
		}
		boolean aDone = false;
		byte z = 0;
		while (true) {
			if (a >= 014) {
				a = 0;
			}
			if (b >= 014) {
				b = 0;
			}
			if (!trueAdd) {
				b = 9 - b;
			}
			c = a + b + cy;
			if (c > 9) {
				cy = 1;
				c -= 10;
			}
			z |= c;
			sys.writeChar(sys.BAR, s | c);
			sys.incrBAR(-1);
			s = 0;
			if (bw != 0) {
				break;
			}
			aDone = (aDone || aw != 0);
			if (aDone) {
				a = 0;
			} else {
				a = sys.readMem(sys.AAR);
				sys.incrAAR(-1);
				aw = (a & 0100);
				a &= 017;
			}
			b = sys.readMem(sys.BAR);
			bw = (b & 0100);
			if (sys.CTL.isS_MODE()) {
				s = (b & 060);
			}
			b &= 017;
		}
		if (!trueAdd && cy != 0) {
			b = sys.readMem(bar);
			bw = (b & 0100);
			b &= 017;
			cy = 1;
			s = (negB ? 000 : 040);
			z = 0;
			while (true) {
				c = 9 - b + cy;
				if (c > 9) {
					cy = 1;
					c -= 10;
				}
				z |= c;
				sys.writeChar(bar, s | c);
				bar = sys.incrAdr(bar, -1);
				s = 0;
				if (bw != 0) {
					break;
				}
				b = sys.readMem(bar);
				bw = (b & 0100);
				if (!first && sys.CTL.isS_MODE()) {
					s = (b & 060);
				}
				b &= 017;
			}
			// not possible to overflow here?
			cy = 0;
		}
		sys.CTL.setZB(z == 0);
		if (cy != 0) {
			sys.CTL.setOVR(true);
		}
	}

	public void execute(HW2000 sys) {
		I_A.add_sub(sys, false);
	}
}
