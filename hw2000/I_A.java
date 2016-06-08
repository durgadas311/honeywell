public class I_A implements Instruction {
	public void execute(HW2000 sys) {
		int bar = sys.BAR;
		byte a = sys.readMem(sys.AAR);
		byte b = sys.readMem(sys.BAR);
		byte aw = (a & 0100);
		byte bw = (b & 0100);
		byte c = 0;
		boolean negA = ((a & 0060) == 0040);
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
			s = 0;
			if (bw != 0) {
				break;
			}
			aDone = (aDone || aw != 0);
			if (aDone) {
				a = 0;
			} else {
				incrAAR(-1);
				a = sys.readMem(sys.AAR);
				aw = (a & 0100);
				a &= 017;
			}
			incrBAR(-1);
			b = sys.readMem(sys.BAR);
			bw = (b & 0100);
			b &= 017;
		}
		if (!trueAdd && cy != 0) {
			sys.BAR = bar;
			cy = 1;
			s = (negB ? 000 : 040);
			z = 0;
			while (true) {
				b = sys.readMem(sys.BAR);
				bw = (b & 0100);
				b &= 017;
				c = 9 - b + cy;
				if (c > 9) {
					cy = 1;
					c -= 10;
				}
				z |= c;
				sys.writeChar(sys.BAR, s | c);
				s = 0;
				if (bw != 0) {
					break;
				}
				incrBAR(-1);
			}
			// not possible to overflow here?
			cy = 0;
		}
		sys.CTL.setZB(z == 0);
		if (cy != 0) {
			sys.CTL.setOVR(true);
		}
	}
}
