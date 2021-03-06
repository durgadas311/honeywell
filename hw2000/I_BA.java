// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_BA implements Instruction {
	public String mnem() { return "BA"; }
	// Binary Add

	public static void nativeToHw(HW2000 sys, Long li, int lsd, int msd) {
		long l = li;
		int a = lsd;
		while (a >= msd) {
			sys.writeChar(a, (byte)(l & 077));
			a = sys.incrAdr(a, -1);
			l >>= 6;
		}
	}

	public static long hwToNative(HW2000 sys, int lsd, int msd) {
		long l = 0;
		int a = msd;
		byte b = sys.readChar(a);
		a = sys.incrAdr(a, 1);
		if ((b & 040) != 0) { // negative
			b |= 0300;
		}
		l = b;	// should sign-extend
		while (a <= lsd) {
			l <<= 6;
			l |= sys.readChar(a);
			a = sys.incrAdr(a, 1);
		}
		return l;
	}

	public void execute(HW2000 sys) {
		byte a = sys.readMem(sys.AAR);
		sys.incrAAR(-1);
		byte b = sys.readMem(sys.BAR);
		byte aw = (byte)(a & 0100);
		byte bw = (byte)(b & 0100);
		byte c = 0;
		a &= 077;
		b &= 077;
		byte cy = 0;
		while (true) {
			c = (byte)(a + b + cy);
			cy = (byte)(c >> 6);
			c &= 077;
			sys.writeChar(sys.BAR, c);
			sys.incrBAR(-1);
			if (bw != 0) {
				break;
			}
			if (aw != 0) {
				a = 0;
			} else {
				a = sys.readMem(sys.AAR);
				sys.incrAAR(-1);
				aw = (byte)(a & 0100);
				a &= 077;
			}
			b = sys.readMem(sys.BAR);
			bw = (byte)(b & 0100);
			b &= 077;
		}
	}
}
