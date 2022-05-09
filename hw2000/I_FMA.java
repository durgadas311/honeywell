// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.math.BigDecimal;

public class I_FMA implements Instruction {
	public String mnem() { return "FMA"; }
	// Floating-point Memory-Accumulator ops

	public static long hwToBin(HW2000 sys, int ptr) {
		long d = 0;
		ptr -= 7; // TODO: find better way
		for (int ix = 0; ix < 8; ++ix) {
			d = (d << 6) | sys.readChar(ptr++);
		}
		return d;
	}

	public static void binToHw(HW2000 sys, int ptr, long d) {
		for (int ix = 0; ix < 8; ++ix) {
			byte b = (byte)(d & 077);
			sys.writeChar(ptr--, b);
			d >>= 6;
		}
	}

	public static double hwToNative(HW2000 sys, int ptr) {
		long d = hwToBin(sys, ptr);
		boolean denorm = ((d & 02000000000000000L) == 0);
		return binToNative(d, denorm);
	}

	public static double binToNative(long d, boolean denorm) {
		byte ms = 0;
		long m = (d >> 12);
		if ((m & 0x800000000L) != 0) {
			ms = 1;
			m = (1 << 36) - m;
		}
		if ((m & 0x07ffffffffL) == 0) {
			// mant zero, nothing else matters
			return 0.0;
		}
		m &= 0x03ffffffffL;	// strip implied "1"... (verify?)
		int x = (int)(d & 0xfff); // TODO: proper strip...
		if ((x & 0x800) != 0) {
			x |= 0xfffff000;
		}
		// TODO: convert 12-bit exponent to 11-bit...
		x += 1023;
		d = ((long)ms << 63) | (((long)x & 0x7ff) << 52) | (m << 18);
		return Double.longBitsToDouble(d);
	}

	public static long binToMant(long d, boolean denorm) {
		if (d == 0) {
			return 0;
		}
		d >>= 12;	// eliminate exponent
		byte ms = (byte)((d >> 35) & 1);
		d &= 0377777777777L;
		if (!denorm) {
			// implied "1"...
			d |= 0200000000000L;
		}
		if (ms != 0) {
			// must sign-extend - not negate!
			d |= 01777777777400000000000L;
		}
		return d;
	}

	// "zeroes" the exponent.
	// caller is reponsible for handling "denormalized" flags.
	public static long mergeMant(long d, long m) {
		// don't care about 'd', we zero the exponent - nothing left.
		// it's already 2's-compliment
		m &= 0777777777777L;
		return m << 12;	// "0" exponent
	}

	public static long nativeToBin(double dd) {
		long d = Double.doubleToLongBits(dd);
		if ((d & 0x7fffffffffffffffL) == 0) {
			d = 0; // TODO: does HW allow -0 ?
		} else {
			byte ms = (byte)((d >> 63) & 1);
			int x = (int)((d >> 52) & 0x7ff);
			x -= 1023;
			long m = (d >> 18) & 0x03ffffffffL;
			// implied "1"...
			m |= 0x0400000000L;
			if (ms != 0) {
				m = -m;
			}
			d = (m << 12) | (x & 0xfff);
		}
		return d;
	}

	public static void nativeToHw(HW2000 sys, double dd, boolean denorm, int ptr) {
		long d = nativeToBin(dd);
		binToHw(sys, ptr, d);
	}

	public void execute(HW2000 sys) {
		if (sys.numXtra() != 2) {
			throw new FaultException("FMA malformed");
		}
		byte x = (byte)((sys.getXtra(0) & 070) >> 3);
		byte y = (byte)(sys.getXtra(0) & 007);
		byte op = (byte)(sys.getXtra(1) & 077);
		sys.CTL.setV((byte)077); // VR is "unspecified" after this
		boolean taken = false;
		int ae;
		long m;
		BigDecimal bd;

		double a, b;
		switch(op) {
		case 000:	// Store Acc
			binToHw(sys, sys.AAR, sys.AC[x]);
			sys.incrAAR(-8);
			sys.addTics(2);
			break;
		case 002:	// Load Acc
			sys.AC[y] = hwToBin(sys, sys.AAR);
			sys.denorm[y] = false; // right?
			sys.incrAAR(-8);
			sys.addTics(2);
			break;
		case 001:	// Load Low-Order Result
			sys.AC[HW2000.LOR] = hwToBin(sys, sys.AAR);
			sys.denorm[HW2000.LOR] = false; // right?
			sys.incrAAR(-8);
			sys.addTics(1);
			break;
		case 007:	// Store Low-Order Result
			binToHw(sys, sys.AAR, sys.AC[HW2000.LOR]);
			sys.incrAAR(-8);
			sys.addTics(1);
			break;
		case 010:	// Add
			a = binToNative(sys.AC[x], sys.denorm[x]) +
					hwToNative(sys, sys.AAR);
			sys.AC[y] = nativeToBin(a);
			sys.denorm[y] = false;
			sys.AC[HW2000.LOR] = 0L; // what is this
			sys.incrAAR(-8);
			if (Double.isInfinite(a)) {
				sys.CTL.setEXO(true);
			}
			sys.addTics(12); // + Nn/6
			break;
		case 011:	// Subtract
			a = binToNative(sys.AC[x], sys.denorm[x]) -
					hwToNative(sys, sys.AAR);
			sys.AC[y] = nativeToBin(a);
			sys.denorm[y] = false;
			sys.AC[HW2000.LOR] = 0L; // what is this
			sys.incrAAR(-8);
			if (Double.isInfinite(a)) {
				sys.CTL.setEXO(true);
			}
			sys.addTics(12); // + Nn/6
			break;
		case 013:	// Multiply
			a = hwToNative(sys, sys.AAR);
			sys.incrAAR(-8);
			a = binToNative(sys.AC[x], sys.denorm[x]) * a;
			sys.AC[y] = nativeToBin(a);
			sys.denorm[y] = false;
			sys.AC[HW2000.LOR] = 0L; // what is this
			sys.denorm[HW2000.LOR] = false;
			if (Double.isInfinite(a)) {
				sys.CTL.setEXO(true);
			}
			sys.addTics(9); // + N1/6 + Nn/6
			break;
		case 012:	// Divide
			a = hwToNative(sys, sys.AAR);
			sys.incrAAR(-8);
			if (sys.AC[x] == 0L) {
				sys.CTL.setDVC(true);
				break;
			}
			b = binToNative(sys.AC[x], sys.denorm[x]);
			sys.AC[HW2000.LOR] = nativeToBin(a % b); // remainder
			a = a / b;
			sys.AC[y] = nativeToBin(a);
			sys.denorm[y] = false;
			sys.denorm[HW2000.LOR] = false;
			if (Double.isInfinite(a)) {
				sys.CTL.setEXO(true);
			}
			sys.addTics(16); // + Nn/6
			break;
		case 003:	// Convert Decimal to FP
			ae = sys.incrAdr(sys.AAR, -11);
			bd = I_M.hwToNative(sys, sys.AAR, ae);
			sys.AAR = ae;
			sys.AC[y] = nativeToBin(bd.doubleValue());
			// TODO: overflow in LOR...
			sys.addTics(9);
			break;
		case 006:	// Convert FP to Decimal
			m = binToMant(sys.AC[x], sys.denorm[x]);
			bd = new BigDecimal(m);
			ae = sys.incrAdr(sys.AAR, -11);
			I_M.nativeToHw(sys, bd, sys.AAR, ae);
			sys.AAR = ae;
			sys.addTics(10);
			break;
		case 004:	// FP Test and Branch
			a = binToNative(sys.AC[x], sys.denorm[x]);
			switch(y) {
			case 000:
				break;
			case 001:
				taken = (a == 0.0);
				break;
			case 002:
				taken = (a < 0.0);
				break;
			case 003:
				taken = (a <= 0.0);
				break;
			case 004:
				taken = (a > 0.0);
				break;
			case 005:
				taken = (a >= 0.0);
				break;
			case 006:
				taken = (a != 0.0);
				break;
			case 007:
				taken = true;
				break;
			}
			sys.addTics(1);
			break;
		case 005:	// FP Test and Branch on Indicator
			switch(y) {
			case 000:
				break;
			case 001:
				taken = sys.CTL.isMPO();
				break;
			case 002:
				taken = sys.CTL.isEXO();
				break;
			case 003:
				taken = sys.CTL.isMPO() && sys.CTL.isEXO();
				break;
			case 004:
				taken = sys.CTL.isDVC();
				break;
			case 005:
				taken = sys.CTL.isDVC() && sys.CTL.isMPO();
				break;
			case 006:
				taken = sys.CTL.isDVC() && sys.CTL.isEXO();
				break;
			case 007:
				taken = sys.CTL.isDVC() && sys.CTL.isEXO() && sys.CTL.isMPO();
				break;
			}
			sys.addTics(1);
			break;
		}
		// If the operation overwrote AC[7], restore 0.0
		sys.AC[7] = 0L;
		sys.denorm[7] = false;
		if (taken) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
			sys.addTics(2);
		}
	}
}
