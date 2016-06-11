import java.math.BigDecimal;

public class I_FMA implements Instruction {
	// Floating-point Memory-Accumulator ops

	public static double hwToNative(HW2000 sys, int ptr) {
		long d = 0;
		ptr -= 7; // TODO: find better way
		for (int ix = 0; ix < 8; ++ix) {
			d = (d << 6) | sys.readChar(ptr++);
		}
		byte ms = 0;
		long m = (d >> 12);
		if ((m & 0x800000000L) != 0) {
			ms = 1;
			m = (1 << 36) - m;
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

	public static long nativeToMant(double dd) {
		long d = Double.doubleToLongBits(dd);
		byte ms = (byte)((d >> 63) & 1);
		long m = (d >> 18) & 0x03ffffffffL;
		// implied "1"...
		m |= 0x0400000000L;
		if (ms != 0) {
			m = -m;
		}
		return m;
	}

	public static void nativeToHw(HW2000 sys, double dd, int ptr) {
		long d = Double.doubleToLongBits(dd);
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
		for (int ix = 0; ix < 8; ++ix) {
			byte b = (byte)(d & 077);
			sys.writeChar(ptr--, b);
			d >>= 6;
		}
	}

	public void execute(HW2000 sys) {
		if (sys.op_xtra.length != 2) {
			throw new RuntimeException("FMA malformed");
		}
		byte x = (byte)(sys.op_xtra[0] & 070);
		byte y = (byte)(sys.op_xtra[0] & 007);
		byte op = (byte)(sys.op_xtra[1] & 077);
		boolean taken = false;
		int ae;
		long m;
		BigDecimal bd;

		double a;
		switch(op) {
		case 000:	// Store Acc
			nativeToHw(sys, sys.AC[x], sys.AAR);
			sys.incrAAR(-8);
			break;
		case 002:	// Load Acc
			sys.AC[y] = hwToNative(sys, sys.AAR);
			sys.incrAAR(-8);
			break;
		case 001:	// Load Low-Order Result
			sys.AC[HW2000.LOR] = hwToNative(sys, sys.AAR);
			sys.incrAAR(-8);
			break;
		case 007:	// Store Low-Order Result
			nativeToHw(sys, sys.AC[HW2000.LOR], sys.AAR);
			sys.incrAAR(-8);
			break;
		case 010:	// Add
			sys.AC[y] = sys.AC[x] + hwToNative(sys, sys.AAR);
			sys.AC[HW2000.LOR] = 0.0; // what is this
			sys.incrAAR(-8);
			if (Double.isInfinite(sys.AC[y])) {
				sys.CTL.setEXO(true);
			}
			break;
		case 011:	// Subtract
			sys.AC[y] = sys.AC[x] - hwToNative(sys, sys.AAR);
			sys.AC[HW2000.LOR] = 0.0; // what is this
			sys.incrAAR(-8);
			if (Double.isInfinite(sys.AC[y])) {
				sys.CTL.setEXO(true);
			}
			break;
		case 013:	// Multiply
			a = hwToNative(sys, sys.AAR);
			sys.incrAAR(-8);
			sys.AC[y] = sys.AC[x] * a;
			sys.AC[HW2000.LOR] = 0.0; // what is this
			if (Double.isInfinite(sys.AC[y])) {
				sys.CTL.setEXO(true);
			}
			break;
		case 012:	// Divide
			a = hwToNative(sys, sys.AAR);
			sys.incrAAR(-8);
			if (sys.AC[x] == 0.0) {
				sys.CTL.setDVC(true);
				break;
			}
			sys.AC[HW2000.LOR] = a % sys.AC[x]; // remainder
			sys.AC[y] = a / sys.AC[x];
			if (Double.isInfinite(sys.AC[y])) {
				sys.CTL.setEXO(true);
			}
			break;
		// TODO:
		//	Binary Mantissa Shift
		//	Others?

		case 003:	// Convert Decimal to FP
			ae = sys.incrAdr(sys.AAR, -11);
			bd = I_M.hwToNative(sys, sys.AAR, ae);
			sys.AAR = ae;
			sys.AC[y] = bd.doubleValue();
			break;
		case 006:	// Convert FP to Decimal
			m = nativeToMant(sys.AC[x]);
			bd = new BigDecimal(m);
			ae = sys.incrAdr(sys.AAR, -11);
			I_M.nativeToHw(sys, bd, sys.AAR, ae);
			sys.AAR = ae;
			break;
		case 004:	// FP Test and Branch
			switch(y) {
			case 000:
				break;
			case 001:
				taken = (sys.AC[x] == 0.0);
				break;
			case 002:
				taken = (sys.AC[x] < 0.0);
				break;
			case 003:
				taken = (sys.AC[x] <= 0.0);
				break;
			case 004:
				taken = (sys.AC[x] > 0.0);
				break;
			case 005:
				taken = (sys.AC[x] >= 0.0);
				break;
			case 006:
				taken = (sys.AC[x] != 0.0);
				break;
			case 007:
				taken = true;
				break;
			}
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
			break;
		}
		if (taken) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
		}
	}
}
