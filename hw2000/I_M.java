// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.math.BigDecimal;

public class I_M implements Instruction {
	public String mnem() { return "M"; }
	public static int nz;	// count non-zero digits
	public static int lz;	// count leading zero digits
	// Multiply (decimal)
	public static int fieldStart(HW2000 sys, int adr) {
		int a = adr;
		byte b;
		do {
			b = sys.readMem(a);
			a = sys.incrAdr(a, -1);
		} while ((b & 0100) == 0);
		return a;
	}

	public static BigDecimal hwToNative(HW2000 sys, int lsd, int msd) {
		char[] ch;
		nz = 0;
		lz = 0;
		// TODO: worry about wrap-around?
		if (lsd <= msd) {
			// TODO: should handle this better
			return new BigDecimal("0");
		}
		ch = new char[lsd - msd + 1];
		int x = 0;
		byte b = sys.readMem(lsd);
		if ((b & 060) == 040) {
			ch[x++] = '-';
		} else {
			ch[x++] = '+';
		}
		int a = msd;
		while (a < lsd) {
			a = sys.incrAdr(a, 1);
			b = (byte)(sys.readMem(a) & 017);
			ch[x++] = (char)('0' + b);
			if (b != 0) ++nz;
			else if (nz == 0) ++lz;
		}
		BigDecimal bd = new BigDecimal(ch);
		return bd;
	}

	public static boolean nativeToHw(HW2000 sys, BigDecimal bd, int lsd, int msd) {
		// This string will have leading zeros suppressed...
		String num = bd.toPlainString();
		int x = 0;
		byte s = 020;
		if (num.charAt(x) == '-') {
			++x;
			s = 040;
		} else if (num.charAt(x) == '+') {
			++x;
		}
		int y = num.length() - 1;
		int a = lsd;
		byte c;
		byte z = 0;
		// TODO: verify space for result?
		while (y >= x) {
			c = (byte)(num.charAt(y) & 017);
			z |= c;
			sys.writeChar(a, (byte)(s | c));
			a = sys.incrAdr(a, -1);
			s = 000;
			--y;
		}
		while (a > msd) {
			sys.writeChar(a, (byte)0);
			a = sys.incrAdr(a, -1);
		}
		return (z == 0);
	}

	public void execute(HW2000 sys) {
		int a = I_M.fieldStart(sys, sys.AAR);
		// TODO: worry about wrap-around?
		int na = (sys.AAR - a);
		BigDecimal bda = I_M.hwToNative(sys, sys.AAR, a);
		int an = nz;
		sys.AAR = a;

		int b = I_M.fieldStart(sys, sys.BAR);
		int be = sys.incrAdr(sys.BAR, -(na + 1));
		BigDecimal bdb = I_M.hwToNative(sys, be , b);
		int bn = nz;

		bdb = bdb.multiply(bda);
		boolean zb = I_M.nativeToHw(sys, bdb, sys.BAR, b);
		sys.BAR = b;

		sys.CTL.setZB(zb);
		if (an > 0 && bn > 0) { // both are non-zero
			// a crude approximation - by this point
			// we've already done: Ni + 2Na + 2Nb + 1
			sys.addTics(2 * an * bn);
		} else {
			// Simple case P=0
			sys.addTics(5);
		}
	}
}
