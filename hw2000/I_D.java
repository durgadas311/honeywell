// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.math.BigDecimal;

public class I_D implements Instruction {
	public String mnem() { return "D"; }
	// Divide (decimal)

	public static int dividBot(HW2000 sys, int adr) {
		int a = adr;
		while ((sys.readMem(a) & 060) == 0) {
			a = sys.incrAdr(a, 1);
		}
		return a;
	}

	public void execute(HW2000 sys) {
		int a = I_M.fieldStart(sys, sys.AAR);
		// TODO: worry about wrap-around?
		int na = (sys.AAR - a);
		BigDecimal bda = I_M.hwToNative(sys, sys.AAR, a);
		int al = I_M.lz;
		sys.AAR = a;
		if (al == na) { // divisor is 0
			sys.addTics(4 + na);
			sys.CTL.setOVR(true);
			return;
		}

		// NOTE: BAR is not the rightmost char of B field...
		// Also: BAR and 'b' relation is reversed...
		int br = I_D.dividBot(sys, sys.BAR);
		int be = sys.incrAdr(sys.BAR, -1);
		BigDecimal bdb = I_M.hwToNative(sys, br, be);
		int bl = I_M.lz;
		int nb = (br - be);

		BigDecimal[] bdr;
		try {
			bdr = bdb.divideAndRemainder(bda);
			// determine locations of results...
			int bq = sys.incrAdr(sys.BAR, -(na - nb + 2));
			int bt = sys.incrAdr(sys.BAR, -(na + 1));
			boolean zb = I_M.nativeToHw(sys, bdr[0], bq, bt);
			I_M.nativeToHw(sys, bdr[1], br, bq);
			sys.CTL.setZB(zb);
			sys.BAR = sys.incrAdr(bq, -1);
			// a crude approximation - by this point
			// we've already done: Ni + 2Na + 2Nb + 1
			if ((nb - bl) >= (na - al)) {
				// had to actually perform division
				sys.addTics(16 + 15 * (nb - bl) * (na - al));
			} else {
				// Simple case Q=0,R=B (includes B=0)
				sys.addTics(5);
			}
		} catch (Exception ee) {
			// assume divide-by-zero
			sys.CTL.setOVR(true);
		}
	}
}
