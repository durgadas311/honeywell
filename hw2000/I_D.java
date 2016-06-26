import java.math.BigDecimal;

public class I_D implements Instruction {
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
		sys.AAR = a;

		// NOTE: BAR is not the rightmost char of B field...
		// Also: BAR and 'b' relation is reversed...
		int br = I_D.dividBot(sys, sys.BAR);
		int be = sys.incrAdr(sys.BAR, -1);
		BigDecimal bdb = I_M.hwToNative(sys, br, be);
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
		} catch (Exception ee) {
			// assume divide-by-zero
			sys.CTL.setOVR(true);
		}
	}
}
