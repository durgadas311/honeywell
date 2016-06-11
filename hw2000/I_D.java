import java.math.BigDecimal;

public class I_D implements Instruction {
	// Divide (decimal)
	public void execute(HW2000 sys) {
		int a = I_M.fieldStart(sys, sys.AAR);
		// TODO: worry about wrap-around?
		int na = (a - sys.AAR);
		BigDecimal bda = I_M.hwToNative(sys, sys.AAR, a);
		sys.AAR = a;

		// NOTE: BAR is not the rightmost char of B field...
		int b = I_M.fieldStart(sys, sys.BAR);
		BigDecimal bdb = I_M.hwToNative(sys, sys.BAR, b);
		int nb = (b - sys.BAR);

		BigDecimal[] bdr = bdb.divideAndRemainder(bda);
		// determine locations of results...
		int bq = sys.incrAdr(sys.BAR, -(na - nb + 2));
		int br = sys.incrAdr(sys.BAR, (nb - 1));
		boolean zb = I_M.nativeToHw(sys, bdr[0], bq, b);
		I_M.nativeToHw(sys, bdr[1], br, bq);
		sys.CTL.setZB(zb);
		sys.BAR = sys.incrAdr(bq, -1);
	}
}
