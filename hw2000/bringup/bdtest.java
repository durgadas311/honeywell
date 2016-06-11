import java.util.Arrays;
import java.math.BigDecimal;

public class bdtest {
	static boolean zero;
	static boolean overflow;

	public static void main(String[] args) {
		int x;
		if (args.length < 1) {
			System.err.println("Usage: bdtest init-val [num...]");
			System.exit(1);
		}
		HW2000 hw = new HW2000();
		int sum, mem;

		sum = 64;
		mem = 128;
		BigDecimal d0 = new BigDecimal(args[0]);
		int msd = sum - args[0].length() + 1;
		hw.writeMem(msd, (byte)0100);
		I_M.nativeToHw(hw, d0, sum, msd);
		System.err.format("  %s\n", d0.toString());
		Instruction add = new I_A();

		for (x = 1; x < args.length; ++x) {
			BigDecimal d1 = new BigDecimal(args[x]);
			msd = mem - args[x].length() + 1;
			hw.writeMem(msd, (byte)0100);
			I_M.nativeToHw(hw, d1, mem, msd);
			System.err.format("+ %s => ... %02o %02o %02o %02o %02o %02o %02o %02o\n",
				d1.toString(),
				hw.readMem(mem-7), hw.readMem(mem-6),
				hw.readMem(mem-5), hw.readMem(mem-4),
				hw.readMem(mem-3), hw.readMem(mem-2),
				hw.readMem(mem-1), hw.readMem(mem));
			hw.AAR = mem;
			hw.BAR = sum;
			add.execute(hw);
		}
		BigDecimal d2 = I_M.hwToNative(hw, sum, 0);
		System.err.format("= %s => ... %02o %02o %02o %02o %02o %02o %02o %02o\n",
				d2.toString(),
				hw.readMem(sum-7), hw.readMem(sum-6),
				hw.readMem(sum-5), hw.readMem(sum-4),
				hw.readMem(sum-3), hw.readMem(sum-2),
				hw.readMem(sum-1), hw.readMem(sum));
		System.err.format("zero: %s overflow: %s\n", hw.CTL.isZB(), hw.CTL.isOVR());
	}
}
