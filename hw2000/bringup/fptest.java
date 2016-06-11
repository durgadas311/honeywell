public class fptest {

	public static void main(String[] args) {
		int x;
		HW2000 hw = new HW2000();
		int mem;

		mem = 64;
		for (x = 0; x < args.length; ++x) {
			double d1 = Double.valueOf(args[x]);
			I_FMA.nativeToHw(hw, d1, mem);
			double d2 = I_FMA.hwToNative(hw, mem);
			System.out.format("%g => %02o %02o %02o %02o %02o %02o %02o %02o => %g\n",
				d1,
				hw.readMem(mem-7), hw.readMem(mem-6),
				hw.readMem(mem-5), hw.readMem(mem-4),
				hw.readMem(mem-3), hw.readMem(mem-2),
				hw.readMem(mem-1), hw.readMem(mem),
				d2);
		}
	}
}
