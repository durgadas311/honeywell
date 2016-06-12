public class bintest {
	static boolean zero;
	static boolean overflow;

	static int len;
	static Long get(String s) {
		Long li;
		if (s.startsWith("0x")) {
			len = ((s.length() - 2) * 4 + 5) / 6;
			li = Long.parseLong(s.substring(2), 16);
		} else if (s.startsWith("0o")) {
			len = ((s.length() - 2) * 3 + 5) / 6;
			li = Long.parseLong(s.substring(2), 8);
		} else if (s.startsWith("0b")) {
			len = ((s.length() - 2) * 1 + 5) / 6;
			li = Long.parseLong(s.substring(2), 2);
		} else {
			len = (s.length() * 35 + 59) / 60;
			li = Long.parseLong(s);
		}
		return li;
	}

	public static void main(String[] args) {
		int x;
		if (args.length < 1) {
			System.err.println("Usage: bintest [op] init-val [num...]");
			System.exit(1);
		}
		x = 0;
		String opc = "+";
		Instruction op;
		if (args[x].equals("-")) {
			++x;
			op = new I_BS();
			opc = "-";
		} else {
			if (args[x].equals("+")) {
				++x;
			}
			op = new I_BA();
		}
		HW2000 hw = new HW2000();
		int sum, mem, msd;

		sum = 64;
		mem = 128;
		Long d0 = null;
		Long d1 = null;

		for (;x < args.length; ++x) {
			if (d0 == null) {
				d0 = get(args[x]);
				msd = sum - len + 1;
				hw.setWord(msd);
				I_BA.nativeToHw(hw, d0, sum, msd);
				System.err.format("  %s => ... %02o %02o %02o %02o %02o %02o %02o %02o\n",
					d0.toString(),
					hw.readMem(sum-7), hw.readMem(sum-6),
					hw.readMem(sum-5), hw.readMem(sum-4),
					hw.readMem(sum-3), hw.readMem(sum-2),
					hw.readMem(sum-1), hw.readMem(sum));
			} else {
				d1 = get(args[x]);
				msd = mem - len + 1;
				hw.setWord(msd);
				I_BA.nativeToHw(hw, d1, mem, msd);
				System.err.format("%s %s => ... %02o %02o %02o %02o %02o %02o %02o %02o\n",
					opc, d1.toString(),
					hw.readMem(mem-7), hw.readMem(mem-6),
					hw.readMem(mem-5), hw.readMem(mem-4),
					hw.readMem(mem-3), hw.readMem(mem-2),
					hw.readMem(mem-1), hw.readMem(mem));
				hw.AAR = mem;
				hw.BAR = sum;
				op.execute(hw);
				hw.writeMem(msd, (byte)0);
			}
		}
		msd = I_M.fieldStart(hw, sum) + 1;
		Long d2 = I_BA.hwToNative(hw, sum, msd);
		System.err.format("= %s => ... %02o %02o %02o %02o %02o %02o %02o %02o\n",
				d2.toString(),
				hw.readMem(sum-7), hw.readMem(sum-6),
				hw.readMem(sum-5), hw.readMem(sum-4),
				hw.readMem(sum-3), hw.readMem(sum-2),
				hw.readMem(sum-1), hw.readMem(sum));
	}
}
