import java.util.Arrays;
import java.math.BigDecimal;

public class mcetest {
	static boolean zero;
	static boolean overflow;

	static CharConverter cvt;
	static void convert(HW2000 sys, String s, int ptr) {
		int a = ptr - s.length() + 1;
		byte w = (byte)0100;
		for (int x = 0; x < s.length(); ++x) {
			char c = s.charAt(x);
			byte b = cvt.asciiToHw((byte)c);
			if (b >= 0100) {
				b = (byte)015;
			}
			sys.writeMem(a, (byte)(w| b));
			w = 0;
			a = sys.incrAdr(a, 1);
		}
	}

	static String hwToString(HW2000 sys, int ptr) {
		String s = "";
		byte w = 0;
		int a = ptr;
		do {
			w = (byte)(sys.readMem(a) & 0100);
			a = sys.incrAdr(a, -1);
		} while (w == 0);
		a = sys.incrAdr(a, 1);
		while (a <=  ptr) {
			byte b = sys.readChar(a);
			a = sys.incrAdr(a, 1);
			String c = cvt.hwToLP(b);
			s += c;
		}
		return s;
	}

	public static void main(String[] args) {
		int x;
		if (args.length == 0 || (args.length & 1) != 0) {
			System.err.println("Usage: bdtest num fmt [...]");
			System.exit(1);
		}
		cvt = new CharConverter(new CardPunchOptions());
		HW2000 hw = new HW2000();
		int fmt, mem, msd;

		fmt = 64;
		mem = 128;
		Instruction mce = new I_MCE();

		for (x = 0; x < args.length; x += 2) {
			BigDecimal d1 = new BigDecimal(args[x]);
			msd = mem - args[x].length() + 1;
			hw.writeMem(msd, (byte)0100);
			I_M.nativeToHw(hw, d1, mem, msd);
			convert(hw, args[x + 1], fmt);
			hw.AAR = mem;
			hw.BAR = fmt;
			mce.execute(hw);
			System.err.format("%s => \"%s\": " +
				"%02o %02o %02o %02o %02o %02o %02o %02o " +
				"%02o %02o %02o %02o %02o %02o %02o %02o " +
				"\"%s\"\n",
				d1.toString(), args[x + 1],
				hw.readMem(fmt-15), hw.readMem(fmt-14),
				hw.readMem(fmt-13), hw.readMem(fmt-12),
				hw.readMem(fmt-11), hw.readMem(fmt-10),
				hw.readMem(fmt-9), hw.readMem(fmt-8),
				hw.readMem(fmt-7), hw.readMem(fmt-6),
				hw.readMem(fmt-5), hw.readMem(fmt-4),
				hw.readMem(fmt-3), hw.readMem(fmt-2),
				hw.readMem(fmt-1), hw.readMem(fmt),
				hwToString(hw, fmt));
		}
	}
}
