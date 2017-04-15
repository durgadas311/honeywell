// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.io.*;

public class f4 {
	public static void main(String[] args) {
		int x = 0;

		if (args.length > x) {
		}
		if (args.length - x < 1) {
			System.err.println("Usage: f4 <in-file> ..tbd..");
			System.exit(1);
		}
		String base = args[x];
		if (base.endsWith(".f4")) {
			base = base.substring(0, base.length() - 3);
		}
		String list = base + ".lst";
		String ezc = base + ".ezc";
		String out = base + ".out";
		Fortran4 cmp = new Fortran4(new File(args[x]));
		int e = cmp.compile(new File(list));
		if (e >= 0) {
			e = cmp.generate(new File(ezc), null);
		}
		if (e < 0) {
			System.err.println(cmp.getErrors());
			System.exit(1);
		}
		FileOutputStream fo = null;
		FileOutputStream lo = null;
		try {
			fo = new FileOutputStream(new File(out));
			lo = new FileOutputStream(new File(list), true);
		} catch (Exception ee) {
			ee.printStackTrace();
			System.exit(1);
		}
		Assembler asm = new Assembler(new File(ezc));
		Loader ldr;
		ldr = new RawLoader(fo, System.err, null, -1);
		e = asm.passOne();
		if (e >= 0) {
			e = asm.passTwo(ldr, lo);
		}
		if (e < 0) {
			System.err.println(asm.getErrors());
		}
		try { fo.close(); } catch (Exception ee) {}
		try { lo.close(); } catch (Exception ee) {}
	}
}
