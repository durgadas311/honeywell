// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.io.*;

public class ezc {
	public static void main(String[] args) {
		boolean cards = false;
		boolean raw = false;
		boolean bin = false;
		boolean rawSW = false;
		int x = 0;

		if (args.length > x) {
			if (args[x].equals("-c")) {
				++x;
				cards = true;
			} else if (args[x].equals("-b")) {
				++x;
				bin = true;
			} else if (args[x].equals("-B")) {
				++x;
				bin = true;
				rawSW = true;
			} else if (args[x].equals("-r")) {
				++x;
				raw = true;
			} else if (args[x].equals("-R")) {
				++x;
				raw = true;
				rawSW = true;
			}
		}
		if (args.length - x < 1) {
			System.err.println("Usage: ezc [-c] <in-file> ..tbd..");
			System.exit(1);
		}
		FileOutputStream fo = null;
		FileOutputStream lo = null;
		try {
			fo = new FileOutputStream(new File("ezc.out"));
			lo = new FileOutputStream(new File("ezc.lst"));
		} catch (Exception ee) {
			ee.printStackTrace();
			System.exit(1);
		}
		Assembler asm = new Assembler(new File(args[x]));
		Loader ldr;
		if (cards) {
			ldr = new CardLoader(fo, asm.charCvt());
		} else if (raw || bin) {
			ldr = new RawLoader(fo, System.err, rawSW ? asm : null, bin ? 250 : -1);
		} else {
			ldr = new TapeLoader(fo, asm.charCvt());
		}
		int e = asm.passOne();
		if (e >= 0) {
			e = asm.passTwo(ldr, lo);
		}
		if (e < 0) {
			System.err.println(asm.getErrors());
		}
		try { fo.close(); } catch (Exception ee) {}
	}
}
