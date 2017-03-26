import java.io.*;

public class ezc {
	public static void main(String[] args) {
		boolean cards = false;
		int x = 0;
		if (args.length > x && args[x].equals("-c")) {
			++x;
			cards = true;
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
		int e = asm.passOne();
		if (e >= 0) {
			e = asm.passTwo(fo, cards, lo);
		}
		try { fo.close(); } catch (Exception ee) {}
	}
}
