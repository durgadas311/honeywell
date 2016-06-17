import java.io.*;

public class ezc {
	public static void main(String[] args) {
		if (args.length < 1) {
			System.err.println("Usage: ezc <in-file> ..tbd..");
			System.exit(1);
		}
		Assembler asm = new Assembler(new File(args[0]));
		int e = asm.passOne();
		if (e >= 0) {
			e = asm.passTwo(new File("ezc.out"), new File("ezc.lst"));
		}
	}
}
