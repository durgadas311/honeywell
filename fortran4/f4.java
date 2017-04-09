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
		Fortran4 cmp = new Fortran4(new File(args[x]));
		int e = cmp.compile(null);
		if (e >= 0) {
			e = cmp.generate(new File("f4.ezc"), null);
		}
		if (e < 0) {
			System.err.println(cmp.getErrors());
		}
	}
}
