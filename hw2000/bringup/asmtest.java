import java.io.*;

public class asmtest {
	public static void main(String[] args) {
		int x;
		boolean trace = false;
		String pgm = "machin.ezc";
		String lst = null;
		if (args.length > 0) {
			x = 0;
			if (args[x].equals("-t")) {
				trace = true;
				++x;
			}
			if (x < args.length) {
				pgm = args[x];
				++x;
			}
			if (x < args.length) {
				lst = args[x];
				++x;
			}
		}
		HW2000 hw = new HW2000();

		hw.asmNGo(pgm, lst, trace);
	}
}
