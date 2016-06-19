import java.io.*;

public class monrun {
	public static void main(String[] args) {
		int x = 0;
		boolean trace = false;
		String mon = "monitor.ezc";
		String lst = null;
		if (args.length > 0) {
			x = 0;
			if (args[x].equals("-t")) {
				trace = true;
				++x;
			}
			if (x < args.length) {
				mon = args[x];
				++x;
			}
		}
		HW2000 hw = new HW2000();

		// TODO: remove ".ezc"
		lst = mon + ".lst";
		hw.asmNGo(mon, lst, trace);
		for (; x < args.length; ++x) {
			if (args[x].equals("-t")) {
				trace = true;
				continue;
			}
			lst = args[x] + ".lst";
			hw.monGo(args[x], lst, trace);
		}
	}
}
