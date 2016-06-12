
public class pitest {
	public static void main(String[] args) {
		int x;
		boolean trace = false;
		String pgm = "machin_obj.bin";
		if (args.length > 0) {
			x = 0;
			if (args[x].equals("-t")) {
				trace = true;
				++x;
			}
			pgm = args[x];
		}
		HW2000 hw = new HW2000();

		hw.loadNGo(pgm, HW2000CCR.AIR_AM_2C, 02027, trace);
	}
}
