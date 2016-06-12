public class P_LinePrinter implements Peripheral {

	public void io(HW2000 sys) {
		byte c3;
		if ((sys.op_xtra[1] & 030) == 010) {
			c3 = sys.op_xtra[3];
		} else {
			c3 = sys.op_xtra[2];
		}
		// C3:
		//	00nnnn: Print then advance nnnn lines.
		//	01nnnn: Print then advance nnnn lines unless HOF, etc.
		//	11nnnn: Do not print, advance nnnn lines.
		//	100xxx: Print, advance to channel xxx.
		//	101xxx: Do not print, advance to channel xxx.

		String s = "";
		boolean print = ((c3 & 040) == 0 || (c3 & 030) == 0);
		// Printing stops *before* char with record mark...
		while (print) {
			byte a = sys.readMem(sys.AAR);
			if ((a & 0300)  == 0300) {
				break;
			}
			a &= 077;
			s += sys.pdc.cvt.hwToLP(a);
			sys.incrAAR(1);
		}
		if ((c3 & 060) != 040) {
			c3 &= 017;
			while (c3 > 0) {
				s += "\n";
				--c3;
			}
		}
		System.out.format("%s", s);
	}

	public void ctl(HW2000 sys) {
	}
}
