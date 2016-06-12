public class I_MOS implements Instruction {
	// Move Or Scan
	public void execute(HW2000 sys) {
		if (sys.hadB() && sys.numXtra() > 0) {
			sys.CTL.setV(sys.getXtra(0));
		}
		byte v = sys.CTL.getV();
		byte m = 0;	// bits to copy
		byte k1 = (byte)0200;	// terminal character sensed ign WM
		byte k2 = (byte)0200;	// terminal character sensed w/WM
		byte ka = (byte)0100;	// terminal punctuation sensed
		byte kb = (byte)0100;	// terminal punctuation sensed
		int i = -1;	// direction
		int n = -1;	// count (infinity)

		switch(v & 007) {
		case 000:
			// no data - must detect later "m == 000"
			break;
		case 001:
			m = (byte)0017;
			break;
		case 002:
			m = (byte)0060;
			break;
		case 003:
			m = (byte)0277;
			break;
		case 004:
			m = (byte)0100;
			break;
		case 005:
			m = (byte)0117;
			break;
		case 006:
			m = (byte)0160;
			break;
		case 007:
			m = (byte)0377;
			break;
		}
		switch(v & 070) {
		case 000:
			n = 1;
			break;
		case 010:
			i = 1;
			break;
		case 020:
			kb = 0;
			break;
		case 030:
			k1 = (byte)0073;
			ka = 0;
			kb = 0;
			i = 1;
			break;
		case 040:
			ka = 0;
			break;
		case 050:
			k2 = (byte)0132;
			kb = 0;
			i = 1;
			break;
		case 060:
			// defaults
			break;
		case 070:
			i = 1;
			k1 = (byte)0073;
			k2 = (byte)0132;
			break;
		}

		byte a;
		byte b;
		do {
			a = sys.readMem(sys.AAR);
			b = sys.readMem(sys.AAR);
			if (m != 0) {
				byte c = (byte)((b & ~m) | (a & m));
				sys.writeMem(sys.BAR, c);
			}
			sys.incrAAR(i);
			sys.incrBAR(i);
			--n;
		} while (n != 0 &&
			(a & ka) == 0 && (b & kb) == 0 &&
			(a & 077) != k1 && (a & 0177) != k2);
	}
}
