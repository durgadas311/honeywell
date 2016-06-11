public class I_MCE implements Instruction {
	// Move Characters and Edit
	public void execute(HW2000 sys) {
		int zar = sys.BAR; // should always get reset...
		byte a =  sys.readMem(sys.AAR);
		sys.incrAAR(-1);
		byte aw = (byte)(a & 0100);
		byte s = (byte)(a & 060);
		byte b =  sys.readMem(sys.BAR);
		byte bw = (byte)(b & 0100);
		b &= 077;
		a &= 017;
		boolean rescan = false;
		boolean zsup = false;
		boolean end;
		byte fill = 0;
		do {
			end = (bw != 0);
			boolean newa = false;
			switch(b) {
			case 000:	// '0'
				rescan = true;
				zsup = true;
				zar = sys.BAR;
				// FALLTHROUGH
			case 015:	// ' ' (blank)
				sys.writeMem(sys.BAR, (byte)(a & 017));
				newa = true;
				break;
			case 054:	// '*'
			case 053:	// '$'
				if (zsup) {
					fill = b;	// or float...
					sys.writeMem(sys.BAR, a);
					newa = true;
				} else {
					sys.writeMem(sys.BAR, b);
				}
				break;
			case 037:	// '?' a.k.a. '&'
				sys.writeMem(sys.BAR, (byte)015);
				break;

			case 075:	// 'cr'
			case 023:	// 'C'
			case 051:	// 'R'
			case 040:	// '-'
				sys.writeMem(sys.BAR, (byte)(s == 040 ? b : 015));
				break;

			case 073:	// ','
			case 033:	// '.'
			default:
				sys.writeMem(sys.BAR, b);
				break;
			}
			if (b != 000) {
				zsup = false;	// correct?
			}
			if (newa) {
				if (aw == 0) {
					a =  sys.readMem(sys.AAR);
					sys.incrAAR(-1);
					aw = (byte)(a & 0100);
				} else {
					a = 0;
				}
			}
			sys.incrBAR(-1);
			b =  sys.readMem(sys.BAR);
			bw = (byte)(b & 0100);
			b &= 077;
		} while (!end);
		sys.incrBAR(1);
		sys.setWord(sys.BAR); // already the case?
		if (!rescan) {
			return;
		}
		while (sys.BAR <= zar) {
			b =  sys.readMem(sys.BAR);
			b &= 077;
			if (b >= 001 && b <= 011) {
				if (fill == 053) {
					sys.incrBAR(-1);
					sys.writeChar(sys.BAR, fill);
				}
				//sys.setWord(sys.BAR);
				return;
			}
			if (fill == 054) {
				sys.writeChar(sys.BAR, fill);
			} else if (b == 000 || b == 073) { // others???
				sys.writeChar(sys.BAR, (byte)015);
			}
			sys.incrBAR(1);
		}
	}
}
