public class I_BA implements Instruction {
	// Binary Add
	public void execute(HW2000 sys) {
		byte a = sys.readMem(sys.AAR);
		sys.incrAAR(-1);
		byte b = sys.readMem(sys.BAR);
		byte aw = (a & 0100);
		byte bw = (b & 0100);
		byte c = 0;
		a &= 077;
		b &= 077;
		byte cy = 0;
		boolean aDone = false;
		byte z = 0;
		while (true) {
			c = a + b + cy;
			cy = (c >> 6);
			c &= 077;
			z |= c;
			sys.writeChar(sys.BAR, c);
			sys.incrBAR(-1);
			if (bw != 0) {
				break;
			}
			aDone = (aDone || aw != 0);
			if (aDone) {
				a = 0;
			} else {
				a = sys.readMem(sys.AAR);
				sys.incrAAR(-1);
				aw = (a & 0100);
				a &= 077;
			}
			b = sys.readMem(sys.BAR);
			bw = (b & 0100);
			b &= 077;
		}
		sys.CTL.setZB(z == 0);
		if (cy != 0) {
			sys.CTL.setOVR(true);
		}
	}
}
