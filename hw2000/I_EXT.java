public class I_EXT implements Instruction {
	// EXTract (logical product, i.e. AND)
	public void execute(HW2000 sys) {
		byte a = sys.readMem(sys.AAR);
		sys.incrAAR(-1);
		byte b = sys.readMem(sys.BAR);
		byte aw = (a & 0100);
		byte bw = (b & 0100);
		byte c = 0;
		a &= 077;
		b &= 077;
		while (true) {
			c = a & b;
			sys.writeChar(sys.BAR, c);
			sys.incrBAR(-1);
			if (bw != 0 || aw != 0) {
				break;
			}
			a = sys.readMem(sys.AAR);
			sys.incrAAR(-1);
			aw = (a & 0100);
			a &= 077;
			b = sys.readMem(sys.BAR);
			bw = (b & 0100);
			b &= 077;
		}
	}
}
