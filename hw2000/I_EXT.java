// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_EXT implements Instruction {
	// EXTract (logical product, i.e. AND)
	public void execute(HW2000 sys) {
		byte a = sys.readMem(sys.AAR);
		sys.incrAAR(-1);
		byte b = sys.readMem(sys.BAR);
		byte aw = (byte)(a & 0100);
		byte bw = (byte)(b & 0100);
		byte c = 0;
		a &= 077;
		b &= 077;
		while (true) {
			c = (byte)(a & b);
			sys.writeChar(sys.BAR, c);
			sys.incrBAR(-1);
			if (bw != 0 || aw != 0) {
				break;
			}
			a = sys.readMem(sys.AAR);
			sys.incrAAR(-1);
			aw = (byte)(a & 0100);
			a &= 077;
			b = sys.readMem(sys.BAR);
			bw = (byte)(b & 0100);
			b &= 077;
		}
	}
}
