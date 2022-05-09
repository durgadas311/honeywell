// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_EXM implements Instruction {
	public String mnem() { return "EXM"; }
	// EXtended Move
	public void execute(HW2000 sys) {
		if (sys.hadB() && sys.numXtra() > 0) {
			sys.CTL.setV(sys.getXtra(0));
		}
		byte v = sys.CTL.getV();
		byte m = 0;	// bits to copy
		byte k = 0;	// terminal punctuation sensed
		int i = -1;	// direction
		int n = -1;	// count (infinity)
		if ((v & 001) != 0) {
			m |= 0077;
		}
		if ((v & 002) != 0) {
			m |= 0100;
		}
		if ((v & 004) != 0) {
			m |= 0200;
		}
		if ((v & 010) != 0) {
			i = 1;
		}
		if ((v & 060) == 0) {
			n = 1;
		} else {
			k = (byte)((v & 060) << 2);	// into punctuation position...
		}
		byte a;
		do {
			a = sys.readMem(sys.AAR);
			sys.incrAAR(i);
			sys.writeMemMask(sys.BAR, a, (byte)~m);
			sys.incrBAR(i);
			--n;
		} while (n != 0 && (a & k) != k);
	}
}
