// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_C implements Instruction {
	// Compare

	// compares AAR to BAR...
	// returns true if BAR is shorter.
	public static byte compare(HW2000 sys, boolean enda) {
		byte a = sys.readMem(sys.AAR);
		sys.incrAAR(-1);
		byte b = sys.readMem(sys.BAR);
		sys.incrBAR(-1);
		byte aw = (byte)(a & 0100);
		byte bw = (byte)(b & 0100);
		byte c = 0;
		a &= 077;
		b &= 077;
		boolean lt = false;
		byte z = 0;
		while (true) {
			c = (byte)(a ^ b);
			z |= c;	// will be 0 at end if B=A
			if (c != 0) {
				// The last one prevails...
				lt = (a < b);
			}
			if (bw != 0) {
				break;
			}
			if (aw != 0) {
				if (enda) {
					break;
				}
				a = 0;
			} else {
				a = sys.readMem(sys.AAR);
				sys.incrAAR(-1);
				aw = (byte)(a & 0100);
				a &= 077;
			}
			b = sys.readMem(sys.BAR);
			sys.incrBAR(-1);
			bw = (byte)(b & 0100);
			b &= 077;
		}
		sys.CTL.setCompare(lt, (z == 0));
		if (bw != 0 && aw == 0) {
			return (byte)077;
		} else {
			return bw;
		}
	}

	public void execute(HW2000 sys) {
		I_C.compare(sys, false);
	}
}
