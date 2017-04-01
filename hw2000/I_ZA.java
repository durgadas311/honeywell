// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_ZA implements Instruction {
	// Zero and Add (decimal)

	public static void zero_add(HW2000 sys, boolean sub) {
		int bar;
		if (sys.hadA() && !sys.hadB()) {
			bar = sys.AAR;
		} else {
			bar = sys.BAR;
		}
		byte a = sys.readMem(sys.AAR);
		sys.incrAAR(-1);

		byte b = sys.readMem(bar);
		byte aw = (byte)(a & 0100);
		byte bw = (byte)(b & 0100);
		boolean negA = ((a & 0060) == 0040);
		if (sub) {
			negA = !negA;
		}
		a &= 017;
		byte s = (byte)(negA ? 040 : 020);
		boolean aDone = false;
		while (true) {
			if (sys.CTL.isS_MODE() && a >= 014) {
				a = 0;
			}
			// 'bar' might be following AAR, need to tolerate that.
			sys.writeChar(bar, (byte)(s | a));
			bar = sys.incrAdr(bar, -1);
			s = 0;
			if (bw != 0) {
				break;
			}
			aDone = (aDone || aw != 0);
			if (aDone) {
				a = 0;
			} else {
				a = sys.readMem(sys.AAR);
				sys.incrAAR(-1);
				aw = (byte)(a & 0100);
				a &= 017;
			}
			b = sys.readMem(bar);
			bw = (byte)(b & 0100);
		}
		if (!sys.hadA() || sys.hadB()) {
			sys.BAR = bar;
		}
	}

	public void execute(HW2000 sys) {
		I_ZA.zero_add(sys, false);
	}
}
