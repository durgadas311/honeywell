// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Map;
import java.util.HashMap;

public class FortranRunTime implements HW2000Trap {
	public FortranRunTime() {
	}

	public boolean doTrap(HW2000 sys) {
		if (sys.SR < 1340 || sys.SR > 1342) {
			return false;
		}
		switch (sys.rawReadMem(sys.SR)) {
		case 0:
			exit(sys);
			break;
		case 1:
			acboio(sys);
			break;
		case 2:
			acboio_x(sys);
			break;
		default:
			sys.SR = sys.BAR;
			break;
		}
		return true;
	}

	private void exit(HW2000 sys) {
		System.err.format("exit %07o\n", sys.SR);
		// remove traps...
		sys.SR = sys.BAR;
	}

	private void acboio(HW2000 sys) {
		byte[] b;
		int a = sys.BAR;
		// TODO: limit scan!
		while ((sys.rawReadMem(a++) & 0200) == 0);
		b = new byte[a - sys.BAR];
		int x = 0;
		for (int z = sys.BAR; z < a; ++z) {
			b[x++] = sys.rawReadMem(z);
		}
		int t = b[x - 1] & 077;
		System.err.format("acboio %02o\n", t);
		if (t == 077) {
			sys.CSR = 0;
		} else {
			sys.CSR = 1342;
		}
		sys.SR = a;
		//sys.halt = true;
	}

	private void acboio_x(HW2000 sys) {
		System.err.format("acboio_x %07o\n", sys.CSR);
		sys.SR = sys.CSR;
		sys.CSR = 1342;
	}
}
