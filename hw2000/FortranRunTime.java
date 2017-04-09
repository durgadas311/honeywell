// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Map;
import java.util.HashMap;

public class FortranRunTime implements HW2000Trap {
	// HW codes for format specifiers that we might support
	static final int A = 021;
	static final int D = 024;
	static final int E = 025;
	static final int F = 026;
	static final int G = 027;
	static final int I = 031;
	static final int L = 043;
	static final int P = 047;
	static final int T = 063;
	static final int X = 067;
	static final int Z = 071;
	private String fmt;
	private String buf;
	private int idx;

	public FortranRunTime() {
	}

	public boolean doTrap(HW2000 sys) {
		if (sys.SR < 1340 || sys.SR > 1342) {
			return false;
		}
		int op = sys.rawReadMem(sys.SR);
		switch (op) {
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
			// our best guess...
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
		sys.SR = sys.BAR;
		int fmt = getAdr(sys);
		// Doesn't check IM...
		int t = sys.rawReadMem(sys.SR++) & 077;
		sys.CSR = 1342;
		if (t == 077) {
			// write record...
			// TODO: carriage control, etc...
			sys.listOut(buf + '\n');
			buf = null;
		} else {
			getFormat(sys, fmt);
		}
	}

	// Called by CSM!
	private void acboio_x(HW2000 sys) {
		sys.SR = sys.CSR;
		sys.CSR = 1342;
		int var = getAdr(sys);
		doParam(sys, var);
	}

	// Doesn't check IM...
	private int getAdr(HW2000 sys) {
		int a = 0;
		for (int n = 0; n < sys.am_na; ++n) {
			a = (a << 6) | (sys.rawReadMem(sys.SR++) & 077);
		}
		return a;
	}

	// Works backward until WM...
	private int getInt(HW2000 sys, int a) {
		int i = 0;
		int b;
		// TODO: re-use routines from instructions?
		for (b = a; b >= 0; --b) {
			if ((sys.rawReadMem(b) & 0100) != 0) {
				break;
			}
		}
		while (b <= a) {
			i = (i << 6) | (sys.rawReadMem(b++) & 077);
		}
		return i;
	}

	private void getFormat(HW2000 sys, int a) {
		// translate/interpret characters until IM...
		// TODO: handle implied-DO, etc...
		fmt = "";
		// TODO: limit scan!
		while (true) {
			int m = sys.rawReadMem(a++);
			fmt += sys.pdc.cvt.hwToLP((byte)(m & 077));
			if ((m & 0200) != 0) {
				break;
			}
		}
		idx = 0;
		buf = "";
		nextParam();
	}

	private void doParam(HW2000 sys, int a) {
		// For now, only 'I'...
		int c = fmt.charAt(idx++);
		if (c != 'I') {
			nextParam(); // certain to fail
			return;
		}
		int n = getNum();
		int val = getInt(sys, a);
		// TODO: field overflow
		String v = String.format("%%%dd", n);
		buf += String.format(v, val);
	}

	private void nextParam() {
		int count = 0;
		while (count < 2) {
			while (idx < fmt.length() && (fmt.charAt(idx) == ' ' ||
					fmt.charAt(idx) == ',')) {
				++idx;
			}
			if (idx >= fmt.length()) {
				idx = 0;
				++count;
				continue;
			}
			int c = fmt.charAt(idx);
			if (Character.isDigit(c)) {
				// must be nnHccccc... (or ???)
				copyHollerith();
			} else if (c == '\'') {
				// TODO: handle quoted string?
				copyQuoted();
			} else {
				// stop and wait for param...
				break;
			}
		}
	}

	private int getNum() {
		int n = 0;
		while (idx < fmt.length() &&
				Character.isDigit(fmt.charAt(idx))) {
			n = (n * 10) + (fmt.charAt(idx++) - '0');
		}
		return n;
	}

	private void copyHollerith() {
		int n = getNum();
		// assert fmt.charAt(idx) == 'H'...
		++idx; // skip 'H'
		while (idx < fmt.length() && n > 0) {
			buf += fmt.charAt(idx++);
			--n;
		}
		while (n > 0) {
			buf += ' ';
			--n;
		}
	}

	private void copyQuoted() {
	}
}
