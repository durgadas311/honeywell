// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Map;
import java.util.HashMap;

public class FortranRunTime implements HW2000Trap {
	static final int base = 1340;
	static final int numOps = 5;

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
		if (sys.SR < base || sys.SR - base >= numOps) {
			return false;
		}
		int op = sys.rawReadMem(sys.SR);
		switch (op) {
		case 0:
			exit(sys);
			break;
		case 1:
			acboio(sys);	// start/end I/O
			break;
		case 2:
			acboio_x(sys); // each parameter in I/O list...
			break;
		case 3:
			acbfph(sys);	// floating point assist, h/w
			break;
		case 4:
			acbfxp(sys);	// fixed-point assist
			break;
		default:
			// our best guess...
			sys.SR = sys.BAR;
			break;
		}
		return true;
	}

	private void exit(HW2000 sys) {
		// System.err.format("exit %07o\n", sys.SR);
		// TODO: remove traps...
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

	private void acbfph(HW2000 sys) {
		sys.SR = sys.BAR;
		int a = getAdr(sys);
		int b = getAdr(sys);
		int fnc = sys.rawReadMem(sys.SR++) & 077;
		double l = getReal(sys, a);
		double r = getReal(sys, b);
		switch (fnc) {
		case 016:	// add
			r = l + r;
			break;
		case 017:	// subtract
			r = l - r;
			break;
		case 020:	// multiply
			r = l * r;
			break;
		case 021:	// divide
			r = l / r;
			break;
		case 022:	// pwer
			r = Math.pow(l, r);
			break;
		}
		putReal(sys, b, r);
	}

	private void acbfxp(HW2000 sys) {
		sys.SR = sys.BAR;
		int a = getAdr(sys);
		int b = getAdr(sys);
		int fnc = sys.rawReadMem(sys.SR++) & 077;
		int l = getInt(sys, a);
		int r = getInt(sys, b);
		switch (fnc) {
		case 020:	// multiply
			r = l * r;
			break;
		case 021:	// divide
			r = l / r;
			break;
		case 022:	// pwer
			r = pow(l, r);
			break;
		}
		putInt(sys, b, r);
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
	private void putInt(HW2000 sys, int a, int v) {
		int b;
		// TODO: re-use routines from instructions?
		for (b = a; b >= 0; --b) {
			sys.rawWriteChar(b, (byte)(v & 077));
			v >>= 6;
			if ((sys.rawReadMem(b) & 0100) != 0) {
				break;
			}
		}
	}
	private double getReal(HW2000 sys, int a) {
		return I_FMA.hwToNative(sys, a);
	}
	private void putReal(HW2000 sys, int a, double v) {
		I_FMA.nativeToHw(sys, v, false, a);
	}

	private int pow(int b, int e) {
		if (e < 0) {
			return 0;
		}
		if (e == 0) {
			return 1;
		}
		if (e == 1) {
			return b;
		}
		int r = 1;
		while (e > 0) {
			if ((e & 1) != 0) {
				r *= b;
			}
			e >>= 1;
			b *= b;
		}
		return r;
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
	}

	private void doParam(HW2000 sys, int a) {
		nextParam();
		// For now, only 'I'...
		int c = fmt.charAt(idx++);
		int n;
		int m;
		double dd;
		int val;
		String v;
		switch (c) {
		case 'I':
			n = getNum();
			val = getInt(sys, a);
			v = String.format("%%%dd", n);
			v = String.format(v, val);
			break;
		case 'L':
			n = getNum();
			val = getInt(sys, a);
			v = String.format("%%%ds", n);
			v = String.format(v, val != 0 ? "T" : "F");
			break;
		case 'F':
			c = 'f';
			// FALLTHROUGH
		case 'E':
			n = getNum();
			if (fmt.charAt(idx) == '.') {
				++idx;
				m = getNum();
			} else {
				m = 0;
			}
			dd = getReal(sys, a);
			v = String.format("%%%d.%d%c", n, m, c);
			v = String.format(v, dd);
			break;
		default:
			nextParam(); // certain to fail
			return;
		}
		if (v.length() > n) {
			v = "*";
			while (v.length() < n) {
				v += '*';
			}
		}
		buf += v;
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
