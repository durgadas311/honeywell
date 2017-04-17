// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Map;
import java.util.HashMap;
import java.util.Vector;

public class FortranRunTime implements HW2000Trap {
	static final String name = "FORTRAN";
	private int base = 0;
	private int numOps = 0;
	private int comm = 0;
	private int commLen = 0;

	private String buf;
	private int idx;
	private int rep;
	private int ip;
	private int dev;
	private int unit;
	private int ioflgs = 0;
	private Peripheral perph;
	SequentialRecordIO sqio = null;
	private boolean input;
	private FormatSpec[] fmt;
	private int fmtAdr;
	private HW2000 sys;

	public FortranRunTime(HW2000 sys) {
		this.sys = sys;
		sys.SR += name.length();
		base = getAdr();
		comm = getAdr();
		int e = getAdr();
		numOps = comm - base;
		commLen = e - comm;
	}

	public String getName() { return name; }
	static public String name() { return name; }
	static public boolean check(HW2000 sys) {
		if ((sys.rawReadMem(sys.SR) & 0100) == 0 ||
			(sys.rawReadMem(sys.SR + name.length()) & 0100) == 0) {
			return false;
		}
		String s = "";
		for (int a = 0; a < name.length(); ++a) {
			s += sys.pdc.cvt.hwToLP((byte)(sys.rawReadMem(sys.SR + a) & 077));
		}
		return name.equals(s);
	}

	public boolean doTrap() {
		if (sys.SR < base || sys.SR - base >= numOps) {
			return false;
		}
		int op = sys.rawReadMem(sys.SR);
		switch (op) {
		case 0: exit(); break;
		case 1: acboio(); break;	// start/end I/O
		case 2: acboio_x(); break;	// each parameter in I/O list...
		case 3: acbfph(); break;	// floating point assist, h/w
		case 4: acbfxp(); break;	// fixed-point assist
		case 5: eof(); break;
		case 6: eot(); break;
		case 7: endfile(); break;
		case 8: rewind(); break;
		case 9: backspace(); break;
		case 10: aint(); break;
		case 11: iint(); break;
		case 12: sqrt(); break;
		case 13: iand(); break;
		case 14: ior(); break;
		case 15: icompl(); break;
		case 16: iexclr(); break;
		case 17: rfloat(); break;
		case 18: ifix(); break;
		case 19: rabs(); break;
		case 20: iabs(); break;
		case 21: atan(); break;
		case 22: atan2(); break;
		case 23: cos(); break;
		case 24: sin(); break;
		case 25: tanh(); break;
		case 26: alog(); break;
		case 27: alog10(); break;
		case 28: amod(); break;
		case 29: mod(); break;
		case 30: exp(); break;
		default:
			// our best guess...
			sys.SR = sys.BAR;
			break;
		}
		return true;
	}

	public void done() {
		if (sqio != null) {
			sqio.end();
			sqio = null;
		}
	}

	private void exit() {
		// System.err.format("exit %07o\n", sys.SR);
		sys.removeTrap(this);
		sys.SR = sys.BAR;
	}

	private void acboio() {
		sys.SR = sys.BAR;
		fmtAdr = getAdr();
		// Doesn't check IM...
		int t = sys.rawReadMem(sys.SR++) & 077;
		sys.CSR = 1342;
		if (t == 077) {
			if (!input) {
				// write record...
				// If we got no parameters, must be "constant" format.
				// copy to output.
				if (idx == 0 && buf.length() == 0) {
					nextParam(false);
				}
				dispatchOutput();
			}
			buf = null;
			if (sqio != null) {
				sqio.end();
				sqio = null;
			}
		} else {
			ioflgs = 0;
			sys.rawWriteChar(comm + 0, (byte)ioflgs);
			// TODO: how to determine READ vs WRITE
			input = (t & 040) != 0;
			dev = (t >> 3) & 003; // TODO: '3' is not a std addr...
			if (input) {
				dev |= 040;
			}
			unit = (t & 007);
			perph = sys.pdc.getPeriph((byte)dev);
			if (perph == null) {
				return;
			}
			if (perph instanceof SequentialRecordIO) {
				sqio = (SequentialRecordIO)perph;
				sqio.begin(unit);
			}
			getFormat(fmtAdr);
			if (input) {
				// read record...
				dispatchInput();
				ip = 0;
			}
		}
	}

	// Called by CSM!
	private void acboio_x() {
		sys.SR = sys.CSR;
		sys.CSR = 1342;
		int var = getAdr();
		if (perph == null) {
			return;
		}
		doParam(var);
	}

	private void acbfph() {
		sys.SR = sys.BAR;
		int a = getAdr();
		int b = getAdr();
		int fnc = sys.rawReadMem(sys.SR++) & 077;
		double l = getReal(a);
		double r = getReal(b);
		switch (fnc) {
		case 015:	// negate
			r = -l;
			break;
		case 016:	// add
			r = r + l;
			break;
		case 017:	// subtract
			r = r - l;
			break;
		case 020:	// multiply
			r = r * l;
			break;
		case 021:	// divide
			r = r / l;
			break;
		case 022:	// power
			r = Math.pow(r, l);
			break;
		}
		putReal(b, r);
	}

	private void acbfxp() {
		sys.SR = sys.BAR;
		int a = getAdr();
		int b = getAdr();
		int fnc = sys.rawReadMem(sys.SR++) & 077;
		int l = getInt(a);
		int r = getInt(b);
		switch (fnc) {
		case 020:	// multiply
			r = l * r;
			break;
		case 021:	// divide
			r = l / r;
			break;
		case 022:	// power
			r = pow(l, r);
			break;
		}
		putInt(b, r);
	}

	private void aint() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		double l = getReal(a);
		l = Math.floor(l);
		putReal(r, l);
	}

	private void iint() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		double l = getReal(a);
		int i = (int)Math.floor(l);
		putInt(r, i);
	}

	private void ifix() {
		iint();
	}

	private void rfloat() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		double l = getInt(a);
		putReal(r, l);
	}

	private void sqrt() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		double l = getReal(a);
		l = Math.sqrt(l);
		putReal(r, l);
	}

	private void rabs() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		double l = getReal(a);
		l = Math.abs(l);
		putReal(r, l);
	}

	private void iabs() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		int i = getInt(a);
		i = Math.abs(i);
		putInt(r, i);
	}

	private void iand() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		int b = getAdr();
		putInt(r, getInt(a) & getInt(b));
	}

	private void ior() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		int b = getAdr();
		putInt(r, getInt(a) | getInt(b));
	}

	private void icompl() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		putInt(r, ~getInt(a));
	}

	private void iexclr() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		int b = getAdr();
		putInt(r, getInt(a) ^ getInt(b));
	}

	private void atan() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		double l = getReal(a);
		l = Math.atan(l);
		putReal(r, l);
	}
	private void atan2() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		int b = getAdr();
		double l = getReal(a);
		double rr = getReal(b);
		l = Math.atan2(rr, l); // what's the right order?
		putReal(r, l);
	}
	private void cos() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		double l = getReal(a);
		l = Math.cos(l);
		putReal(r, l);
	}
	private void sin() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		double l = getReal(a);
		l = Math.sin(l);
		putReal(r, l);
	}
	private void tanh() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		double l = getReal(a);
		l = Math.tanh(l);
		putReal(r, l);
	}
	private void alog() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		double l = getReal(a);
		l = Math.log(l);
		putReal(r, l);
	}
	private void alog10() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		double l = getReal(a);
		l = Math.log10(l);
		putReal(r, l);
	}
	private void amod() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		int b = getAdr();
		double l = getReal(a);
		double rr = getReal(b);
		putReal(r, l % rr);
	}
	private void mod() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		int b = getAdr();
		int l = getInt(a);
		int rr = getInt(b);
		putInt(r, l % rr);
	}
	private void exp() {
		sys.SR = sys.BAR;
		int r = getAdr();
		int a = getAdr();
		double l = getReal(a);
		l = Math.exp(l);
		putReal(r, l);
	}

	private void eof() {
		sys.SR = sys.BAR;
		int a = getAdr();
		int e = ((ioflgs & 001) != 0 ? 1 : 2);
		putInt(a, e);
	}
	private void eot() {
		sys.SR = sys.BAR;
		int a = getAdr();
		int e = ((ioflgs & 002) != 0 ? 1 : 2);
		putInt(a, e);
	}
	private void tapeCtl(char cmd) {
		sys.SR = sys.BAR;
		int t = sys.rawReadMem(sys.SR++) & 077;
		dev = (t >> 3) & 003; // TODO: '3' is not a std addr...
		unit = (t & 007);
		perph = sys.pdc.getPeriph((byte)dev);
		if (!(perph instanceof P_MagneticTape)) {
			return;
		}
		SequentialRecordIO sqio = (SequentialRecordIO)perph;
		sqio.begin(unit);
		switch (cmd) {
		case 'R':
			sqio.rewind();
			break;
		case 'B':
			sqio.backspace();
			break;
		case 'E':
			sqio.appendRecord(null, 0, 0);
			sqio.appendRecord(null, 0, 0);
			break;
		}
		sqio.end();
	}
	private void endfile() {
		tapeCtl('E');
	}
	private void rewind() {
		tapeCtl('R');
	}
	private void backspace() {
		tapeCtl('B');
	}

	// Doesn't check IM...
	private int getAdr() {
		int a = fetchAdr(sys.SR);
		sys.SR += sys.am_na;
		return a;
	}

	// This is a clone of HW2000.fetchAddr() - keep in sync!
	private int fetchAdr(int p) {
		int a = 0;
		for (int n = 0; n < sys.am_na; ++n) {
			a = (a << 6) | (sys.rawReadMem(p++) & 077);
		}
		int x = (a >> sys.am_shift);
		a &= sys.am_mask; // TODO: need? | (ref & ~am_mask);
		if (x == 0) {
			return a;
		}
		if (sys.am_na == 3 && x == 0x07 || x == 0x10) {
			return fetchAdr(a);
		}
		int ix = ((x & 0x0f) * 4) - sys.am_na + 1;
		if (x > 0x10) {
			if (!sys.CTL.isRELOC()) {
				ix += (sys.IBR << 12);
			}
		} else if (sys.am_na == 4) {
			// all set for X1-X15?
		} else {
			if (!sys.CTL.isRELOC()) {
				ix += (sys.SR & ~0x07fff);
			}
		}
		if (sys.CTL.isRELOC()) {
			ix += (sys.BRR << 12);
		}
		a += fetchAdr(ix);
		a &= sys.am_mask; // need ref?
		return a;
	}

	// Search backward until WM... TODO: is that right?
	private String getStr(int a) {
		String s = "";
		int b;
		for (b = a; b >= 0; --b) {
			if ((sys.rawReadMem(b) & 0100) != 0) {
				break;
			}
		}
		while (b <= a) {
			s += sys.pdc.cvt.hwToLP((byte)(sys.rawReadMem(b++) & 077));
		}
		return s;
	}

	private void putStr(int a, String s) {
		int b;
		for (b = a; b >= 0; --b) {
			if ((sys.rawReadMem(b) & 0100) != 0) {
				break;
			}
		}
		int x = 0;
		while (b <= a) {
			byte bb = (byte)015; // blank space
			if (x < s.length()) {
				bb = sys.pdc.cvt.asciiToHw((byte)s.charAt(x++));
			}
			sys.rawWriteChar(b, bb);
		}
	}

	// Works backward until WM...
	private int getInt(int a) {
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
	private void putInt(int a, int v) {
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
	private double getReal(int a) {
		return I_FMA.hwToNative(sys, a);
	}
	private void putReal(int a, double v) {
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

	private void getFormat(int a) {
		// translate/interpret characters until IM...
		// TODO: handle implied-DO, etc...
		String fmt = "";
		// TODO: limit scan!
		while (true) {
			int m = sys.rawReadMem(a++);
			fmt += sys.pdc.cvt.hwToLP((byte)(m & 077));
			if ((m & 0200) != 0) {
				break;
			}
		}
		idx = 0;
		rep = 0;
		this.fmt = scanFormat(fmt, 0);
		idx = 0;
		buf = "";
	}

	private void doParam(int a) {
		if (input) {
			doParamIn(a);
		} else {
			doParamOut(a);
		}
	}

	private void doParamIn(int a) {
		nextParam(true);
		int c = fmt[idx].spec;
		int n = fmt[idx].width;
		if (ip >= buf.length()) {
			++idx;
			return;
		}
		if (ip + n > buf.length()) {
			n = buf.length() - ip;
		}
		double dd;
		int val;
		int b = 10;
		String L;
		switch (c) {
		case 'A':
			putStr(a, buf.substring(ip, ip + n));
			break;
		case 'O':
			b = 8;
		case 'I':
			val = 0;
			try {
				val = Integer.valueOf(buf.substring(ip, ip + n).trim(), b);
			} catch (Exception ee) { }
			putInt(a, val);
			break;
		case 'L':
			L = buf.substring(ip, ip + n).trim();
			boolean l = (L.charAt(0) == 'T');
			putInt(a, l ? 1 : 0);
			break;
		case 'E':
		case 'F':
		case 'G':
			dd = 0.0;
			try {
				dd = Double.valueOf(buf.substring(ip, ip + n));
			} catch (Exception ee) { }
			putReal(a, dd);
			break;
		case 'H': // done in nextParam()... TODO: move here?
			// must transfer actual input characters to FORMAT statement...
			// TODO!
		case 'X': // done in nextParam()... TODO: move here?
		default:
			++idx;
			return;
		}
		ip += n;
		++idx;
	}

	private void doParamOut(int a) {
		nextParam(true);
		int c = fmt[idx].spec;
		int n = fmt[idx].width;
		int m;
		double dd;
		int val;
		String v = "";
		switch (c) {
		case 'A':
			v = getStr(a);
			v = String.format(fmt[idx].format, v);
			break;
		case 'I':
		case 'O':
			val = getInt(a);
			v = String.format(fmt[idx].format, val);
			break;
		case 'L':
			val = getInt(a);
			v = String.format(fmt[idx].format, val != 0 ? "T" : "F");
			break;
		case 'F':
		case 'G':
		case 'E':
			dd = getReal(a);
			v = String.format(fmt[idx].format, dd);
			break;
		case 'X': // done in nextParam()... TODO: move here?
		case 'H': // done in nextParam()... TODO: move here?
		default:
			++idx;
			return;
		}
		if (v.length() > n) {
			v = "*";
			while (v.length() < n) {
				v += '*';
			}
		}
		buf += v;
		++idx;
	}

	private void doHInput(FormatSpec fmt) {
		if (fmt.spec == '/') {
			dispatchInput();
			return;
		}
		if (fmt.spec != 'H' || fmt.offset < 0) {
			ip += fmt.width;
			return;
		}
		// TODO: this may not be the right address...
		int a = fmt.offset;
		for (int x = 0; x < fmt.width; ++x) {
			sys.rawWriteChar(a++,
				sys.pdc.cvt.asciiToHw((byte)buf.charAt(ip++)));
		}
	}

	// We have a parameter, must find a format spec to use...
	private void nextParam(boolean must) {
		if (must && idx >= fmt.length) {
			idx = rep;
			if (input) {
				dispatchInput();
			} else {
				dispatchOutput();
				buf = "";
			}
		}
		while (idx < fmt.length && !fmt[idx].parm) {
			if (input) {
				doHInput(fmt[idx]);
			} else if (fmt[idx].spec == '/') {
				dispatchOutput();
				buf = "";
			} else if (fmt[idx].spec == 'X') {
				for (int x = 0; x < fmt[idx].width; ++x) {
					buf += ' ';
				}
			} else {	// 'H'
				buf += fmt[idx].format;
			}
			++idx;
		}
	}

	private FormatSpec[] scanFormat(String f, int lev) {
		Vector<FormatSpec> fmt = new Vector<FormatSpec>();
		while (idx < f.length()) {
			while (idx < f.length() && (f.charAt(idx) == ' ' ||
					f.charAt(idx) == ',')) {
				++idx;
			}
			if (idx >= f.length()) {
				break;
			}
			int c = f.charAt(idx);
			int r = 1;
			if (Character.isDigit(c)) {
				// could be nnHxxx or rIw etc...
				r = getNum(f);
				c = f.charAt(idx);
			}
			switch (c) {
			case '(':
				// This gets tricky with global 'idx'...
				++idx;
				FormatSpec[] f1 = scanFormat(f, lev + 1);
				if (lev == 0) {
					rep = fmt.size();
				}
				while (r > 0) {
					for (FormatSpec fs : f1) {
						fmt.add(fs);
					}
					--r;
				}
				break;
			case ')':
				++idx;
				if (lev > 0) {
					return fmt.toArray(new FormatSpec[0]);
				}
				// else error...
				break;
			case '/':
				++idx;
				fmt.add(new FormatSpec('/', 0, 0));
				break;
			case 'X':
				++idx;
				fmt.add(new FormatSpec('X', r, -1));
				break;
			case 'H':
				++idx;
				int o = fmtAdr + idx; // assumes 'idx' follows mem addr
				fmt.add(new FormatSpec('H', r, getHollerith(f, r), o));
				break;
			case '\'':
				// repetition count not allowed?
				++idx;
				String q = getQuoted(f);
				++idx;
				fmt.add(new FormatSpec('H', q.length(), q, -1));
				break;
			default:
				++idx;
				int w = getNum(f);
				int d = -1;
				if (idx < f.length() && f.charAt(idx) == '.') {
					++idx;
					d = getNum(f);
				}
				while (r > 0) {
					fmt.add(new FormatSpec(c, w, d));
					--r;
				}
			}
		}
		return fmt.toArray(new FormatSpec[0]);
	}

	private int getNum(String f) {
		int n = 0;
		while (idx < f.length() &&
				Character.isDigit(f.charAt(idx))) {
			n = (n * 10) + (f.charAt(idx++) - '0');
		}
		return n;
	}

	private String getHollerith(String f, int n) {
		String s = "";
		while (idx < f.length() && n > 0) {
			s += f.charAt(idx++);
			--n;
		}
		while (n > 0) {
			s += ' ';
			--n;
		}
		return s;
	}

	private String getQuoted(String f) {
		String s = "";
		while (idx < f.length() && f.charAt(idx) != '\'') {
			s += f.charAt(idx++);
		}
		return s;
	}

	private void dispatchOutput() {
		if (perph instanceof P_LinePrinter) {
			// TODO: carriage control, etc...
			// ...or just send through actual peripheral...
			String cc = "\n";
			if (buf.length() == 0) {
				sys.listOut(cc);
				return;
			}
			char cr = buf.charAt(0);
			switch (cr) {
			case ' ': break;
			case '+': cc = ""; break;
			case '0': cc += '\n'; break;
			case '1': break; // TODO: Form Feed
			case '9':
				cc += '\n';
			case '8':
				cc += '\n';
			case '7':
				cc += '\n';
			case '6':
				cc += '\n';
			case '5':
				cc += '\n';
			case '4':
				cc += '\n';
			case '3':
				cc += '\n';
			case '2':
				cc += '\n';
				break;
			}
			sys.listOut(cc + buf.substring(1));
		} else if (sqio != null) {
			if (!sqio.ready()) {
				ioflgs |= 004;
				sys.rawWriteChar(comm + 0, (byte)ioflgs);
				return;
			}
			if (buf == null) {
				buf = "";
			}
			byte[] b = new byte[buf.length()];
			for (int x = 0; x < buf.length(); ++x) {
				b[x] = sys.pdc.cvt.asciiToHw((byte)buf.charAt(x));
			}
			sqio.appendRecord(b, 0, b.length);
		} else {
			System.err.format("Output on unsupported device %02o %o\n", dev, unit);
		}
	}

	private void dispatchInput() {
		if (sqio == null) {
			System.err.format("Input on unsupported device %02o %o\n", dev, unit);
			return;
		}
		buf = "";
		if (!sqio.ready()) {
			// TODO: EOF? error?
			ioflgs |= 004;
			sys.rawWriteChar(comm + 0, (byte)ioflgs);
			return;
		}
		byte[] b = sqio.nextRecord();
		if (b == null) {
			ioflgs |= 002;
			sys.rawWriteChar(comm + 0, (byte)ioflgs);
			return;
		}
		if (b.length == 0) {
			ioflgs |= 001;
			sys.rawWriteChar(comm + 0, (byte)ioflgs);
			return;
		}
		// Need ASCII in order to scan numbers, but want HW for strings!
		// Also, may not want LP special chars (unicode).
		for (int x = 0; x < b.length; ++x) {
			buf += sys.pdc.cvt.hwToLP(b[x]);
		}
	}
}
