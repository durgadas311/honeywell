// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.Arrays;
import java.util.Vector;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;

public class Assembler {
	File inFile;
	BufferedReader in;
	InstrDecode idc;
	CharConverter cvt;
	OutputStream lst;
	int currLoc;
	int lineNo;
	int adrMode;
	int adrMask;
	int rep = 0;
	private Vector<String> errs;
	private Map<String,Integer> symTab;
	byte[] code;
	boolean end;
	boolean asmPass;
	String prog;
	String segm;
	int segno;
	String rev;
	int vis;
	int endAdr;
	int minAdr;
	int maxAdr;
	byte[] image;
	CoreMemory sys;
	int reloc;
	boolean listing;
	Loader loader = null;

	class Clear {
		public int start;
		public int end;
		public int fill;
	}

	Clear clear = null;

	// TODO:
	//	Handle ad-hoc constants.
	//	Floating-point constants. (differentiate "F" from symbol?)
	//	Special marks by 'mrk' (column 7)
	//	RESV,fill syntax
	//	DSA ARG,ARG,VAR syntax
	//	DA directive
	//
	public Assembler(File input) {
		prog = null; // or default to file name?
		segm = "01";
		segno = 1;
		rev = "000";
		vis = 0;	// TODO: needs to be non-zero?
		inFile = input;
		symTab = new HashMap<String,Integer>();
		errs = new Vector<String>();
		try {
			in = new BufferedReader(new FileReader(inFile));
		} catch (Exception ee) {
			ee.printStackTrace();
			System.exit(1);
		}
		idc = new InstrDecode(true);
		cvt = new CharConverter();
		lst = null;
		sys = null;
		image = null;
		reloc = 0;
		listing = false;
	}

	// This is destructive, clears errors.
	public String getErrors() {
		String s = "";
		while (errs.size() > 0) {
			if (s.length() > 0) {
				s += '\n';
			}
			s += errs.remove(0);
		}
		return s;
	}

	public int passOne() {
		asmPass = false;
		int ret = 0;
		currLoc = 0;
		lineNo = 0;
		end = false;
		endAdr = 0;
		minAdr = 0x100000;
		maxAdr = 0;
		while (!end && (ret = scanOne()) >= 0) {
		}
		try { in.close(); } catch (Exception ee) {}
		//System.err.format("END OF PASS 1 - %d %07o %07o %07o\n", ret, minAdr, maxAdr, endAdr);
		if (errs.size() > 0) {
			ret = -1;
		}
		return ret;
	}

	public Map<String, Integer> getSymTab() {
		return symTab;
	}

	public void listSymTab() {
		int x = 0;
		listOut("Symbol Table:\n");
		Map<String, Integer> sorted = new TreeMap<String, Integer>(symTab);
		for (Map.Entry<String, Integer> entry : sorted.entrySet()) {
			String l = String.format("  %7s %07o",
					entry.getKey(), entry.getValue());
			++x;
			if (x >= 7) {
				x = 0;
				l += '\n';
			}
			listOut(l);
		}
		if (x > 0) {
			listOut("\n");
		}
	}

	public int getMin() { return minAdr; }
	public int getMax() { return maxAdr; }
	public int getStart() { return endAdr; }
	public String getName() { return prog; }
	public byte[] getHWName() {
		byte[] bb = new byte[prog.length()];
		for (int x = 0; x < bb.length; ++x) {
			bb[x] = cvt.asciiToHw((byte)(prog.charAt(x) & 0x7f));
		}
		return bb;
	}

	// 'sys' provides printer output, 'loader' provides destination for code.
	public int passTwo(CoreLoader ldr, int reloc, boolean list) {
		this.reloc = reloc;
		try {
			in = new BufferedReader(new FileReader(inFile));
		} catch (Exception ee) {
			// 'in' should never fail - already validated in ctor.
			ee.printStackTrace();
			return -1;
		}
		loader = ldr;
		listing = list;
		asmPass = true;
		int ret = 0;
		currLoc = 0;
		lineNo = 0;
		end = false;
		segm = "01"; // reset from pass one...
		segno = 1;
		loader.begin(minAdr, prog, segm, rev, vis);
		loader.range(minAdr, maxAdr); // passOne set these...
		while (!end && (ret = scanOne()) >= 0) {
		}
		loader.end(endAdr);
		try { in.close(); } catch (Exception ee) {}
		if (errs.size() > 0) {
			ret = -1;
		}
		if (listing) {
			// TODO: intersperse with listing...
			for (String l : errs) {
				listOut("*** " + l + "\n");
			}
			listOut(String.format("END OF ASSEMBLY%s\n",
				ret < 0 ? " (ERRORS)" : ""));
		}
		return ret;
	}

	public int passTwo(Loader ldr, Object list) {
		try {
			in = new BufferedReader(new FileReader(inFile));
		} catch (Exception ee) {
			// 'in' should never fail - already validated in ctor.
			ee.printStackTrace();
			return -1;
		}
		// TODO: clean this up
		if (list instanceof OutputStream) {
			lst = (OutputStream)list;
			listing = true;
		} else if (list instanceof CoreMemory) {
			this.sys = (CoreMemory)list;
			listing = true;
		}
		loader = ldr;
		asmPass = true;
		int ret = 0;
		currLoc = 0;
		lineNo = 0;
		end = false;
		segm = "01"; // reset from pass one...
		segno = 1;
		loader.begin(minAdr, prog, segm, rev, vis);
		loader.range(minAdr, maxAdr); // passOne set these...
		while (!end && (ret = scanOne()) >= 0) {
		}
		loader.end(endAdr);
		//System.err.format("END OF PASS 2 - %07o %07o %07o\n", minAdr, maxAdr, endAdr);
		if (errs.size() > 0) {
			ret = -1;
		}
		if (listing) {
			// TODO: intersperse with listing...
			for (String l : errs) {
				listOut("*** " + l + "\n");
			}
			listOut(String.format("END OF ASSEMBLY%s\n",
				ret < 0 ? " (ERRORS)" : ""));
		}
		try { in.close(); } catch (Exception ee) {}
		return ret;
	}

	private String replaceChars(String in, String srch, String repl) {
		char[] inc = in.toCharArray();
		char[] out = new char[inc.length];
		for (int x = 0; x < inc.length; ++x) {
			int i = srch.indexOf(inc[x]);
			if (i >= 0) {
				out[x] = repl.charAt(i);
			} else {
				out[x] = inc[x];
			}
		}
		return new String(out);
	}

	private void listOut(String str) {
		str = replaceChars(str, "\001\011\006\010\007", "\u00a2\u25a1\u25a0\u00a9\u2260");
		if (lst != null) {
			try {
				lst.write(str.getBytes());
			} catch (Exception ee) {
			}
		} else if (sys != null) {
			sys.listOut(str);
		} else if (loader instanceof CoreLoader) {
			((CoreLoader)loader).listOut(str);
		}
	}

	private char charMark(byte b) {
		char mk = ' ';
		switch(b & 0300) {
		case 0100:
			mk = 'W';
			break;
		case 0200:
			mk = 'I';
			break;
		case 0300:
			mk = 'R';
			break;
		}
		return mk;
	}

	public CharConverter charCvt() { return cvt; }

	public int lookupSym(String sym) {
		return symTab.get(sym);
	}

	public String lookupAdr(int adr) {
		String sym = null;
		int min = 99999999;
		for (Map.Entry<String,Integer> set : symTab.entrySet()) {
			int d = adr - set.getValue();
			if (d < 0) {
				continue;
			}
			if (d < min) {
				min = d;
				sym = set.getKey();
			}
		}
		return sym;
	}

	private int scanOne() {
		// TODO: tolerate "illegal" TAB characters
		// TODO: input from punchcard vs ASCII file (vs MagTape?)
		String line = "";
		code = null;
		int orgLoc = currLoc;
		try {
			line = in.readLine();
			if (line == null) {
				return -1;
			}
		} catch (Exception ee) {
			ee.printStackTrace();
			return -1;
		}
		++lineNo;
		if (line.length() == 0) {
			// TODO: pass-thru to listing?
			return 0;
		}
		// first do convenience translations of special chars
		line = replaceChars(line.toUpperCase(), CharConverter.hwAsciiSup, CharConverter.hwAsciiRep);
		String card = String.format("%-80s", line.toUpperCase());
		// TODO: handle D data cards... C/L continuation and macro...
		char typ = card.charAt(5);
		if (typ == '*' || typ == 'T') {
			if (listing) {
				String l = "                                 " + line + "\n";
				listOut(l);
			}
			return 0;
		}
		char mrk = card.charAt(6);
		String loc;
		String opc;
		String opd;
		// TODO: extended symbol length
		if (true) {
			loc = card.substring(7, 14);
			opc = card.substring(14, 20).trim();
			opd = card.substring(20);
		} else {
			loc = card.substring(7, 18);
			opc = card.substring(18, 24).trim();
			opd = card.substring(24);
		}
		boolean rev = false;
		if (loc.startsWith(" ")) {
			rev = true;
			loc = loc.substring(1).trim();
		} else {
			loc = loc.trim();
		}
		// TODO: handle numeric loc, i.e. absolute memory address
		// (*temporary* currLoc). Also out-of-sequence base?
		int e = 0;
		// first, preserve "quotes"...
		// Punchcards ambiguity: '@' or '\'' (card) is ':' (listing)
		// TODO: IBM029 has different '\''
		if (opd.length() > 0) {
			if ((opd.charAt(0) == '@' || opd.charAt(0) == '\'' ||
					opd.charAt(0) == ':')) {
				e = opd.indexOf(opd.charAt(0), 1);
				if (e < 0) {
					e = 0;
				}
			} else if (opd.matches("#[0-9]+A.*")) {
				e = opd.indexOf('A');
				int n = Integer.valueOf(opd.substring(1, e));
				e += n + 1;
				// TODO: if e > len will throw exception?
			}
		}
		e = opd.indexOf(' ', e);
		if (e >= 0) {
			//opd = opd.substring(0, e).trim();
			// CAUTION: trim() removes more than blanks!
			opd = opd.substring(0, e);
		}
		clear = null;
		byte op = idc.getOp(opc);
		if (op != InstrDecode.OP_ILL) {
			rep = 0;
			e = processMachCode(op, mrk, loc, rev, opd);
		} else {
			e = processAsmDir(opc, mrk, loc, rev, opd);
		}
		if (!asmPass) {
			if (e > 0 && e < 0x100000 && orgLoc < minAdr) {
				minAdr = orgLoc;
			}
			if (currLoc > maxAdr) {
				maxAdr = currLoc;
			}
			return e;
		}
		if (code != null) {
			// TODO: disallow any number of interviening statements?
			int repadd = 0;
			do {
				loader.setCode(reloc + orgLoc + repadd, code);
				repadd += code.length;
			} while (rep > 0 && --rep > 0);
			rep = 0;
		} else if (clear != null) {
			loader.clear(clear.start, clear.end, (byte)clear.fill);
			clear = null;
		}
		if (listing) {
			String l = "";
			if (code != null && code.length > 0) {
				char mk = charMark(code[0]);
				char mk2 = ' ';
				if (code.length > 1) {
					mk2 = charMark(code[code.length - 1]);
				}
				for (int y = 0; y < code.length; ++y) {
					l += String.format("%02o", code[y] & 077);
				}
				if (l.length() > 16) {
					l = l.substring(0, 16) + "+";
				}
				l = String.format("     %07o %c%c %-17s", orgLoc, mk, mk2, l);
			} else if (e >= 0x100000) { // special case for some ASM directives
				l = String.format("     %07o                     ",
					(e & 0x0fffff));
			} else {
				l = "                                 ";
			}
			listOut(l + line + "\n");
		}
		if (e >= 0x200000) { // special case for EX/XFR directives
			// TODO: further annotate listing?
			loader.exec(e & 0x0fffff);
			// generate default segment...
			if (segno > 0) {
				++segno;
				segm = String.format("%02d", segno);
				loader.segment(prog, segm, this.rev, vis);
			}
		}
		return e;
	}

	private void errsAdd(String err) {
		// TODO: intersperse errors in listing...
		errs.add(err + " at line " + lineNo);
	}

	// -1 = error
	// -2..-16 = X1..X15
	// -18..-32 = Y1..Y15
	private int parseArg(String opd) {
		int adr = 0;
		// TODO: handle context (index vs. address).
		if (opd.matches("^[XY][1-9]$") || opd.matches("^[XY]1[0-5]$")) {
			adr = Integer.valueOf(opd.substring(1));
			if (adrMode == 3 && (opd.charAt(0) == 'Y' || adr > 6)) {
				errsAdd("Invalid indexed address " + opd);
				return -1;
			}
			// adr: 1..15
			if (opd.charAt(0) == 'Y') {
				adr += 16; // 17..31
			}
			// adr: 1..15, 17..31
			adr += 1; // adr: 2..16, 18..32
			return -adr; // -2..-16, -18..-32
		}
		if (opd.equals("*")) {
			return currLoc;
		}
		if (Character.isDigit(opd.charAt(0))) {
			try {
				return Integer.valueOf(opd) & adrMask;
			} catch (Exception ee) {
				errsAdd("Invalid number " + opd);
				return -1;
			}
		}
		if (symTab.containsKey(opd)) {
			return symTab.get(opd);
		} else if (asmPass) {
			errsAdd("Undefined symbol " + opd);
			return -1;
		}
		return 0;
	}

	// One or more symbols or numbers, separated by +/-,
	// and an optional + index-reg (if indexed is true).
	private int parseExpr(String opd, boolean indexed) {
		int ix = 0;
		int adr = 0;
		char op = '+';
		while (ix >= 0) {
			int x = opd.indexOf('+', ix);
			int y = opd.indexOf('-', ix);
			if (y >= 0 && (x < 0 || y >= 0 && y < x)) {
				x = y;
			}
			int a;
			if (x >= 0) {
				a = parseArg(opd.substring(ix, x));
				if (a < -1) {
					errsAdd("Indexed must be last " + opd);
					return -1;
				}
			} else {
				a = parseArg(opd.substring(ix));
			}
			if (a == -1) {
				// need message?
				return -1;
			}
			if (a < -1) {
				if (!indexed) {
					errsAdd("Indexed not allowed " + opd);
					return -1;
				}
				a = -a; // 2..16, 18..32
				--a;	// 1..16, 17..31
				if (adrMode == 3) {
					adr |= (a << 15);
				} else { // must be 4
					adr |= (a << 19);
				}
				// must be last, could return here
			} else {
				if (op == '-') {
					adr -= a;
				} else {
					adr += a;
				}
				adr &= adrMask;
			}
			ix = x;
			if (ix >= 0) {
				op = opd.charAt(ix++);
			}
		}
		return adr;
	}

	private int parseAdr(String opd, boolean simple) {
		int ix;
		int adr = 0;
		int idx = 0;	// 0: not a valid index register - no indexing
		if (opd.charAt(0) == '(') {
			if (simple) {
				errsAdd("Indirect address not allowed");
				return -1;
			}
			if (adrMode < 3) {
				errsAdd("Indirect address in 2-char mode");
				return -1;
			}
			ix = opd.indexOf(')');
			if (ix < 0) {
				errsAdd("Invalid indirect address " + opd);
				return -1;
			}
			// TODO: can there be residual after ')' ?
			adr = parseExpr(opd.substring(1, ix), false);
			if (adr < 0) {
				return -1;
			}
			if (adrMode == 3) {
				adr |= 0700000;
			} else { // must be 4
				adr |= 040000000;
			}
		} else {
			adr = parseExpr(opd, !simple && adrMode >= 3);
			if (adr < 0) {
				return -1;
			}
		}
		return adr;
	}

	private void putAdr(byte[] bb, int ix, int adr) {
		for (int y = adrMode - 1; y >= 0; --y) {
			bb[ix + y] = (byte)(adr & 077);
			adr >>= 6;
		}
	}

	// This is a clone of I_FMA.nativeToHw() - KEEP IN SYNC!
	private void nativeToHw(byte[] bb, double dd, boolean denorm, int ptr) {
		long d = Double.doubleToLongBits(dd);
		if ((d & 0x7fffffffffffffffL) == 0) {
			d = 0; // TODO: does HW allow -0 ?
		} else {
			byte ms = (byte)((d >> 63) & 1);
			int x = (int)((d >> 52) & 0x7ff);
			x -= 1023;
			long m = (d >> 18) & 0x03ffffffffL;
			if (!denorm) {
				// implied "1"...
				m |= 0x0400000000L;
			}
			if (ms != 0) {
				m = -m;
			}
			d = (m << 12) | (x & 0xfff);
		}
		for (int ix = 0; ix < 8; ++ix) {
			byte b = (byte)(d & 077);
			bb[ptr--] = b;
			d >>= 6;
		}
	}

	private byte parseVar(String opd) {
		byte v = 0;
		try {
			v = Byte.valueOf(opd, 8);
		} catch (Exception ee) {
			errsAdd("Invalid variant " + opd);
		}
		return v;
	}

	private byte[] parseCon(String opd) {
		char c = opd.charAt(0);
		if (c == '@' || c == ':' || c == '\'') {
			// Punchcards ambiguity: '@' or '\'' (card) is ':' (listing)
			// TODO: IBM029 has different '\'' ('<')
			int e = opd.indexOf(c, 1);
			if (e < 0) {
				// TODO: should be error?
				e = opd.length();
			}
			byte[] bb = new byte[e - 1];
			for (int y = 1; y < e; ++y) {
				// TODO: check for invalid chars?
				bb[y - 1] = cvt.asciiToHw((byte)(opd.charAt(y) & 0x7f));
			}
			return bb;
		} else if (c == '#' || c == '=') {
			// Punchcards ambiguity: '#' or '=' (card) is '=' (listing)
			// TODO: IBM029 has different '=' ('>')
			int e = -1;
			int base = 10;
			if (opd.matches("#[0-9]+B[-+0-9]+")) {
				e = opd.indexOf('B', 1);
			} else if (opd.matches("#[0-9]+C[0-7]+")) {
				// Must left-justify digits in final value
				e = opd.indexOf('C', 1);
				base = 8;
			} else if (opd.matches("#[0-9]+A.*")) {
				e = opd.indexOf('A', 1);
				base = 0; // characters
			} else if (!opd.matches("#[0-9]+")) {
				return null;
			}
			int n;
			if (e < 0) {
				n = Integer.valueOf(opd.substring(1));
			} else {
				n = Integer.valueOf(opd.substring(1, e));
			}
			byte[] bb = new byte[n];
			if (e < 0) {
				Arrays.fill(bb, (byte)015);
				return bb;
			}
			++e;
			if (base == 0) {
				int f = opd.length();
				for (int y = 0; y < n; ++y) {
					if (y + e < f) {
						bb[y] = cvt.asciiToHw((byte)
							(opd.charAt(y + e) & 0x7f));
					} else {
						bb[y] = (byte)015;
					}
				}
				return bb;
			}
			// assume it will fit in a long...
			long l = Long.valueOf(opd.substring(e), base);
			if (base == 8 && l != 0) {
				// Must left-justify digits in final value
				int nn = opd.substring(e).length();
				if ((nn & 1) != 0) {
					l <<= 3;
					++nn;
				}
				while (nn < n * 2) {
					l <<= 6;
					nn += 2;
				}
			}
			for (int y = n - 1; y >= 0; --y) {
				bb[y] = (byte)(l & 077);
				l >>= 6;
			}
			return bb;
		} else if (c == 'F') {
			double dd = 0.0;
			try {
				dd = Double.valueOf(opd.substring(1));
			} catch (Exception ee) {
				errsAdd("Invalid floating point number " + opd);
			}
			byte[] bb = new byte[8];
			nativeToHw(bb, dd, false, 7);
			return bb;
		} else if (Character.isDigit(c) || c == '+' || c == '-') {
			int s = 0;
			int e = opd.length();
			if (!Character.isDigit(c)) {
				++s;
			}
			byte[] bb = new byte[e - s];
			for (int y = s; y < e; ++y) {
				// assume all are decimal digits...
				bb[y - s] = (byte)(opd.charAt(y) & 017);
			}
			if (c == '-') {
				bb[e - s - 1] |= (byte)040;
			} else if (c == '+') {
				bb[e - s - 1] |= (byte)020;
			}
			return bb;
		} else {
			errsAdd("Unsupported constant " + opd);
			return null;
		}
	}

	private void setLabel(String loc, boolean set, int len) {
		if (loc.length() > 0 && set) {
			int v = currLoc;
			if (len > 0) {
				--v;
			}
			if (symTab.containsKey(loc) && symTab.get(loc) != v) {
				errsAdd("Redefined symbol " + loc);
				return;
			}
			symTab.put(loc, v);
		}
	}

	private int countChars(String targ, char c) {
		int x = targ.indexOf(c);
		int n = 0;
		while (x >= 0) {
			++n;
			x = targ.indexOf(c, x + 1);
		}
		return n;
	}

	private int processMachCode(byte op, char mrk, String loc, boolean rev, String opd) {
		int flags = idc.getFlags(op);
		int xflags = 0;
		int a = 0;
		int b = 0;
		int c = 0;
		byte[] v = null;
		String[] opds = opd.split(",");
		int ox = 0;

		if ((flags & InstrDecode.OP_INVAL) != 0) { // not possible?
			errsAdd(String.format("Invalid op code %02o", op));
			return -1;
		}
		if ((flags & InstrDecode.OP_SPC) != 0) {
			op = 065;
		}
		int il = 1; // always has an opcode
		// General Rule, optional operands:
		// Right operand only present if left one(s) also present.
		// But this is based on "HAS" flags - an instruction may not
		// have A or B but may have V. But, if it has A and A was
		// not provided then it cannot have B or V.
		if ((flags & InstrDecode.OP_HAS_A) != 0) {
			if (ox < opds.length && opds[ox].length() > 0) {
				a = parseAdr(opds[ox], false);
				il += adrMode;
				xflags |= InstrDecode.OP_HAS_A;
				++ox;
			} else if ((flags & InstrDecode.OP_REQ_A) != 0) {
				errsAdd("Reqd A field missing");
				return -1;
			}
		}
		if ((flags & InstrDecode.OP_HAS_B) != 0) {
			if (ox < opds.length && opds[ox].length() > 0) {
				b = parseAdr(opds[ox], false);
				il += adrMode;
				xflags |= InstrDecode.OP_HAS_B;
				++ox;
			} else if ((flags & InstrDecode.OP_REQ_B) != 0) {
				errsAdd("Reqd B field missing");
				return -1;
			}
		}
		if ((flags & InstrDecode.OP_HAS_C) != 0 &&
				ox + 1 == opds.length) {
			if (ox < opds.length && opds[ox].length() > 0) {
				c = parseAdr(opds[ox], false);
				il += adrMode;
				xflags |= InstrDecode.OP_HAS_C;
				++ox;
			} else if ((flags & InstrDecode.OP_REQ_C) != 0) {
				errsAdd("Reqd C field missing");
				return -1;
			}
		} else if (ox < opds.length && opds[ox].length() > 0) {
			// If programmer added variants, assemble them regardless...
			int n = opds.length - ox;
			// TODO: req'd number of variants?
			v = new byte[n];
			for (int y = 0; y < n; ++y) {
				v[y] = parseVar(opds[ox + y]);
			}
			il += n;
			xflags |= InstrDecode.OP_HAS_V;
			ox = opds.length;
		} else if ((flags & InstrDecode.OP_REQ_V) != 0) {
			errsAdd("Reqd variants missing");
			return -1;
		}
		if (!asmPass) {
			setLabel(loc, !rev, 0);
			currLoc += il;
			setLabel(loc, rev, il);
			return il;
		}
		code = new byte[il];
		int x = 0;
		// TODO: punctuation is a variable...
		code[x++] = op;
		if ((xflags & InstrDecode.OP_HAS_A) != 0) {
			putAdr(code, x, a);
			x += adrMode;
		}
		if ((xflags & InstrDecode.OP_HAS_B) != 0) {
			putAdr(code, x, b);
			x += adrMode;
		}
		if ((xflags & InstrDecode.OP_HAS_C) != 0) {
			putAdr(code, x, c);
			x += adrMode;
		}
		if ((xflags & InstrDecode.OP_HAS_V) != 0) {
			for (int y = 0; y < v.length; ++y) {
				code[x++] = (byte)(v[y] & 077);
			}
		}
		setMarks(code, mrk, true);
		setLabel(loc, !rev, 0);	// set labels again?
		currLoc += il;
		setLabel(loc, rev, il);	// set labels again?
		return il;
	}

	private int processAsmDir(String opc, char mrk, String loc, boolean rev,
						String opd) {
		if (opc.equals("DCW")) {
			return processDefCon(mrk, loc, rev, opd, true);
		} else if (opc.equals("DC")) {
			return processDefCon(mrk, loc, rev, opd, false);
		} else if (opc.startsWith("RESV")) {
			rep = 0;
			int fill = -1;
			if (opc.indexOf(',') == 4) {
				if (opc.length() < 6) {
					fill = 015;
				} else {
					fill = cvt.asciiToHw((byte)(opc.charAt(5) & 0x7f));
				}
			}
			return processResv(mrk, loc, rev, opd, fill);
		} else if (opc.equals("DSA")) {
			return processDefSym(mrk, loc, rev, opd);
		} else if (opc.equals("DA")) {
			return noImpl(opc);
		} else if (opc.equals("PROG")) {
			rep = 0;
			prog = String.format("%-6.6s", opd);
			return 0;
		} else if (opc.equals("SEG")) {
			rep = 0;
			segm = String.format("%-2.2s", opd);
			segno = -1;
			if (asmPass) {
				loader.segment(prog, segm, this.rev, vis);
			}
			return 0;
		} else if (opc.equals("EX")) {
			return processEnd(opd, 1);
		} else if (opc.equals("XFR")) {
			return processEnd(opd, 2);
		} else if (opc.equals("ORG")) {
			return processOrg(loc, opd, rev);
		} else if (opc.equals("MORG")) {
			return processMorg(loc, opd, rev);
		} else if (opc.equals("LITORG")) {
			// since we don't gather literals, this can be ORG
			return processOrg(loc, opd, rev);
		} else if (opc.equals("ADMODE")) {
			rep = 0;
			int m = Integer.valueOf(opd);
			switch(m) {
			case 2:
				adrMask = 0x0fff;
				break;
			case 3:
				adrMask = 0x07fff;
				break;
			case 4:
				adrMask = 0x07ffff;
				break;
			default:
				errsAdd("Invalid argument " + opd);
				return -1;
			}
			adrMode = m;
			return 0;
		} else if (opc.equals("EQU")) {
			return processEqu(loc, opd);
		} else if (opc.equals("CEQU")) {
			return noImpl(opc);
		} else if (opc.equals("SKIP")) {
			return 0; // silently ignore
		} else if (opc.equals("SFX")) {
			return noImpl(opc);
		} else if (opc.equals("REP")) {
			return processRep(opd);
		} else if (opc.equals("GEN")) {
			return noImpl(opc);
		} else if (opc.equals("SETLIN")) {
			return 0; // silently ignore
		} else if (opc.equals("XBASE")) {
			return noImpl(opc);
		} else if (opc.equals("RANGE")) {
			return processRange(opd);
		} else if (opc.equals("CLEAR")) {
			return processClear(opd);
		} else if (opc.equals("END")) {
			return processEnd(opd, 0);
		}
		return noImpl(opc);
	}

	private int processEnd(String opd, int type) {
		rep = 0;
		int ret = 0;
		int adr = 0;
		if (opd.length() > 0) {
			adr = parseAdr(opd, true);
			ret = adr | 0x100000;
		}
		switch (type) {
		case 0:
			endAdr = adr;
			end = true;
			break;
		case 1:
		case 2:
			if (asmPass) {
				// Need to delay actual run until
				// we complete scanOne().
				ret = adr | 0x200000;
			}
			break;
		}
		return ret;
	}

	private void setMarks(byte[] bb, char mk, boolean defWM) {
		if (bb.length < 1) {
			return;
		}
		int last = bb.length - 1;
		switch(mk) {
		case ' ':
			break;
		case 'L':
			code[0] |= 0200;
			break;
		case 'R':
			code[last] |= 0200;
			break;
		case 'A':
			code[0] |= 0100;
			defWM = false;
			break;
		case 'B':
			code[0] |= 0200;
			defWM = false;
			break;
		case 'C':
			code[0] |= 0300;
			defWM = false;
			break;
		case 'D':
			code[last] |= 0100;
			defWM = false;
			break;
		case 'E':
			code[last] |= 0200;
			defWM = false;
			break;
		case 'F':
			code[last] |= 0300;
			defWM = false;
			break;
		case 'G':
			code[0] |= 0200;
			code[last] |= 0100;
			defWM = false;
			break;
		case 'H':
			code[0] |= 0200;
			code[last] |= 0200;
			defWM = false;
			break;
		case 'I':
			code[0] |= 0200;
			code[last] |= 0300;
			defWM = false;
			break;
		case 'J':
			code[0] |= 0100;
			code[last] |= 0100;
			defWM = false;
			break;
		case 'K':
			code[0] |= 0100;
			code[last] |= 0200;
			defWM = false;
			break;
		case 'M':
			code[0] |= 0100;
			code[last] |= 0300;
			defWM = false;
			break;
		case 'N':
			defWM = false;
			break;
		case 'P':
			code[0] |= 0300;
			code[last] |= 0100;
			defWM = false;
			break;
		case 'S':
			code[0] |= 0300;
			code[last] |= 0200;
			defWM = false;
			break;
		case 'T':
			code[0] |= 0300;
			code[last] |= 0300;
			defWM = false;
			break;
		}
		if (defWM) {
			code[0] |= 0100;
		}
	}

	private int processDefCon(char mrk, String loc, boolean rev, String opd, boolean mark) {
		setLabel(loc, rev, 0);
		code = parseCon(opd);
		if (code == null) {
			errsAdd("Could not parse constant " + opd);
			return -1;
		}
		int len = code.length;
		setMarks(code, mrk, mark);
		currLoc += len;
		setLabel(loc, !rev, len);
		if (rep > 0) {
			currLoc += (rep - 1) * len;
		}
		return len;
	}

	private int processResv(char mrk, String loc, boolean rev, String opd, int fill) {
		rep = 0;
		setLabel(loc, rev, 0);
		int len = Integer.valueOf(opd);
		int ret = currLoc | 0x100000;
		if (fill >= 0) {
			clear = new Clear();
			clear.start = currLoc;
			clear.end = currLoc + len - 1;
			clear.fill = fill;
		}
		currLoc += len;
		setLabel(loc, !rev, len);
		return ret;
	}

	private int processOrg(String loc, String opd, boolean rev) {
		int adr;
		rep = 0;
		if (Character.isDigit(opd.charAt(0))) {
			adr = Integer.valueOf(opd);
		} else if (symTab.containsKey(opd)) {
			adr = symTab.get(opd);
		} else {
			errsAdd("Undefined symbol " + opd);
			return -1;
		}
		setLabel(loc, rev, 0);
		currLoc = adr;
		setLabel(loc, !rev, 0);
		return 0x100000 | adr;
	}

	private int processRep(String opd) {
		rep = -1;
		try {
			rep = Integer.valueOf(opd);
		} catch (Exception ee) {}
		if (rep <= 0) {
			errsAdd("Invalid REP count " + opd);
			return -1;
		}
		return 0;
	}

	private int processMorg(String loc, String opd, boolean rev) {
		int adr;
		rep = 0;
		adr = Integer.valueOf(opd);
		if (((adr - 1) & adr) != 0) {
			errsAdd("MORG not power-of-two");
			return -1;
		}
		--adr;
		setLabel(loc, rev, 0);
		currLoc = (currLoc + adr) & ~adr;
		setLabel(loc, !rev, 0);
		return 0x100000 | currLoc;
	}

	private int processEqu(String loc, String opd) {
		rep = 0;
		int adr = parseAdr(opd, true);
		symTab.put(loc, adr);
		int ret = adr | 0x100000;
		return ret;
	}

	private int processDefSym(char mrk, String loc, boolean rev, String opd) {
		setLabel(loc, rev, 0);
		int adr = parseAdr(opd, false);
		// TODO: support adr[,adr[,var]] - and how to use that...
		int len = adrMode;
		code = new byte[adrMode];
		putAdr(code, 0, adr);
		code[0] |= 0100;
		if (mrk == 'L') {
			code[0] |= 0200;
		}
		if (mrk == 'R') {
			code[len - 1] |= 0200;
		}
		currLoc += len;
		setLabel(loc, !rev, len);
		// TODO: does REP apply?
		if (rep > 0) {
			currLoc += (rep - 1) * len;
		}
		return adrMode;
	}

	private int processRange(String opd) {
		rep = 0;
		String[] opds = opd.split(",");
		if (opds.length != 2 || opds[0].length() == 0 || opds[1].length() == 0) {
			errsAdd("RANGE requires 2 parameters");
			return -1;
		}
		int a = parseAdr(opds[0], true);
		int b = parseAdr(opds[1], true);
		if (a < 0 || b < 0) {
			errsAdd("RANGE bad address");
			return -1;
		}
		minAdr = a;
		maxAdr = b;
		return 0;
	}

	private int processClear(String opd) {
		rep = 0;
		String[] opds = opd.split(",");
		if (opds.length < 2 || opds[0].length() == 0 || opds[1].length() == 0) {
			errsAdd("CLEAR requires 2 or 3 parameters");
			return -1;
		}
		int a = parseAdr(opds[0], true);
		int b = parseAdr(opds[1], true);
		int c = 0;
		if (opds.length > 2) {
			byte ch;
			if (opds[2].length() == 0) {
				ch = ' ';
			} else {
				ch = (byte)(opds[2].charAt(0) & 0x7f);
			}
			c = cvt.asciiToHw(ch);
		}
		if (a < 0 || b < 0) {
			errsAdd("CLEAR bad address");
			return -1;
		}
		clear = new Clear();
		clear.start = a;
		clear.end = b;
		clear.fill = c;
		return 0;
	}

	private int noImpl(String op) {
		rep = 0;
		errsAdd(op + " not supported");
		return -1;
	}
}
