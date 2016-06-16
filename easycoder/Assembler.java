import java.io.*;
import java.util.Arrays;
import java.util.Vector;
import java.util.Map;
import java.util.HashMap;

public class Assembler {
	File inFile;
	BufferedReader in;
	FileOutputStream out;
	FileOutputStream lst;
	int currLoc;
	int lineNo;
	int adrMode;
	private Vector<String> errs;
	private Map<String,Integer> symTab;
	byte[] code;
	boolean end;
	String prog;

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
		inFile = input;
		symTab = new HashMap<String,Integer>();
		errs = new Vector<String>();
		try {
			in = new BufferedReader(new FileReader(inFile));
		} catch (Exception ee) {
			ee.printStackTrace();
			System.exit(1);
		}
		out = null;
		lst = null;
	}

	public int passOne() {
		asmPass = false;
		int ret;
		currLoc = 0;
		lineNo = 0;
		end = false;
		while (!end && (ret = scanOne()) >= 0) {
		}
		try {
			in.close();
		} catch (Exception ee) {}
		return ret;
	}

	public int passTwo(File output, File list) {
		try {
			in = new BufferedReader(new FileReader(inFile));
			if (output != null) {
				out = new FileOutputStream(output);
			}
			if (list != null) {
				lst = new FileOutputStream(list);
			}
		} catch (Exception ee) {
			// 'in' should never fail - already validated in ctor.
			ee.printStackTrace();
			return -1;
		}
		asmPass = true;
		int ret;
		currLoc = 0;
		lineNo = 0;
		end = false;
		while (!end && (ret = scanOne()) >= 0) {
		}
		try {
			in.close();
		} catch (Exception ee) {}
		return ret;
	}

	private void objOut(byte[] b) {
		try {
			out.write(b);
		} catch (Exception ee) {
		}
	}

	private void listOut(String str) {
		try {
			lst.write(str.byteArray());
		} catch (Exception ee) {
		}
	}

	private int scanOne() {
		String line = "";
		code = null;
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
		// TODO: handle D data cards... C/L continuation and macro...
		String typ = line.substring(5, 6);
		if (typ.equals("*") || typ.equals("T")) {
			if (lst != null) {
				String l = "                                 " + line + "\n";
				listOut(l);
			}
			return 0;
		}
		String mrk = line.substring(6, 7);
		String loc;
		String opc;
		String opd;
		if (true) {
			loc = line.substring(7, 14);
			opc = line.substring(14, 20).trim();
			opd = line.substring(20);
		} else {
			loc = line.substring(7, 18);
			opc = line.substring(18, 24).trim();
			opd = line.substring(24);
		}
		boolean rev = false;
		if (loc.startsWith(" ")) {
			rev = true;
			loc = loc.substring(1).trim();
		} else {
			loc = loc.trim();
		}
		int e = opd.indexOf(' ');
		if (e >= 0) {
			opd = opd.substring(0, e).trim();
		}
		byte op = idc.getOp(opc);
		if (op != InstrDecode.OP_ILL) {
			e = processMachCode(op, loc, rev, opd);
		} else {
			e = processAsmDir(opc, mrk, loc, rev, opd);
		}
		if (!asmPass) {
			return e;
		}
		if (out != null && code != null) {
			objOut(code);
		}
		if (lst != null) {
			String l = "";
			if (code != null) {
				for (int y = 0; y < code.length; ++y) {
					l += String.format("%02o", code[y] & 077);
				}
				if (l.length() > 16) {
					l = l.substring(0, 16) + "+";
				}
				l = String.format("     %07o %c  %-17s", orgLoc, 'W', l);
			} else if (e >= 0x100000) { // special case for some ASM directives
				l = String.format("     %07o                     ",
					(e & 0x0fffff));
			} else {
				l = "                                 ";
			}
			listOut(l + line + "\n");
		}
		while (errs.size() > 0) {
			String l = errs.remove(0);
			if (lst != null) {
				listOut("*** " + l + "\n");
			}
			System.err.println(l);
		}
		return e;
	}

	private int parseAdr(String opd) {
		int ix = opd.indexOf('-');
		int adr = 0;
		if (ix < 0) {
			ix = opd.indexOf('+');
			if (ix < 0) {
				ix = opd.length();
			}
		}
		// TODO: handle possible 3rd arg - index ref.
		String sym = opd.substring(0, ix);
		if (Character.isDigit(sym.charAt(0)) {
			adr = Integer.valueOf(sym);
		} else if (sym.equals("*")) {
			adr = currLoc;
		} else {
			if (symTab.contains(sym)) {
				adr = symTab.get(sym);
			} else if (asmPass) {
				errs.add("Undefined symbol " + sym);
				return -1;
			}
		}
		if (ix < opd.length()) {
			int a = Integer.valueOf(opd.substring(ix + 1));
			if (opd.charAt(ix).equals('-')) {
				adr -= a;
			} else {
				adr += a;
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

	private byte parseVar(String opd) {
		byte v;
		try {
			v = Byte.valueOf(opd, 8);
		} catch (Exception ee) {
		}
		return v;
	}

	private byte[] parseCon(String opd) {
		char c = opd.charAt(0);
		if (c == '@') {
			int e = opd.indexOf('@', 1);
			if (e < 0) {
			}
			byte[] bb = new byte[e];
			for (int y = 1; y < e; ++y) {
				bb[y - 1] = cvt.asciiToHw((byte)(opd.charAt(y) & 0x7f));
			}
			return bb;
		} else if (c == '#') {
			int e = opd.indexOf('B', 1);
			int base = 10;
			if (e < 0) {
				e = opd.indexOf('C', 1);
				base = 8;
			}
			if (e < 0) {
				e = opd.indexOf('A', 1);
				base = 0; // characters
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
				int f = opd.length() - e;
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
			for (int y = n - 1; y >= 0; --y) {
				bb[y] = (byte)(l & 077);
				l >>= 6;
			}
			return bb;
		} else if (c == '+' || c == '-') {
			int e = opd.length() - 1;
			byte[] bb = new byte[e];
			for (int y = 0; y < e; ++y) {
				// assume all are decimal digits...
				bb[y] = (byte)(opd.charAt(y + 1) & 017)
			}
			bb[e - 1] |= (byte)(c == '-' ? 040 : 020);
			return bb;
		} else {
			errs.add("Unsupported constant " + opd);
			return null;
		}
	}

	private void setLabel(String loc, boolean set, int len) {
		if (loc.length() > 0 && set) {
			int v = currLoc;
			if (len > 0) {
				--v;
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

	private int processMachCode(byte op, String loc, boolean rev, String opd) {
		int flags = idc.getFlags(op);
		int xflags = 0;
		int a = 0;
		int b = 0;
		byte[] v = null;
		String[] opds = opd.split(",");
		int ox = 0;

		if ((flags & OP_INVAL) != 0) { // not possible?
			return -1;
		}
		if ((flags & OP_SPC) != 0) {
			op = 065;
		}
		int il = 1; // always has an opcode
		// General Rule, optional operands:
		// Right operand only present if left one(s) also present.
		// But this is based on "HAS" flags - an instruction may not
		// have A or B but may have V. But, if it has A and A was
		// not provided then it cannot have B or V.
		if ((flags & OP_HAS_A) != 0) {
			if (ox < opds.length) {
				a = parseAdr(opds[ox]);
				il += adrMode;
				xflags |= OP_HAS_A;
				++ox;
			} elseif ((flags & OP_REQ_A) != 0) {
				errs.add("Reqd A field missing at line " + lineNo);
				return -1;
			}
		}
		if ((flags & OP_HAS_B) != 0) {
			if (ox < opds.length) {
				b = parseAdr(opds[ox]);
				il += adrMode;
				xflags |= OP_HAS_B;
				++ox;
			} else if ((flags & OP_REQ_B) != 0) {
				errs.add("Reqd B field missing at line " + lineNo);
				return -1;
			}
		}
		if ((flags & OP_HAS_V) != 0) {
			if (ox < opds.length) {
				int n = opds.length - ox;
				// TODO: req'd number of variants?
				v = new byte[n];
				for (int y = 0; y < n; ++y) {
					v[y] = parseVar(opds[ox + y]);
				}
				il += n;
				xflags |= OP_HAS_V;
				ox = opds.length;
			} else if ((flags & OP_REQ_V) != 0) {
				errs.add("Reqd variants missing at line " + lineNo);
				return -1;
			}
		}
		if (!asmPass) {
			currLoc += il;
			return il;
		}
		code = new byte[il];
		int x = 0;
		// TODO: punctuation is a variable...
		code[x++] = op | 0100;
		if ((xflags & OP_HAS_A) != 0) {
			putAdr(code, x, a);
			x += adrMode;
		}
		if ((xflags & OP_HAS_B) != 0) {
			putAdr(code, x, b);
			x += adrMode;
		}
		if ((xflags & OP_HAS_V) != 0) {
			for (int y = 0; y < v.length; ++y) {
				code[x++] = (byte)(v[y] & 077);
			}
		}
		currLoc += code.length;
		return code.length;
	}

	private int processAsmDir(String opc, String mrk, String loc, boolean rev,
						String opd) {
		if (opc.equals("DCW")) {
			return processDefCon(mrk, loc, rev, opd, true);
		} else if (opc.equals("DC")) {
			return processDefCon(mrk, loc, rev, opd, false);
		} else if (opc.equals("RESV")) {
			return processResv(mrk, loc, rev, opd);
		} else if (opc.equals("DSA")) {
			return processDefSym(mrk, loc, rev, opd);
		} else if (opc.equals("DA")) {
			return noImpl(opc);
		} else if (opc.equals("PROG")) {
			prog = opd;
			return 0;
		} else if (opc.equals("SEG")) {
			return noImpl(opc);
		} else if (opc.equals("EX")) {
			return noImpl(opc);
		} else if (opc.equals("XFR")) {
			return noImpl(opc);
		} else if (opc.equals("ORG")) {
			return processOrg(loc, opd, rev);
		} else if (opc.equals("MORG")) {
			return noImpl(opc);
		} else if (opc.equals("LITORG")) {
			return noImpl(opc);
		} else if (opc.equals("ADMODE")) {
			int m = Integer.valueOf(opd);
			if (m < 2 || m > 4) {
				errs.add("Invalid argument " + opd);
				return -1;
			}
			adrMode = m;
		} else if (opc.equals("EQU")) {
			return processEqu(loc, opd);
		} else if (opc.equals("CEQU")) {
			return noImpl(opc);
		} else if (opc.equals("SKIP")) {
			return noImpl(opc);
		} else if (opc.equals("SFX")) {
			return noImpl(opc);
		} else if (opc.equals("REP")) {
			return noImpl(opc);
		} else if (opc.equals("GEN")) {
			return noImpl(opc);
		} else if (opc.equals("SETLIN")) {
			return noImpl(opc);
		} else if (opc.equals("XBASE")) {
			return noImpl(opc);
		} else if (opc.equals("RANGE")) {
			return noImpl(opc);
		} else if (opc.equals("CLEAR")) {
			return noImpl(opc);
		} else if (opc.equals("END")) {
			// TODO: pull start address...
			end = true;
			return 0;
		}
	}

	private int processDefCon(String mrk, String loc, String rev, String opd, boolean mark) {
		setLabel(loc, !rev, 0);
		code = parseCon(opd);
		if (code == null) {
			return -1;
		}
		int len code.length;
		if (mark) {
			code[0] |= 0100;
		}
		// TODO: special marks by 'mrk'
		currLoc += len;
		setLabel(loc, rev, len);
		return len;
	}

	private int processResv(String mrk, String loc, String rev, String opd) {
		setLabel(loc, !rev, 0);
		int len = Integer.valueOf(opd);
		int ret = currLoc | 0x100000;
		currLoc += len;
		setLabel(loc, rev, len);
		return ret;
	}

	private int processOrg(String loc, String opd, boolean rev) {
		int adr;
		if (Character.isDigit(opd.charAt(0)) {
			adr = Integer.valueOf(opd);
		} else if (symTab.contains(opd)) {
			adr = symTab.get(sym);
		} else {
			errs.add("Undefined symbol " + opd);
			return -1;
		}
		setLabel(loc, rev, 0);
		currLoc = adr;
		setLabel(loc, !rev, 0);
		return 0x100000 | adr;
	}

	private int processEqu(String loc, String opd) {
		int adr = parseAdr(opd);
		symTab.put(loc, adr);
		int ret = adr | 0x100000;
		return ret;
	}

	private int processDefSym(String mrk, String loc, String rev, String opd) {
		setLabel(loc, !rev, 0);
		int adr = parseAdr(opd);
		code = new byte[adrMode];
		putAdr(code, 0, adr);
		currLoc += adrMode;
		setLabel(loc, rev, adrMode);
		return adrMode;
	}

	private int noImpl(String op) {
		errs.add(op + " not supported");
		return -1;
	}
}
