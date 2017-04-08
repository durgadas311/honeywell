// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.Arrays;
import java.util.Vector;
import java.util.Map;
import java.util.HashMap;

public class Fortran4 implements FortranParser {
	File inFile;
	BufferedReader in;
	String next = null;
	InstrDecode idc;
	CharConverter cvt;
	OutputStream out;
	OutputStream lst;
	int currLoc;
	int lineNo;
	int adrMode;
	private Vector<String> errs;
	private Map<String,Integer> symTab;
	byte[] code;
	boolean end;
	boolean asmPass;
	String prog;
	int endAdr;
	int minAdr;
	int maxAdr;
	byte[] image;
	CoreMemory sys;
	int reloc;
	boolean listing;
	Stack<Integer> doLoops;
	Map<Integer, DoStatement> doStmts;
	Vector<FortranItem> program;

	static final String _DO = "^DO[0-9]+[A-Z][A-Z0-9]*=[A-Z0-9]+,[A-Z0-9]+";
	static final String _SFC = "^[A-Z][A-Z0-9]*([^)]+)=";
	static final String _IF = "^IF("; // may have nested parens
	static final String _ASGN = "^ASSIGN[0-9]+TO[A-Z][A-Z0-9]*$";
	static final String _GOTO = "^GOTO[0-9]+$";
	static final String _GOTOC = "^GOTO([0-9][0-9,]*),[A-Z][A-Z0-9]*$";
	static final String _GOTOA = "^GOTO[A-Z][A-Z0-9]*,([0-9][0-9,]*)$";
	static final String _CONT = "^CONTINUE$";
	static final String _END = "^END$";
	static final String _PAUSE = "^PAUSE[0-7]*$";
	static final String _STOP = "^STOP[0-7]*$";
	static final String _RET = "^RETURN$";
	static final String _CHAIN = "^CALLCHAIN[A-Z0-9]$";
	static final String _CALLP = "^CALL[A-Z][A-Z0-9]*([^)]+)$";
	static final String _CALL = "^CALL[A-Z][A-Z0-9]*$";
	static final String _DIM = "^DIMENSION";
	static final String _COM = "^COMMON";
	static final String _EQU = "^EQUIVALENCE";
	static final String _IFC = "^INTEGERFUNCTION[A-Z][A-Z0-9]*([^)]+)$";
	static final String _RFC = "^REALFUNCTION[A-Z][A-Z0-9]*([^)]+)$";
	static final String _LFC = "^LOGICALFUNCTION[A-Z][A-Z0-9]*([^)]+)$";
	static final String _FNC = "^FUNCTION[A-Z][A-Z0-9]*([^)]+)$";
	static final String _SUB = "^SUBROUTINE[A-Z][A-Z0-9]*([^)]+)$";
	static final String _INT = "^INTEGER";
	static final String _REA = "^REAL";
	static final String _LOG = "^LOGICAL";
	static final String _EXT = "^EXTERNAL";
	static final String _READ = "^READ([^)]+)";
	static final String _WRITE = "^WRITE([^)]+)";
	static final String _FMT = "^FORMAT(.*)$"; // may have nested parens
	static final String _ENDF = "^ENDFILE[A-Z][A-Z0-9]*$";
	static final String _REW = "^REWIND[A-Z][A-Z0-9]*$";
	static final String _BSP = "^BACKSPACE[A-Z][A-Z0-9]*$";
	static final String _LET = "^[A-Z][A-Z0-9]*=";

	// TODO:
	//	Handle ad-hoc constants.
	//	Floating-point constants. (differentiate "F" from symbol?)
	//	Special marks by 'mrk' (column 7)
	//	RESV,fill syntax
	//	DSA ARG,ARG,VAR syntax
	//	DA directive
	//
	public Fortran4(File input) {
		prog = null; // or default to file name?
		inFile = input;
		symTab = new HashMap<String,Integer>();
		doLoops = new Stack<Integer>();
		doStmts = new HashMap<Integer, DoStatement>();
		program = new Vector<FortranItem>();
		errs = new Vector<String>();
		try {
			in = new BufferedReader(new FileReader(inFile));
		} catch (Exception ee) {
			ee.printStackTrace();
			System.exit(1);
		}
		cvt = new CharConverter();
		out = null;
		lst = null;
		sys = null;
		image = null;
		reloc = 0;
		listing = false;
	}

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

	public void listSymTab() {
		int x = 0;
		listOut("Symbol Table:\n");
		for (Map.Entry<String, Integer> entry : symTab.entrySet()) {
			String l = String.format("  %6s %07o", entry.getKey(), entry.getValue());
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

	public int passTwo(CoreMemory sys, int reloc, boolean list) {
		this.reloc = reloc;
		try {
			in = new BufferedReader(new FileReader(inFile));
		} catch (Exception ee) {
			// 'in' should never fail - already validated in ctor.
			ee.printStackTrace();
			return -1;
		}
		this.sys = sys;
		listing = list;
		asmPass = true;
		int ret = 0;
		currLoc = 0;
		lineNo = 0;
		end = false;
		while (!end && (ret = scanOne()) >= 0) {
		}
		try { in.close(); } catch (Exception ee) {}
		if (errs.size() > 0) {
			ret = -1;
		}
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
				listing = true;
			}
		} catch (Exception ee) {
			// 'in' should never fail - already validated in ctor.
			ee.printStackTrace();
			return -1;
		}
		asmPass = true;
		int ret = 0;
		currLoc = 0;
		lineNo = 0;
		end = false;
		while (!end && (ret = scanOne()) >= 0) {
		}
		System.err.format("END OF PASS 2 - %07o %07o %07o\n", minAdr, maxAdr, endAdr);
		if (errs.size() > 0) {
			ret = -1;
		}
		if (ret >= 0 && out != null) {
			objOut(image);
		}
		if (listing) {
			listSymTab();
		}
		try { in.close(); } catch (Exception ee) {}
		try { if (lst != null) lst.close(); } catch (Exception ee) {}
		try { if (out != null) out.close(); } catch (Exception ee) {}
		return ret;
	}

	private void objOut(byte[] b) {
		try {
			out.write(b);
		} catch (Exception ee) {
		}
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

	private int scanOne() {
		// TODO: tolerate "illegal" TAB characters
		// TODO: track source line number with statements
		String line = next;
		code = null;
		int orgLoc = currLoc;
while (true) {
		try {
			next = in.readLine();
			if (next == null && line == null) {
				return -1;
			}
		} catch (Exception ee) {
			ee.printStackTrace();
			return -1;
		}
		++lineNo;
		int e = next.length();
		if (e > 72) {
			e = 72;
		}
		if (line == null) {
			line = next.substring(0, e);;
			continue;
		}
		if (next.length() >= 6 && next.charAt(5) != ' ' && next.charAt(5) != '0') {
			line += next.substring(6, e);
			continue;
		}
		// We have a statement...
		break;
}
		// first do convenience translations of special chars
		line = replaceChars(line.toUpperCase(), CharConverter.hwAsciiSup, CharConverter.hwAsciiRep);
		// May also need to translate for FORTRAN?
		// since only CHARACTER data ends up in program
		// (unprocessed), perhaps only needs to be done there.
		if (line.length() == 0) {
			// TODO: pass-thru to listing?
			return 0;
		}
		if (line.length() < 7) {
			// TODO: error?
			return -1;
		}
		if (line.charAt(0) == 'C') {
			if (listing) {
				String l = line + "\n";
				listOut(l);
			}
			return 0;
		}
		if (line.startsWith(" TITLE")) {
			prog = line.substring(6).trim();
			return 0;
		}
		if (line.startsWith("DATA")) {
			processDATA(line);
			return 0;
		}
		lab = line.substring(0, 5).trim();
		stmt = line.substring(6).replaceAll("\\s", "");

		int labl = -1;
		if (lab.length() > 0) {
			labl = Integer.valueOf(lab);
		}
		if (labl > 0) {
			// TODO: except FORMAT (and?)
			String sym = String.format("$%05d", labl);
			emit("  %-7sRESV  0", sym);
			setLabel(sym, labl); // TODO: check duplicates
		}
		FortranItem itm = null;
		DoStatement do;
		if ((do = DoStatement.parse(stmt, this)) != null) {
			itm = do;
			doLoops.push(do);
			doStmts.put(do.getTerm(), do);
		}
		if (itm == null) {
			itm = StmtFunction.parse(stmt, this);
		}
		itm.label = labl;
		itm.src = currLine;
		program.add(itm);
		return (itm == null ? -1 : 0);
	}

	private void checkDo(FortranItem itm) {
		if (!doStmts.containsKey(itm.label)) {
			return;
		}
		// Might be more than one...
		DoStatement do = doStmts.get(itm.label);
		if (doLoops.peek() != do) {
			// error... how to recover...
			return;
		}
		doLoops.pop();
		// Need to allow duplicates, and enforce order...
		foobar
		do.genLoop(out, this);
		foobar
	}

	private void setDefs() {
		for (FortranItem itm : program) {
			itm.genDefs(out, this);
		}
	}

	private void setCode() {
		for (FortranItem itm : program) {
			// TODO: need to emit label defs...
			if (itm.label > 0) {
				emit(String.format("  #%05d RESV  0", itm.label));
			}
			itm.genCode(out, this);
			checkDo(itm);
		}
	}

	public void setVariable(String var, int val) {
		if (!symTab.containsKey(var)) {
			emit("  %-7sDCW   #%dB%d", var, intWidth, val);
			symTab.put(var, val);
		}
	}

	public void setConst(int const) {
		String sym = String.format("=%d", const);
		setVariable(sym, const);
	}

	public void emit(String ezc) {
		out.format("%05d%s\n", ezcLine++, ezc);
	}

	private int parseAdr(String opd, boolean simple) {
		int ix;
		int adr = 0;
		boolean ind = false;
		int idx = 0;	// 0: not a valid index register - no indexing
		if (opd.charAt(0) == '(') {
			if (simple) {
				errs.add("Indirect address not allowed at line " + lineNo);
				return -1;
			}
			if (adrMode < 3) {
				errs.add("Indirect address in 2-char mode at line " + lineNo);
				return -1;
			}
			ix = opd.indexOf(')');
			if (ix < 0) {
				errs.add("Invalid indirect address " + opd + " at line " + lineNo);
				return -1;
			}
			// TODO: can there be residual after ')' ?
			ind = true;
			opd = opd.substring(1, ix);
		}
		ix = opd.lastIndexOf('+');
		if (ix >= 0 && opd.substring(ix).matches("\\+[XY][0-9]+")) {
			if (ind) {
				errs.add("Indexed address with indirect at line " + lineNo);
				return -1;
			}
			if (simple) {
				errs.add("Indexed address not allowed at line " + lineNo);
				return -1;
			}
			if (adrMode < 3) {
				errs.add("Indexed address in 2-char mode at line " + lineNo);
				return -1;
			}
			idx = Integer.valueOf(opd.substring(ix + 2));
			if (idx < 1 || (adrMode == 3 && (opd.charAt(ix + 1) == 'Y' || idx > 6)) || (adrMode > 3 && idx > 15)) {
				errs.add("Invalid indexed address " + opd + " at line " + lineNo);
				return -1;
			}
			if (opd.charAt(ix + 1) == 'Y') {
				idx += 16;
			}
			opd = opd.substring(0, ix);
		}
		ix = opd.indexOf('-');
		if (ix < 0) {
			ix = opd.indexOf('+');
			if (ix < 0) {
				ix = opd.length();
			}
		}
		// TODO: handle possible 3rd arg - index ref.
		String sym = opd.substring(0, ix);
		if (Character.isDigit(sym.charAt(0))) {
			adr = Integer.valueOf(sym);
		} else if (sym.equals("*")) {
			adr = currLoc;
		} else {
			if (symTab.containsKey(sym)) {
				adr = symTab.get(sym);
			} else if (asmPass) {
				errs.add("Undefined symbol " + sym + " at line " + lineNo);
				return -1;
			}
		}
		if (ix < opd.length()) {
			int a = Integer.valueOf(opd.substring(ix + 1));
			if (opd.charAt(ix) == '-') {
				adr -= a;
			} else {
				adr += a;
			}
		}
		if (ind) {
			if (adrMode == 3) {
				adr |= 0700000;
			} else { // must be 4
				adr |= 040000000;
			}
		} else if (idx > 0) {
			if (adrMode == 3) {
				adr |= (idx << 15);
			} else { // must be 4
				adr |= (idx << 19);
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
		byte v = 0;
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
				e = opd.length();
			}
			byte[] bb = new byte[e - 1];
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
				bb[y] = (byte)(opd.charAt(y + 1) & 017);
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
			if (symTab.containsKey(loc) && symTab.get(loc) != v) {
				errs.add("Redefined symbol " + loc + " at line " + lineNo);
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
			errs.add(String.format("Invalid op code %02o at line %d", op, lineNo));
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
				errs.add("Reqd A field missing at line " + lineNo);
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
				errs.add("Reqd B field missing at line " + lineNo);
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
				errs.add("Reqd C field missing at line " + lineNo);
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
			errs.add("Reqd variants missing at line " + lineNo);
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
			return processMorg(loc, opd, rev);
		} else if (opc.equals("LITORG")) {
			return noImpl(opc);
		} else if (opc.equals("ADMODE")) {
			int m = Integer.valueOf(opd);
			if (m < 2 || m > 4) {
				errs.add("Invalid argument " + opd);
				return -1;
			}
			adrMode = m;
			return 0;
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
			int ret = 0;
			if (opd.length() > 0) {
				endAdr = parseAdr(opd, true);
				ret = endAdr | 0x100000;
			}
			end = true;
			return ret;
		}
		return noImpl(opc);
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
			errs.add("Could not parse constant " + opd);
			return -1;
		}
		int len = code.length;
		setMarks(code, mrk, mark);
		currLoc += len;
		setLabel(loc, !rev, len);
		return len;
	}

	private int processResv(char mrk, String loc, boolean rev, String opd, int fill) {
		setLabel(loc, rev, 0);
		int len = Integer.valueOf(opd);
		int ret = currLoc | 0x100000;
		if (asmPass && fill >= 0) {
			// could this be excessively large?
			code = new byte[len];
			Arrays.fill(code, (byte)fill);
		}
		currLoc += len;
		setLabel(loc, !rev, len);
		return ret;
	}

	private int processOrg(String loc, String opd, boolean rev) {
		int adr;
		if (Character.isDigit(opd.charAt(0))) {
			adr = Integer.valueOf(opd);
		} else if (symTab.containsKey(opd)) {
			adr = symTab.get(opd);
		} else {
			errs.add("Undefined symbol " + opd + " at line " + lineNo);
			return -1;
		}
		setLabel(loc, rev, 0);
		currLoc = adr;
		setLabel(loc, !rev, 0);
		return 0x100000 | adr;
	}

	private int processMorg(String loc, String opd, boolean rev) {
		int adr;
		adr = Integer.valueOf(opd);
		if (((adr - 1) & adr) != 0) {
			errs.add("MORG not power-of-two at line " + lineNo);
			return -1;
		}
		--adr;
		setLabel(loc, rev, 0);
		currLoc = (currLoc + adr) & ~adr;
		setLabel(loc, !rev, 0);
		return 0x100000 | currLoc;
	}

	private int processEqu(String loc, String opd) {
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
		return adrMode;
	}

	private int noImpl(String op) {
		errs.add(op + " not supported at line " + lineNo);
		return -1;
	}
}
