// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.Arrays;
import java.util.Vector;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.Stack;

public class Fortran4 implements Compiler, FortranParser {
	File inFile;
	BufferedReader in;
	String next = null;
	CharConverter cvt;
	PrintStream out;
	PrintStream lst;
	int currLoc;
	int lineNo;
	int curLine;
	int ezcLine;
	int adrMode = 3;
	private Vector<String> errs;
	private Map<String, FortranOperand> symTab;
	private Map<String, FortranOperand> allSyms;
	private Map<String, FortranArray> arrays;
	private Vector<RunTimeLibrary> libs;
	boolean inProg;
	boolean inSubr; // SUBR or FUNC, also inProg...
	boolean liveExpr;
	FortranOperand curSubr;
	CodeImpliedDo doScope = null;
	boolean end;
	String prog;
	int endAdr;
	int minAdr;
	int maxAdr;
	CoreMemory sys;
	int reloc;
	boolean listing;
	Stack<DoStatement> doLoops;
	Map<Integer, DoStatement> doStmts;
	Vector<FortranItem> program;
	int[] implicits;
	int intPrec = 3;
	int[] devices;
	private int itmp = 0;
	private int ltmp = 0;
	private int rtmp = 0;
	private int atmp = 0;
	private int xtmp = 0;
	private boolean ezcListing = false;
	private boolean symListing = true;
	private boolean wantDump = false;
	private boolean data = false;

	public Fortran4(File input) {
		prog = null; // or default to file name?
		inFile = input;
		implicits = new int[26];
		Arrays.fill(implicits, FortranOperand.REAL);
		for (int x = 'I'; x <= 'N'; ++x) {
			implicits[x - 'A'] = FortranOperand.INTEGER;
		}
		libs = new Vector<RunTimeLibrary>();
		symTab = new HashMap<String, FortranOperand>();
		allSyms = new HashMap<String, FortranOperand>();
		arrays = new HashMap<String, FortranArray>();
		doLoops = new Stack<DoStatement>();
		doStmts = new HashMap<Integer, DoStatement>();
		program = new Vector<FortranItem>();
		errs = new Vector<String>();
		// TODO: Tape Units... dev 5 => unit 1, dev 4 => unit 0,
		// others TBD. Also, how to rewrite if devices re-assigned.
		devices = new int[]{
			007, 007, 010, 020, 000, 001, 002, 003, 004,
			005, 006, 007, 007, 007, 007, 007, 007, 007,
		};
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
		reloc = 0;
		listing = false;
		libs.add(new FortranLibCalls(this));
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

	public Map<String, String> getSymTab() {
		Map<String, String> ret = new HashMap<String, String>();
		for (Map.Entry<String, FortranOperand> entry : allSyms.entrySet()) {
			ret.put(entry.getKey(), entry.getValue().name());
		}
		return ret;
	}

	public void listSymTab() {
		int x = 0;
		listOut("Symbol Table:\n");
		Map<String, FortranOperand> sorted =
			new TreeMap<String, FortranOperand>(allSyms);
		for (Map.Entry<String, FortranOperand> entry : sorted.entrySet()) {
			String e;
			String s1 = entry.getKey();
			FortranOperand fo = entry.getValue();
			if (s1.equals(fo.name())) {
				e = String.format("%s %s", s1, fo.getType());
			} else {
				e = String.format("%s=%s %s",
					s1, fo.name(), fo.getType());
			}
			String l = String.format("  %-25s", e);
			++x;
			if (x >= 4) {
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

	public int compile(CoreMemory sys, boolean lst) {
		this.sys = sys;
		listing = lst;
		int e = compile();
		this.sys = null;
		return e;
	}

	public int compile(PrintStream list) {
		try {
			if (list != null) {
				lst = list;
				listing = true;
			}
		} catch (Exception ee) {
			//ee.printStackTrace();
			return -1;
		}
		int e = compile();
		return e;
	}

	private int compile() {
		int ret = 0;
		lineNo = 0;
		end = false;
		inProg = false;
		inSubr = false;
		while (!end && (ret = scanOne()) >= 0) {
		}
		if (errs.size() > 0) {
			ret = -1;
		}
		if (listing) {
			for (String l : errs) {
				listOut("*** " + l + "\n");
			}
			listOut(String.format("END OF COMPILE%s\n",
					ret < 0 ? " (ERRORS)" : ""));
		}
		if (!data) {
			try { in.close(); } catch (Exception ee) {}
		}
		return ret;
	}

	public boolean listEasyCoder() { return ezcListing; }
	public boolean listSymbols() { return symListing; }
	public boolean wantsDump() { return wantDump; }
	public boolean hasData() { return data; }
	public BufferedReader getData() { return in; }
	public int lineCount() { return lineNo; }

	public int generate(File output) {
		if (output == null) {
			errsAdd("No output file");
			return -1;
		}
		try {
			out = new PrintStream(output);
		} catch (Exception ee) {
			ee.printStackTrace();
			return -1;
		}
		int ret = 0;
		ezcLine = 1;
		// Can errors be generated during this phase?
		emit(String.format("         PROG  %s", prog));
		emit(String.format("         ADMODE%d", adrMode));
		emit("         ORG   1340");
		for (RunTimeLibrary rtl : libs) {
			rtl.genLib(this);
		}
		//
		setDefs();	// generate all variables/constants
		//
		emit(String.format("  $START CAM   %02o", adrMode == 4 ? 060 : 000));
		for (RunTimeLibrary rtl : libs) {
			rtl.genCode(this);
		}
		//
		setCode();	// generate program code
		//
		// Termination handled by END statements...
		// END => STOP or RETURN...
		emit("  $ENDAA SCR   $ENDZZ,70");
		for (RunTimeLibrary rtl : libs) {
			rtl.genExit(this);
		}
		emit("   $ENDZZB     0");
		emit("         NOP");	// ensure WM after last instruction
		//
		setData();	// arrays, common(?), un-initialized data
		//
		emit("         END   $START");
		if (errs.size() > 0) {
			ret = -1;
		}
		try { if (out != null) out.close(); } catch (Exception ee) {}
		return ret;
	}

	private void processDUMP(String ln) {
		wantDump = true;
	}

	private void processDATA(String ln) {
		end = true;
		inProg = false;
		inSubr = false;
		data = true;
	}

	// This doesn't really work as advertized... We don't yet
	// support multi-job runs at this level.
	private void processALTER(String ln) {
		String[] args = ln.split("[ ,]+");
		for (int x = 1; x < args.length; ++x) {
			String s = args[x];
			if (s.equals("SAVE")) {
			} else if (s.equals("PUNCH")) {
			} else if (s.equals("NOLIST")) {
				// do NOT list symbols...
				symListing = false;
			} else if (s.equals("LIST")) {
				// DO list EasyCoder output
				ezcListing = true;
			} else {
				errsAdd("Invalid *ALTER option " + s);
			}
		}
	}

	private void processJOBID(String ln) {
		String[] args = ln.split("[ ,]+");
		for (int x = 1; x < args.length; ++x) {
			String s = args[x];
			if (s.startsWith("IO")) {
				// I/O devices IOiioopp (reader, printer, punch)
				// alter devices[] accordingly...
			} else if (s.startsWith("*")) {
			} else if (s.startsWith("M")) {
				// memory limits - affects address mode?
				int m = Integer.valueOf(s.substring(1));
				if (m >= 32768) {
					adrMode = 4;
					if (intPrec < adrMode) {
						intPrec = adrMode;
					}
				}
			} else if (s.startsWith("F")) {
				// all REAL same size...
			} else if (s.startsWith("I")) {
				int p = Integer.valueOf(s.substring(1));
				if (p >=3 && p <= 12) {
					intPrec = p;
				}
				if (intPrec < adrMode) {
					intPrec = adrMode;
				}
			} else if (s.equals("SAVE")) {
			} else if (s.equals("PUNCH")) {
			} else if (s.equals("NOLIST")) {
				// do NOT list symbols...
				symListing = false;
			} else if (s.equals("LIST")) {
				// DO list EasyCoder output
				ezcListing = true;
			} else if (s.equals("TAPEIP")) {
			} else {
				errsAdd("Invalid *JOBID option " + s);
			}
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

	private void listSrc(String src) {
		listOut(String.format("%5d %s\n", lineNo, src));
	}

	private int scanOne() {
		// TODO: tolerate "illegal" TAB characters
		// TODO: track source line number with statements
		String line = next;
		next = null;
		curLine = lineNo;
		// Do not read-ahead for "*XXX" Job Ctl Cards...
		while (true) {
			if (line != null && line.length() > 0 &&
					line.charAt(0) == '*') {
				// Process JCL before doing read-ahead.
				// JCL does not support continuation.
				break;
			}
			try {
				next = in.readLine();
				if (next == null && line == null) {
					end = true;
					return 0;
				}
			} catch (Exception ee) {
				ee.printStackTrace();
				return -1;
			}
			if (next != null) {
				++lineNo;
				if (listing) {
					listSrc(next);
				}
				if (next.length() > 0 && next.charAt(0) == '*' &&
							line != null) {
					// process FORTRAN card before JCL
					break;
				}
				int e = next.length();
				if (e > 72) {
					e = 72;
				}
				if (line == null) {
					curLine = lineNo;
					line = next.substring(0, e);;
					next = null;
					continue;
				}
				if (inProg && next.length() >= 6 &&
						next.charAt(0) != 'C' &&
						next.charAt(5) != ' ' &&
						next.charAt(5) != '0') {
					line += next.substring(6, e);
					next = null;
					continue;
				}
			}
			// We have a statement...
			break;
		}
		// first do convenience translations of special chars
		//line = replaceChars(line.toUpperCase(), CharConverter.hwAsciiSup, CharConverter.hwAsciiRep);
		line = line.toUpperCase();
		if (line.length() == 0) {
			// TODO: pass-thru to listing? (already done)
			return 0;
		}
		if (line.charAt(0) == 'C') {
			// inProg = true; ?
			return 0;
		}
		// TODO: some of these must be before any FORTRAN cards...
		// ... or must they?
		if (!inProg) {
			if (line.startsWith(" TITLE")) {
				prog = line.substring(6).trim();
				return 0;
			}
			if (!inProg && line.startsWith("*JOBID")) {
				processJOBID(line);
				return 0;
			}
			if (!inProg && line.startsWith("*ALTER")) {
				processALTER(line);
				return 0;
			}
			if (!inProg && line.startsWith("*DUMP")) {
				processDUMP(line);
				return 0;
			}
		}
		// TODO: support in-stream data
		// (support compiling from punch cards in general!)
		if (line.startsWith("*DATA")) {
			processDATA(line);
			return 0;
		}
		// These should not appear to us.
		if (line.startsWith("*ENDDATA") ||
				line.startsWith("1EOF ")) {
			return 0;
		}
		if (line.startsWith("*")) {
			// another type of comment card
			return 0;
		}
		// Must be a FORTRAN non-comment card...
		if (line.length() < 7) {
			// TODO: error?
			return 0;
		}
		String lab = line.substring(0, 5).trim();
		String stmt = squeeze(line, 6);

		int labl = -1;
		if (lab.length() > 0) {
			labl = Integer.valueOf(lab);
		}
		FortranItem itm = null;
		liveExpr = false;
		// TODO: enforce inProg for these...
		itm = DoStatement.parse(stmt, this);
		if (itm == null) { itm = StmtFunction.parse(stmt, this); }
		if (itm == null) { itm = IfStatement.parse(stmt, this); }
		if (itm == null) { itm = FormatStatement.parse(stmt, this); }
		if (itm == null) { itm = DimStatement.parse(stmt, this); }
		if (itm == null) {
			itm = ProgramStatement.parse(stmt, this);
			// TODO: error-check state?
			if (itm != null) inProg = true; // same for SUBR/FUNC
		}
		if (itm == null) {
			SubrStatement sub = SubrStatement.parse(stmt, this);
			// TODO: error-check state?
			if (sub != null) {
				itm = sub;
				inProg = true;
				inSubr = true;
				curSubr = sub.getSubr();
			}
		}
		if (itm == null) {
			FuncStatement sub = FuncStatement.parse(stmt, this);
			// TODO: error-check state?
			if (sub != null) {
				itm = sub;
				inProg = true;
				inSubr = true;
				curSubr = sub.getFunc();
			}
		}
		if (itm == null) {
			itm = EndStatement.parse(stmt, this);
			if (itm != null) {
				inProg = false;
				inSubr = false;
				allSyms.putAll(symTab);
				symTab.clear();
			}
		}
		if (itm == null) { itm = DefStatement.parse(stmt, this); }
		if (itm == null) { itm = ImplStatement.parse(stmt, this); }
		if (itm == null) { itm = DataStatement.parse(stmt, this); }
		// The above statements CANNOT be target of IF, parseAction() CAN.
		if (itm == null) { itm = parseAction(stmt); }
		if (itm == null) {
			errsAdd(String.format("Unrecognized statement \"%s\"", line));
			return -1; // or continue?
		}
		itm.label = labl;
		itm.src = curLine;
		program.add(itm);
		return (itm == null ? -1 : 0);
	}

	private FortranItem parseAction(String stmt) {
		FortranItem itm = null;
		if (itm == null) { itm = WriteStatement.parse(stmt, this); }
		if (itm == null) { itm = ReadStatement.parse(stmt, this); }
		if (itm == null) { itm = GotoStatement.parse(stmt, this); }
		if (itm == null) { itm = AGotoStatement.parse(stmt, this); }
		if (itm == null) { itm = CGotoStatement.parse(stmt, this); }
		if (itm == null) { itm = AsgnStatement.parse(stmt, this); }
		if (itm == null) { itm = ContStatement.parse(stmt, this); }
		if (itm == null) { itm = LetStatement.parse(stmt, this); }
		if (itm == null) { itm = StopStatement.parse(stmt, this); }
		if (itm == null) { itm = CallStatement.parse(stmt, this); }
		if (itm == null) { itm = ReturnStatement.parse(stmt, this); }
		if (itm == null) { itm = TapeCtlStatement.parse(stmt, this); }
		return itm;
	}

	private String squeeze(String in, int e) {
		int n = in.length();
		boolean q = false;
		int i = 0;
		int h = 0;
		String s = "";
		for (int x = e; x < n; ++x) {
			char c = in.charAt(x);
			if (c == ' ' && !q && h == 0) {
				continue;
			}
			s += c;
			if (c == '\'' && h == 0) {
				q = !q;
				continue;
			}
			if (h > 0) {
				--h;
				continue;
			}
			if (q) {
				continue;
			}
			if (c == 'H' && i > 0) {
				h = i;
				continue;
			}
			if (!Character.isDigit(c)) {
				i = 0;
				continue;
			}
			i = (i * 10) + (c - '0');
		}
		return s;
	}

	private void checkDo(FortranItem itm) {
		if (!doStmts.containsKey(itm.label)) {
			return;
		}
		// Might be more than one...
		DoStatement du = doStmts.get(itm.label);
		if (doLoops.peek() != du) {
			// error... how to recover...
			return;
		}
		// Need to allow duplicates, and enforce order...
		do {
			if (doLoops.peek() == du) { // should always be true
				doLoops.pop();
			}
			du.genLoop(this);
			du = du.getNext();
		} while (du != null);
	}

	private void setDefs() {
		for (FortranOperand fo : allSyms.values()) {
			fo.genDefs(this);
		}
		for (FortranItem itm : program) {
			itm.genDefs(this);
		}
	}

	private void setCode() {
		for (FortranItem itm : program) {
			emit(String.format("         SETLIN%d", itm.src));
			if (itm.label > 0) {
				emit(String.format("  $%05d RESV  0", itm.label));
			}
			if (itm instanceof DoStatement) {
				DoStatement du = (DoStatement)itm;
				doLoops.push(du);
				if (doStmts.containsKey(du.getTerm())) {
					du.setNext(doStmts.get(du.getTerm()));
				}
				doStmts.put(du.getTerm(), du);
			}
			itm.genCode(this);
			checkDo(itm);
		}
	}

	private void setData() {
		for (FortranArray ary : arrays.values()) {
			ary.genData(this);
		}
	}

	private int uniq = 0;
	public String uniqueName() {
		return String.format("/U%04d", uniq++);
	}

	public void errsAdd(int line, String err) {
		// TODO: intersperse errors in listing... if compile phase
		errs.add(err + " at line " + line);
	}

	public void errsAdd(String err) {
		errsAdd(curLine, err);
	}

	// finds next comman that is at same paren level
	public int matchingComma(String str, int start) {
		int x = start;
		int y = x;
		int p = 0;
		int n = str.length();
		while (y < n && (str.charAt(y) != ',' || p > 0)) {
			if (str.charAt(y) == '(') {
				++p;
			} else if (str.charAt(y) == ')') {
				--p;
			}
			++y;
		}
		if (p != 0) {
			return -1;
		}
		return y; // points to comma... or end
	}
	public int matchingParen(String str, int lparen) {
		if (str.charAt(lparen) != '(') {
			return -1;
		}
		int x = lparen;
		int y = x + 1; // start inside first paren...
		int p = 1; // inside 1 paren...
		int n = str.length();
		while (p > 0 && y < n) {
			if (str.charAt(y) == '(') {
				++p;
			} else if (str.charAt(y) == ')') {
				--p;
			}
			++y;
		}
		if (p != 0) {
			return -1;
		}
		return y;
	}

	public int getDev(int code) {
		return devices[code & 017];
	}

	public FortranOperand getSym(String id) {
		if (symTab.containsKey(id)) {
			return symTab.get(id);
		} else {
			return null;
		}
	}

	public void addSym(String id, FortranOperand op) {
		symTab.put(id, op);
	}

	// integer, real, ".TRUE.", ".FALSE.", "nnHxxxx..."
	public FortranOperand parseConstant(String id) {
		FortranOperand fo;
		// must normalize the value for 'id'...
		if (id.equals(".TRUE.") || id.equals(".TRUE.")) {
			boolean v = id.equals(".TRUE.");
			return FortranConstant.get(this, v);
		}
		if (id.matches("[-+]?[0-9]+.*")) {
			int v = Integer.valueOf(id);
			return FortranConstant.get(this, v);
		}
		if (id.matches("[0-9]+H.*") || id.matches("'.*'")) {
			// TODO: how to handle char constants...
			return null;
		}
		if (id.matches("([-+.E0-9]*,[-+.E0-9]*)")) {
			int e = id.length() - 1;
			int x = id.indexOf(',');
			double[] v = new double[2];
			v[0] = Double.valueOf(id.substring(1, x));
			v[1] = Double.valueOf(id.substring(x + 1, e));
			return FortranConstant.get(this, v);
		}
		// must be REAL
		try {
			double v = Double.valueOf(id);
			return FortranConstant.get(this, v);
		} catch (Exception ee) {
		}
		errsAdd(String.format("Invalid constant \"%s\"", id));
		return null;
	}

	public void setImplicit(char ltr, int type) {
		implicits[ltr - 'A'] = type;
	}

	public FortranOperand parseVariable(String id) {
		return parseVariable(id, -1);
	}

	public FortranOperand parseVariable(String id, int type) {
		String dims = null;
		char let = id.charAt(0);
		int p = intPrec;
		boolean uniq = false;
		if (type == FortranOperand.ADDRESS) {
			p = adrMode;
		}
		int x = id.indexOf('(');
		if (x >= 0) {
			int y = matchingParen(id, x); // should be last char...
			if (y < 0) {
				// TODO: need better error handling here
				errsAdd("Malformed array reference");
				//return null; // will this break things?
			}
			dims = id.substring(x + 1, y - 1);
			id = id.substring(0, x);
		}
		FortranOperand fo = null;
		if (dims == null && doScope != null) { // && INTEGER?
			fo = doScope.getVariable(id);
			if (fo != null) {
				return fo;
			}
		}
		String sym = id;
		if (inSubr && Character.isLetter(let)) {
			uniq = true;
			id = curSubr.name() + "." + id;
		}
		if (symTab.containsKey(id)) {
			fo = symTab.get(id);
			if (dims == null) {
				if (type >= 0 && fo instanceof FortranParameter) {
					FortranParameter fp = (FortranParameter)fo;
					if (!fp.setType(type)) {
						errsAdd("Redefinition of parameter after use");
					}
				}
				if (fo instanceof FortranParameter) {
					FortranParameter fp = (FortranParameter)fo;
					fp.reference();
				}
				return fo;
			}
			// references to arrays... confirm fo.type() == ARRAY?
			p = fo.precision();
			type = fo.type();
		} else if (let != '$' && type < 0) {
			type = implicits[let - 'A'];
		}
		if (fo == null && dims != null) {
			errsAdd("Undefined array");
			return null; // will this break things?
		}
		if (uniq) {
			sym = uniqueName();
		}
		// only INTEGER has variable precision, at this level
		if (dims == null) {
			fo = new FortranVariable(sym, type, p);
			addSym(id, fo);
		} else {
			FortranArrayRef fa = parseArrayRef((FortranArray)fo, dims);
			fo = fa;
		}
		return fo;
	}

	// For "early" parsing of function/subroutine parameters... before inSubr
	// These variables are not defined in the normal sequence, as they
	// must be contiguous.
	public FortranParameter parseParameter(String id, FortranOperand scope, int type) {
		String sym = uniqueName();
		// TODO: can parameter types be overridden?
		if (type < 0) {
			type = implicits[id.charAt(0) - 'A'];
		}
		if (scope != null) {
			id = scope.name() + "." + id;
		}
		if (symTab.containsKey(id)) {
			// should not happen - except return values
			FortranOperand fo = symTab.get(id);
			if (fo.kind() != FortranOperand.PARAMETER) {
				return null;
			}
			return (FortranParameter)fo;
		}
		FortranParameter fo = new FortranParameter(sym, type);
		addSym(id, fo);
		return fo;
	}

	public FortranSubprogram parseSubprogram(String id, int type, int argc) {
		if (type == -1) {
			type = implicits[id.charAt(0) - 'A'];
		}
		FortranSubprogram fs;
		// might not be in this current program unit...
		if (symTab.containsKey(id) || allSyms.containsKey(id)) {
			FortranOperand fo;
			if (symTab.containsKey(id)) {
				fo = symTab.get(id);
			} else {
				fo = allSyms.get(id);
			}
			if (fo.kind() != FortranOperand.FUNCTION) {
				errsAdd("Subprogram name conflict");
				return null;
			}
			fs = (FortranSubprogram)fo;
			if (fs.type() < 0 && type >= 0) {
				fs.setType(type, this);
			}
			// TODO: check types?
			if (((FortranSubprogram)fo).numArgs() != argc) {
				errsAdd("Subprogram parameter conflict");
				return null;
			}
			return fs;
		}
		// TODO: track dummy argument types...
		// complicated by possible subsequent (re)defines...
		fs = new FortranSubprogram(id, type, argc, this);
		addSym(id, fs);
		return fs;
	}

	// Only called for DIMENSION, <type> <array-def>, or COMMON (tbd)
	public FortranArray parseArray(String id, int type, int[] dims) {
		String sym = id;
		if (type < 0) {
			type = implicits[id.charAt(0) - 'A'];
		}
		if (inSubr) {
			sym = uniqueName();
			id = curSubr.name() + "." + id;
		}
		if (symTab.containsKey(id)) {
			// TODO: compare dimensions, names, ...
			// TODO: support subprogram parameter as array?
			// requires promoting FortranParameter to FortranArray...
			FortranOperand fo = symTab.get(id);
			if (fo.kind() != FortranOperand.ARRAY) {
				return null;
			}
			return (FortranArray)fo;
		}
		int prec = 0;
		if (type == FortranOperand.INTEGER) {
			prec = intPrec;
		} else if (type == FortranOperand.ADDRESS) {
			prec = adrMode;
		}
		FortranArray fa = new FortranArray(sym, type, prec, dims);
		addSym(id, fa);
		arrays.put(id, fa);
		return fa;
	}

	public FortranArrayRef parseArrayRef(FortranArray ary, String dims) {
		String[] ds = dims.split(",");
		if (liveExpr) {
			int ex = 0;
			if (!dims.matches("[0-9,]+")) {
				errsAdd("Array subscript not a constant");
			} else {
				ex = ary.subscriptValue(ds);
			}
			return new FortranArrayRef(ary, ex);
		} else {
			String ex = ary.subscriptExpr(ds);
			FortranExpr xpr = new FortranExpr(ex, this);
			return new FortranArrayRef(ary, xpr);
		}
	}

	public FortranSubprogram refLibFunc(String fnc) {
		FortranSubprogram ff = null;
		for (RunTimeLibrary rtl : libs) {
			ff = rtl.refLibFunc(fnc, this);
			if (ff != null) {
				break;
			}
		}
		return ff;
	}

	public FortranFuncCall parseFuncCall(FortranSubprogram fnc, String nm, String args) {
		// could include parenthesised expressions... including function calls...
		// this is essentially a FortranExpr... for each arg...
		// that recursion is handled in FortranFuncCall...
		if (fnc == null) {
			fnc = refLibFunc(nm);
			// might still be null, so make fwd-decl
		}
		return new FortranFuncCall(fnc, nm, args, this);
	}

	// '(' array-ref ',' var '=' int ',' int [',' int] ')'
	// Does NOT handle a list in 'array-ref'... TODO
	public String[] parseImpliedDo(String du) {
		int n = du.length();
		if (du.charAt(n - 1) != ')') { return null; }
		int x = 1;
		--n;
		int y = matchingComma(du, x);
		if (y < 0 || y >= n) { return null; }
		String ref = du.substring(x, y);
		// TODO: 'ref' could be another (nested) implied DO...
		String[] refs;
		if (ref.matches("\\(.*\\)")) {
			refs = parseImpliedDo(ref);
			if (refs == null) {
				return null;
			}
		} else {
			refs = new String[]{ ref };
		}
		x = y + 1;
		y = du.indexOf('=', x);
		if (y < 0) { return null; }
		String var = du.substring(x, y);
		String re = "(.*[\\(,])" + var + "([\\),].*)";
		if (!ref.matches(re)) { return null; }
		x = y + 1;
		y = du.indexOf(',', x);
		if (y < 0) { return null; }
		int s = Integer.valueOf(du.substring(x, y));
		x = y + 1;
		y = du.indexOf(',', x);
		if (y < 0) {
			y = n;
		}
		int e = Integer.valueOf(du.substring(x, y));
		x = y + 1;
		int t = 1;
		if (x < n) {
			t = Integer.valueOf(du.substring(x, n));
		}
		Vector<String> vs = new Vector<String>();
		for (int z = s; z <= e; z += t) {
			String rep = String.format("$1%d$2", z);
			for (String rf : refs) {
				vs.add(rf.replaceFirst(re, rep));
			}
		}
		return vs.toArray(new String[0]);
	}

	// TODO: array-ref and function-call?
	public FortranOperand parseOperand(String id) {
		// TODO: do all constants start with +/-/digit?
		char c = id.charAt(0);
		if (Character.isDigit(c) || c == '+' || c == '-' || c == '.') {
			return parseConstant(id);
		} else {
			return parseVariable(id);
		}
	}

	public FortranOperand currSubr() {
		if (inSubr) return curSubr;
		return null;
	}
	public boolean inSubroutine() { return inProg && inSubr; }
	public boolean inMainProg() { return inProg && !inSubr; }

	public void resetTemps() {
		itmp = 0;
		ltmp = 0;
		rtmp = 0;
		atmp = 0;
		xtmp = 0;
	}
	public FortranOperand getIntTemp(int id) {
		return parseVariable(String.format("$ITMP%d", itmp++),
						FortranOperand.INTEGER);
	}

	public FortranOperand getLogTemp(int id) {
		return parseVariable(String.format("$LTMP%d", ltmp++),
						FortranOperand.LOGICAL);
	}

	public FortranOperand getRealTemp(int id) {
		return parseVariable(String.format("$RTMP%d", rtmp++),
						FortranOperand.REAL);
	}

	public FortranOperand getCplxTemp(int id) {
		return parseVariable(String.format("$XTMP%d", xtmp++),
						FortranOperand.COMPLEX);
	}

	public FortranOperand getAdrTemp(int id) {
		return parseVariable(String.format("$ATMP%d", atmp++),
						FortranOperand.ADDRESS);
	}

	public FortranOperand getTemp(int id, int type) {
		switch (type) {
		case FortranOperand.INTEGER:
			return getIntTemp(id);
		case FortranOperand.REAL:
			return getRealTemp(id);
		case FortranOperand.LOGICAL:
			return getLogTemp(id);
		case FortranOperand.COMPLEX:
			return getCplxTemp(id);
		case FortranOperand.ADDRESS:
			return getAdrTemp(id);
		}
		return null;	// serves 'em right
	}

	public void setScope(CodeImpliedDo ido) {
		doScope = ido;
	}
	public void setLive(boolean live) {
		liveExpr = live;
	}

	public void setExpr(FortranExpr expr) {
		expr.genCode(this);
	}

	public void emit(String ezc) {
		out.format("%05d%s\n", ezcLine++, ezc);
	}

	public FortranExpr parseExpr(String expr) {
		return new FortranExpr(expr, this);
	}

	public FortranItem recurse(String stmt) {
		return parseAction(stmt);
	}

	public int getLine() { return curLine; }
	public int addrMode() { return adrMode; }
	public int intPrecision() { return intPrec; }
	public void setName(String nm) {
		prog = nm;
	}
}
