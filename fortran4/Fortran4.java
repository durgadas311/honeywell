// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.Arrays;
import java.util.Vector;
import java.util.Map;
import java.util.HashMap;
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
	boolean inProg;
	boolean inSubr; // SUBR or FUNC, also inProg...
	FortranOperand curSubr;
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

	public Fortran4(File input) {
		prog = null; // or default to file name?
		inFile = input;
		implicits = new int[26];
		Arrays.fill(implicits, FortranOperand.REAL);
		for (int x = 'I'; x <= 'N'; ++x) {
			implicits[x - 'A'] = FortranOperand.INTEGER;
		}
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

	public void listSymTab() {
		int x = 0;
		listOut("Symbol Table:\n");
		for (Map.Entry<String, FortranOperand> entry : allSyms.entrySet()) {
			String l = String.format("  %6s=%7s", entry.getKey(), entry.getValue().name());
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

	public int compile(CoreMemory sys, boolean lst) {
		this.sys = sys;
		listing = lst;
		int e = compile();
		this.sys = null;
		return e;
	}

	public int compile(File list) {
		try {
			if (list != null) {
				lst = new PrintStream(list);
				listing = true;
			}
		} catch (Exception ee) {
			//ee.printStackTrace();
			return -1;
		}
		int e = compile();
		if (listing) {
			listing = false;
			try { lst.close(); } catch (Exception ee) {}
			lst = null;
		}
		return e;
	}

	private int compile() {
		int ret = 0;
		lineNo = 0;
		end = false;
		inProg = false;
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
		try { in.close(); } catch (Exception ee) {}
		return ret;
	}

	public int generate(File output, File list) {
		lst = null;
		if (output == null) {
			errsAdd("No output file");
			return -1;
		}
		try {
			out = new PrintStream(output);
			if (list != null) {
				// TODO: append? or not used at all?
				//lst = new PrintStream(list);
				//listing = true;
			}
		} catch (Exception ee) {
			// 'in' should never fail - already validated in ctor.
			ee.printStackTrace();
			return -1;
		}
		int ret = 0;
		ezcLine = 1;
		// Can errors be generated during this phase?
		emit(String.format("         PROG  %s", prog));
		emit(String.format("         ADMODE%d", adrMode));
		emit("         ORG   1340");
		// TODO: get these from FortranRunTime?
		emit("  $EXIT  DC    #1B0");
		emit("  $ACBOIODC    #1B1");
		emit("         DC    #1B2");
		emit("  $ACBFPHDC    #1B3");
		emit("  $ACBFXPDC    #1B4");
		emit("  EOF    DC    #1B5");
		emit("  EOT    DC    #1B6");
		emit("  $ENDFILDC    #1B7");
		emit("  $REWINDDC    #1B8");
		emit("  $BKSPACDC    #1B9");
		emit("  AINT   DC    #1B10");
		emit("  INT    DC    #1B11");
		emit("  SQRT   DC    #1B12");
		emit("  IAND   DC    #1B13");
		emit("  IOR    DC    #1B14");
		emit("  ICOMPL DC    #1B15");
		emit("  IEXCLR DC    #1B16");
		emit("  FLOAT  DC    #1B17");
		emit("  IFIX   DC    #1B18");
		// TODO: enter templates for library functions...
		// new FortranSubprogram(...) ?
		//
		setDefs();
		emit(String.format("  $START CAM   %02o", adrMode == 4 ? 060 : 000));
		emit("         B     0-1"); // special trap "load runtime"
		emit(" R       DCW   @FORTRAN@"); // runtime to "load"
		setCode();
		// Termination handled by END statements...
		// END => STOP or RETURN...
		emit("         NOP");
		setData();
		emit("         END   $START");
		if (errs.size() > 0) {
			ret = -1;
		}
		if (listing) {
			// listSymTab();
		}
		try { in.close(); } catch (Exception ee) {}
		try { if (lst != null) lst.close(); } catch (Exception ee) {}
		try { if (out != null) out.close(); } catch (Exception ee) {}
		return ret;
	}

	private void processDATA(String ln) {
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
			} else if (s.equals("LIST")) {
			} else if (s.equals("TAPEIP")) {
			} else {
				errsAdd("Invalid *JOBID option " +s);
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
		curLine = lineNo;
		while (true) {
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
				int e = next.length();
				if (e > 72) {
					e = 72;
				}
				if (line == null) {
					curLine = lineNo;
					line = next.substring(0, e);;
					continue;
				}
				if (inProg && next.length() >= 6 &&
						next.charAt(0) != 'C' &&
						next.charAt(5) != ' ' &&
						next.charAt(5) != '0') {
					line += next.substring(6, e);
					continue;
				}
			}
			// We have a statement...
			break;
		}
		// first do convenience translations of special chars
		line = replaceChars(line.toUpperCase(), CharConverter.hwAsciiSup, CharConverter.hwAsciiRep);
		if (line.length() == 0) {
			// TODO: pass-thru to listing?
			return 0;
		}
		if (inProg && line.charAt(0) == 'C') {
			return 0;
		}
		// TODO: some of these must be before any FORTRAN cards...
		if (!inProg) {
			if (line.startsWith(" TITLE")) {
				prog = line.substring(6).trim();
				return 0;
			}
			if (!inProg && line.startsWith("*JOBID")) {
				processJOBID(line);
				return 0;
			}
			if (line.startsWith("DATA")) {
				processDATA(line);
				return 0;
			}
		}
		// Must be a FORTRAN non-comment card...
		if (line.length() < 7) {
			// TODO: error?
			return -1;
		}
		String lab = line.substring(0, 5).trim();
		String stmt = squeeze(line, 6);

		int labl = -1;
		if (lab.length() > 0) {
			labl = Integer.valueOf(lab);
		}
		FortranItem itm = null;
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
		return y; // points to comma...
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
		int type = implicits[id.charAt(0) - 'A'];
		return parseVariable(id, type);
	}

	public FortranOperand parseVariable(String id, int type) {
		String dims = null;
		int p = intPrec;
		if (type == FortranOperand.ADDRESS) {
			p = adrMode;
		}
		int x = id.indexOf('(');
		if (x >= 0) {
			int y = id.indexOf(')', x); // should be last char...
			if (y < 0) {
				errsAdd("Malformed array reference");
				//return null; // will this break things?
			}
			dims = id.substring(x + 1, y);
			id = id.substring(0, x);
		}
		String sym = id;
		if (inSubr) {
			sym = uniqueName();
			id = curSubr.name() + "." + id;
		}
		FortranOperand fo = null;
		if (symTab.containsKey(id)) {
			fo = symTab.get(id);
			if (dims == null) {
				return fo;
			}
			p = fo.precision();
			type = fo.type();
		}
		if (fo == null && dims != null) {
			errsAdd("Undefined array");
			//return null; // will this break things?
		}
		// only INTEGER has variable precision, at this level
		if (dims == null) {
			fo = new FortranVariable(sym, type, p);
			addSym(id, fo);
		} else {
			FortranArrayRef fa = parseArrayRef((FortranArray)fo, dims);
			fa.setTemp(this, 0);
			fo = fa;
		}
		return fo;
	}

	// For "early" parsing of function/subroutine parameters... before inSubr
	// These variables are not defined in the normal sequence, as they
	// must be contiguous.
	public FortranParameter parseParameter(String id, FortranOperand scope) {
		String sym = uniqueName();
		// TODO: can parameter types be overridden?
		int type = implicits[id.charAt(0) - 'A'];
		id = scope.name() + "." + id;
		if (symTab.containsKey(id)) {
			// should not happen
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
		if (type < 0) {
			type = implicits[id.charAt(0) - 'A'];
		}
		FortranSubprogram fs;
		if (symTab.containsKey(id)) {
			FortranOperand fo = symTab.get(id);
			if (fo.kind() != FortranOperand.FUNCTION ||
					fo.type() != type ||
					((FortranSubprogram)fo).numArgs() != argc) {
				errsAdd("Subprogram name conflict");
				return null;
			}
			return (FortranSubprogram)fo;
		}
		// TODO: track dummy argument types...
		// complicated by possible subsequent (re)defines...
		fs = new FortranSubprogram(id, type, argc, this);
		addSym(id, fs);
		return fs;
	}

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
		int p = 1;
		String ex = String.format("%d*(%s", ary.sizeof(), ds[0]);
		for (int x = 1; x < ds.length; ++x) {
			ex += String.format("+%d*(%s", ary.getDim(x - 1), ds[x]);
			++p;
		}
		while (p > 0) {
			ex += ')';
			--p;
		}
		FortranExpr xpr = new FortranExpr(ex, this);
		return new FortranArrayRef(ary, xpr);
	}

	public FortranFuncCall parseFuncCall(FortranSubprogram fnc, String nm, String args) {
		// could include parenthesised expressions... including function calls...
		// this is essentially a FortranExpr... for each arg...
		// that recursion is handled in FortranFuncCall...
		return new FortranFuncCall(fnc, nm, args, this);
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

	public FortranOperand getIntTemp(int id) {
		return parseVariable(String.format("$ITMP%d", id), FortranOperand.INTEGER);
	}

	public FortranOperand getLogTemp(int id) {
		return parseVariable(String.format("$LTMP%d", id), FortranOperand.LOGICAL);
	}

	public FortranOperand getRealTemp(int id) {
		return parseVariable(String.format("$RTMP%d", id), FortranOperand.REAL);
	}

	public FortranOperand getCplxTemp(int id) {
		return parseVariable(String.format("$XTMP%d", id), FortranOperand.COMPLEX);
	}

	public FortranOperand getAdrTemp(int id) {
		return parseVariable(String.format("$ATMP%d", id), FortranOperand.ADDRESS);
	}

	public void setFuncDefs(String fnc, String[] args) {
	}

	public void setFuncSubr(String name, String[] args) {
	}

	public void setFuncRet(String fnc, String res) {
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
