// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.Arrays;
import java.util.Vector;
import java.util.Map;
import java.util.HashMap;
import java.util.Stack;

public class Fortran4 implements FortranParser {
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
	int adrMode;
	private Vector<String> errs;
	private Map<String, String> symTab;
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

	public Fortran4(File input) {
		prog = null; // or default to file name?
		inFile = input;
		symTab = new HashMap<String, String>();
		doLoops = new Stack<DoStatement>();
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
		for (Map.Entry<String, String> entry : symTab.entrySet()) {
			String l = String.format("  %6s=%7s", entry.getKey(), entry.getValue());
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

	public int compile(File list) {
		try {
			// Not needed for single-pass compile
			// in = new BufferedReader(new FileReader(inFile));
			if (list != null) {
				lst = new PrintStream(list);
				listing = true;
			}
		} catch (Exception ee) {
			// 'in' should never fail - already validated in ctor.
			ee.printStackTrace();
			return -1;
		}
		int ret = 0;
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

	public int generate(File output, File list) {
		try {
			if (output != null) {
				out = new PrintStream(output);
			}
			if (list != null) {
				// TODO: append? or not used at all?
				lst = new PrintStream(list);
				listing = true;
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
		emit("         ADMODE3");
		emit("         ORG   1340");
		// TODO: get these from FortranRunTime?
		emit("  $EXIT  DC    #1B0");
		emit("  $ACBOIODC    #1B1");
		emit("         DC    #1B2");
		//
		emit("  $TEMPI DCW   #4B0"); // TODO: precision
		emit("  $TEMPR DCW   #10B0"); // TODO: precision
		emit("  $TEMPL DCW   #1B0");
		emit("  $TEMPA DSA   0");
		emit("  $TEMPX DCW   #20B0"); // TODO: precision
		setDefs();
		emit("  $START B     0-1"); // TODO: special trap
		emit(" R       DCW   @FORTRAN@");
		setCode();
		emit("         B     $EXIT");
		emit("         H     *");
		emit("         NOP");
		emit("         END   $START");
		if (errs.size() > 0) {
			ret = -1;
		}
		if (listing) {
			listSymTab();
		}
		try { in.close(); } catch (Exception ee) {}
		try { if (lst != null) lst.close(); } catch (Exception ee) {}
		try { if (out != null) out.close(); } catch (Exception ee) {}
		return ret;
	}

	private void processDATA(String ln) {
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
		int e = next.length();
		if (e > 72) {
			e = 72;
		}
		// TODO: echo 'next' to listing?
		if (line == null) {
			curLine = lineNo;
			line = next.substring(0, e);;
			continue;
		}
		if (next.length() >= 6 && next.charAt(5) != ' ' && next.charAt(5) != '0') {
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
		DoStatement du;
		if ((du = (DoStatement)DoStatement.parse(stmt, this)) != null) {
			itm = du;
			doLoops.push(du);
			doStmts.put(du.getTerm(), du);
		}
		if (itm == null) { itm = StmtFunction.parse(stmt, this); }
		if (itm == null) { itm = GotoStatement.parse(stmt, this); }
		if (itm == null) { itm = AGotoStatement.parse(stmt, this); }
		if (itm == null) { itm = CGotoStatement.parse(stmt, this); }
		if (itm == null) { itm = AsgnStatement.parse(stmt, this); }
		if (itm == null) { itm = IfStatement.parse(stmt, this); }
		if (itm == null) { itm = WriteStatement.parse(stmt, this); }
		if (itm == null) { itm = EndStatement.parse(stmt, this); }
		if (itm == null) { itm = ContStatement.parse(stmt, this); }
		if (itm == null) { itm = FormatStatement.parse(stmt, this); }
		if (itm == null) { itm = ProgramStatement.parse(stmt, this); }
		if (itm == null) {
			System.err.format("Unknown: %s\n", stmt);
			return -1;
		}
		itm.label = labl;
		itm.src = curLine;
		program.add(itm);
		return (itm == null ? -1 : 0);
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
		doLoops.pop();
		// Need to allow duplicates, and enforce order...
		do {
			du.genLoop(out, this);
			du = du.getNext();
		} while (du != null);
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
				emit(String.format("  $%05d RESV  0", itm.label));
			}
			itm.genCode(out, this);
			checkDo(itm);
		}
	}

	private int uniq = 0;
	private String uniqueName() {
		return String.format("/U%05d", uniq++);
	}

	// TODO: convert to using FortranOperand...
	public void setVariable(String var, int val) {
		if (!symTab.containsKey(var)) {
			emit(String.format("  %-7sDCW   #%dB%d", var, addrMode(), val));
			// TODO: generate unique name if needed
			symTab.put(var, var);
		}
	}

	public void setLocalVar(String scope, String var, int val) {
		String sym = scope + '.' + var;
		if (!symTab.containsKey(var)) {
			String u = uniqueName();
			emit(String.format("  %-7sDCW   #%dB%d", u, addrMode(), val));
			symTab.put(var, u);
		}
	}

	public void setConst(int konst) {
		String sym = String.format(":%d", konst);
		setVariable(sym, konst);
	}

	public void setFuncDefs(String fnc, String[] args) {
	}

	public void setFuncSubr(String name, String[] args) {
	}

	public void setFuncRet(String fnc, String res) {
	}

	public void setExpr(FortranExpr expr) {
	}

	public void emit(String ezc) {
		out.format("%05d%s\n", ezcLine++, ezc);
	}

	public FortranExpr parseExpr(String expr) {
		return new FortranExpr(expr);
	}

	public FortranItem recurse(String stmt) {
		return null;
	}

	public int getLine() { return curLine; }
	public String tempAdr() { return "$TEMPA"; }
	public String tempInt() { return "$TEMPI"; }
	public String tempReal() { return "$TEMPR"; }
	public String tempLog() { return "$TEMPL"; }
	public String tempComplex() { return "$TEMPX"; }
	public int addrMode() { return 3; }
	public void setName(String nm) {
		prog = nm;
	}
}
