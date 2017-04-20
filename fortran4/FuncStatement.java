// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FuncStatement extends FortranItem {
	static final String _PAT = "[A-Z]*FUNCTION[A-Z][A-Z0-9]*\\([A-Z0-9,]*\\)";
	private String errors = "";
	FortranSubprogram func;
	FortranParameter retv = null;
	FortranParameter[] args = null;

	public FuncStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x  = 0;
		int type = -1;
		if (stmt.startsWith("INTEGER")) {
			x += 7;
			type = FortranOperand.INTEGER;
		} else if (stmt.startsWith("REAL")) {
			x += 4;
			type = FortranOperand.REAL;
		} else if (stmt.startsWith("DOUBLEPRECISION")) {
			x += 15;
			type = FortranOperand.REAL;
		} else if (stmt.startsWith("LOGICAL")) {
			x += 4;
			type = FortranOperand.LOGICAL;
		} else if (stmt.startsWith("COMPLEX")) {
			x += 4;
			type = FortranOperand.COMPLEX;
		}
		if (!stmt.startsWith("FUNCTION", x)) {
			pars.errsAdd("Malformed FUNCTION statement");
			return;
		}
		x += 8; // skip FUNCTION
		int y = stmt.indexOf('(', x);
		if (y < 0) {
			pars.errsAdd("Malformed FUNCTION statement");
			return;
		}
		String s = stmt.substring(x, y);
		x = y + 1;
		int na = 0;
		String[] av = null;
		y = stmt.indexOf(')', x); // should be last char
		if (y <= x) {
			pars.errsAdd("Malformed FUNCTION Statement");
			return;
		}
		// do not "register" these, they are private, dummy, vars...
		// they must be contiguous for EXM...
		av = stmt.substring(x, y).split(",");
		na = av.length;
		func = pars.parseSubprogram(s, type, na);
		if (func == null || func.type() != type) {
			pars.errsAdd("Function name not unique");
			return;
		}
		retv = pars.parseParameter(s, func, func.type());
		args = new FortranParameter[av.length];
		for (x = 0; x < av.length; ++x) {
			args[x] = pars.parseParameter(av[x], func, -1);
		}
	}

	public static FuncStatement parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new FuncStatement(pot, pars);
	}

	public void genDefs(FortranParser pars) {
		if (args == null) {
			return;
		}
		pars.emit(String.format("   %-6sDSA   0", retv.ref()));
		for (int x = 0; x < args.length; ++x) {
			pars.emit(String.format("   %-6sDSA   0", args[x].ref()));
		}
	}

	public void genCode(FortranParser pars) {
		int ret = pars.addrMode() * 2 + 3;
		int retscr = ret + pars.addrMode();
		pars.emit(String.format("  %-7sRESV  0", func.name()));
		pars.emit(String.format("         SCR   %s+%d,70", func.name(), retscr));
		pars.emit(String.format("         B     %s+%d", func.name(), retscr + 1));
		// restore index registers... changes 'return' offset for SCR
		pars.emit("         B     0"); // return to caller
		// start subroutine code...
		pars.emit(String.format("         EXM   (%s+%d),%s,57",
					func.name(), ret + 1,
					retv.ref()));
		pars.emit(String.format("         SCR   %s+%d,67",
					func.name(), retscr));
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}

	// Specific to FuncStatement
	public FortranOperand getFunc() { return func; }
}
