// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class SubrStatement extends FortranItem {
	static final String _PAT = "SUBROUTINE[A-Z][A-Z0-9]*\\([A-Z0-9,]*\\)";
	static final String _APAT = "SUBROUTINE[A-Z][A-Z0-9]*";
	private String errors = "";
	FortranSubprogram subr;
	FortranParameter[] args = null;

	public SubrStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 10; // skip SUBROUTINE
		int y = stmt.indexOf('(', x);
		if (y < 0) {
			y = n;
		}
		subr = pars.parseSubprogram(stmt.substring(x, y), FortranOperand.VOID);
		if (subr == null || subr.type() != FortranOperand.VOID) {
			pars.errsAdd("Subroutine name not unique");
		}
		if (y >= n) {
			return;
		}
		x = y + 1;
		y = stmt.indexOf(')', x); // should be last char
		if (y > x) {
			// do not "register" these, they are private, dummy, vars...
			// they must be contiguous for EXM...
			String[] av = stmt.substring(x, y).split(",");
			args = new FortranParameter[av.length];
			for (x = 0; x < av.length; ++x) {
				args[x] = pars.parseParameter(av[x], subr);
			}
		}
	}

	public static SubrStatement parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT) && !pot.matches(_APAT)) {
			return null;
		}
		return new SubrStatement(pot, pars);
	}

	public void genDefs(FortranParser pars) {
		if (args == null) {
			return;
		}
		for (int x = 0; x < args.length; ++x) {
			pars.emit(String.format("   %-6sDSA   0", args[x].ref()));
		}
	}

	public void genCode(FortranParser pars) {
		int ret = pars.addrMode() * 2 + 3;
		int retscr = ret + pars.addrMode();
		pars.emit(String.format("  %-7sRESV  0", subr.name()));
		pars.emit(String.format("         SCR   %s+%d,70", subr.name(), retscr));
		pars.emit(String.format("         B     %s+%d", subr.name(), retscr + 1));
		// restore index registers... changes 'return' offset for SCR
		pars.emit("         B     0"); // return to caller
		// start subroutine code...
		if (args != null) {
			pars.emit(String.format("         EXM   (%s+%d),%s,57",
					subr.name(), ret + 1,
					args[0].ref()));
			pars.emit(String.format("         SCR   %s+%d,67",
					subr.name(), retscr));
		}
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}

	// Specific to SubrStatement
	public FortranOperand getSubr() { return subr; }
}
