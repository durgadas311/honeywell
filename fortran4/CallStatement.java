// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class CallStatement extends FortranItem {
	static final String _PAT = "CALL[A-Z][A-Z0-9]*\\([A-Z0-9,]*\\)";
	static final String _APAT = "CALL[A-Z][A-Z0-9]*";
	private String errors = "";
	FortranOperand subr;
	FortranOperand[] args = null;

	public CallStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 4; // skip CALL
		int y = stmt.indexOf('(', x);
		if (y < 0) {
			y = n;
		}
		subr = pars.parseSubprogram(stmt.substring(x, y), FortranOperand.VOID);
		if (subr.type != FortranOperand.VOID) {
			pars.errsAdd("Subroutine name not unique");
		}
		if (y >= n) {
			return;
		}
		x = y + 1;
		y = stmt.indexOf(')', x); // should be last char
		if (y > x) {
			String[] av = stmt.substring(x, y).split(",");
			args = new FortranOperand[av.length];
			for (x = 0; x < av.length; ++x) {
				args[x] = pars.parseOperand(av[x]);
			}
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT) && !pot.matches(_APAT)) {
			return null;
		}
		return new CallStatement(pot, pars);
	}

	public void genDefs(FortranParser pars) {
	}

	public void genCode(FortranParser pars) {
		// TODO: confirm number of calling parameters...
		pars.emit(String.format("         B     %s", subr.name()));
		if (args == null) {
			return;
		}
		int x = 0;
		for (; x < args.length - 1; ++x) {
			pars.emit(String.format("         DSA   %s",
							args[x].name()));
		}
		pars.emit(String.format(" R       DSA   %s",
							args[x].name()));
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
