// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.util.Vector;
import java.io.*;

public class CallStatement extends FortranItem {
	static final String _PAT = "CALL[A-Z][A-Z0-9]*\\([A-Z0-9,]*\\)";
	static final String _APAT = "CALL[A-Z][A-Z0-9]*";
	private String errors = "";
	FortranOperand subr;
	FortranExpr[] args = null;

	public CallStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 4; // skip CALL
		int y = stmt.indexOf('(', x);
		if (y < 0) {
			y = n;
		}
		String f = stmt.substring(x, y);
		x = y;
		int na = 0;
		if (x < n) {
			Vector<FortranExpr> vx = new Vector<FortranExpr>();
			do {
				y = pars.matchingComma(stmt, x);
				if (y < 0) {
					y = n;
				}
				vx.add(new FortranExpr(stmt.substring(x, y), pars));
				// TODO: check errors?
				x = y + 1;
			} while (x < n);
			args = vx.toArray(new FortranExpr[0]);
			na = args.length;
		}
		subr = pars.parseSubprogram(f, FortranOperand.VOID, na);
		if (subr.type != FortranOperand.VOID) {
			pars.errsAdd("Subroutine name not unique");
		}
		pars.resetTemps();
		for (x = 0; x < na; ++x) {
			args[x].setTemp(pars, 0);
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
		int x;
		for (x = 0; x < args.length; ++x) {
			args[x].genCode(pars);
		}
		pars.emit(String.format("         B     %s", subr.name()));
		if (args == null) {
			return;
		}
		for (x = 0; x < args.length - 1; ++x) {
			pars.emit(String.format("         DSA   %s",
							args[x].getResult()));
		}
		pars.emit(String.format(" R       DSA   %s",
							args[x].getResult()));
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
