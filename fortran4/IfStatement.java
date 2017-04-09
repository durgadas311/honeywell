// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.regex.Pattern;

public class IfStatement extends FortranItem {
	static final String _PAT = "IF\\(.*"; // may have nested parens
	private String errors = "";
	private FortranExpr expr;
	int[] arith = null;
	FortranItem stmt = null;

	public IfStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 2; // skip IF
		int y = x + 1; // start inside first paren...
		int p = 1; // inside 1 paren...
		while (p > 0 && y < n) {
			if (stmt.charAt(y) == '(') {
				++p;
			} else if (stmt.charAt(y) == ')') {
				--p;
			} else {
			}
			++y;
		}
		expr = pars.parseExpr(stmt.substring(x, y));
		x = y; // 'y' already points past end paren
		String rest = stmt.substring(x);
		if (rest.matches("[0-9]+,[0-9]+,[0-9]+")) {
			// must be arith IF...
			String[] nn = rest.split(",");
			arith = new int[3];
			arith[0] = Integer.valueOf(nn[0]);
			arith[1] = Integer.valueOf(nn[1]);
			arith[2] = Integer.valueOf(nn[2]);
			// expr must be REAL or INTEGER
		} else {
			// TODO: restricted statements, must skip...
			this.stmt = pars.recurse(rest);
			// expr must be LOGICAL
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new IfStatement(pot, pars);
	}

	public void genDefs(PrintStream out, FortranParser pars) {
		// Anything for us?
		if (stmt != null) {
			stmt.genDefs(out, pars);
		}
	}

	public void genCode(PrintStream out, FortranParser pars) {
		pars.setExpr(expr); // TODO: where to put result
		if (stmt != null) {
			// LOGICAL expression, result is Zero-balance for .FALSE.
			pars.emit(String.format("         BCT   #%05d,60", src));
			stmt.genCode(out, pars);
			pars.emit(String.format("  #%05d RESV  0", src));
		} else {
			// Arith expression, result is where???
			// TODO: only INTEGER can compare this way...
			pars.emit(String.format("         C     =0,%s", expr.getResult()));
			pars.emit(String.format("         BCT   $%05d,41", arith[0]));
			pars.emit(String.format("         BCT   $%05d,42", arith[1]));
			pars.emit(String.format("         B     $%05d", arith[2]));
		}
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
