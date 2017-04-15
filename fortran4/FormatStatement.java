// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FormatStatement extends FortranItem {
	static final String _PAT = "FORMAT\\(.*";
	private String errors = "";
	String fmtStr;

	public FormatStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 6; // skip FORMAT
		int y = x + 1; // start inside first paren...
		int p = 1; // inside 1 paren...
		while (p > 0 && y < n) {
			if (stmt.charAt(y) == '(') {
				++p;
			} else if (stmt.charAt(y) == ')') {
				--p;
			}
			++y;
		}
		// [x,y] include parens...
		if (y < n || p != 0) {
			pars.errsAdd("Malformed FORMAT string");
		}
		fmtStr = stmt.substring(x + 1, y - 1);
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new FormatStatement(pot, pars);
	}

	public void genDefs(FortranParser pars) {
		pars.emit(String.format(" R $%05dDCW   @%s@", label, fmtStr));
		label = -1;
	}

	public void genCode(FortranParser pars) {
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
