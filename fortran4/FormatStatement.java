// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FormatStatement extends FortranItem {
	static final String _PAT = "FORMAT\\(.*";
	private String errors = "";
	String fmtStr;

	public FormatStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 6; // skip FORMAT
		int y = pars.matchingParen(stmt, x);
		// [x,y] include parens...
		if (y < n) { // includes y < 0 error case
			pars.errsAdd("Malformed FORMAT string");
		}
		fmtStr = stmt.substring(x + 1, y - 1); // strip parens
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
