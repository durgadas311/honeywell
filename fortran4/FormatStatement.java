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
			return;
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
		String f = fmtStr;
		boolean lbl = false;
		String l = String.format("$%05d", label);
		while (f.length() > 40) {
			pars.emit(String.format("   %-6sDCW   #40A%s",
					lbl ? "" : l, f.substring(0, 40)));
			lbl = true;
			f = f.substring(40);
		}
		pars.emit(String.format(" R %-6sDCW   #%dA%s",
					lbl ? "" : l, f.length(), f));
		label = -1; // ensure we don't label the statement
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
