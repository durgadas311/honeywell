// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.regex.Pattern;

public class DefStatement extends FortranItem {
	static final String _IPAT = "INTEGER[^=]*";
	static final String _RPAT = "REAL[^=]*";
	static final String _LPAT = "LOGICAL[^=]*";
	static final String _XPAT = "COMPLEX[^=]*";
	private String errors = "";

	public DefStatement(String stmt, FortranParser pars, int type) {
		int n = stmt.length();
		int x = 0;
		switch (type) {
		case FortranOperand.INTEGER:
			x += 7; // skip INTEGER
			break;
		case FortranOperand.REAL:
			x += 4; // skip REAL
			break;
		case FortranOperand.LOGICAL:
			x += 7; // skip LOGICAL
			break;
		case FortranOperand.COMPLEX:
			x += 7; // skip COMPLEX
			break;
		}
		int y = x;
		while (y < n) {
			y = stmt.indexOf(',', x);
			if (y < 0) {
				y = n;
			}
			pars.parseVariable(stmt.substring(x, y), type);
			x = y + 1;
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (pot.matches(_IPAT)) {
			return new DefStatement(pot, pars, FortranOperand.INTEGER);
		}
		if (pot.matches(_RPAT)) {
			return new DefStatement(pot, pars, FortranOperand.REAL);
		}
		if (pot.matches(_LPAT)) {
			return new DefStatement(pot, pars, FortranOperand.LOGICAL);
		}
		if (pot.matches(_XPAT)) {
			return new DefStatement(pot, pars, FortranOperand.COMPLEX);
		}
		return null;
	}

	public void genDefs(PrintStream out, FortranParser pars) {
	}

	public void genCode(PrintStream out, FortranParser pars) {
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
