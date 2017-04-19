// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.regex.Pattern;

public class ImplStatement extends FortranItem {
	static final String _PAT = "IMPLICIT[-,\\(\\)A-Z]*";
	private String errors = "";

	public ImplStatement(String stmt, FortranParser pars) {
		int type;
		int n = stmt.length();
		int x = 8;
		while (x < n) {
			if (stmt.startsWith("INTEGER", x)) {
				type = FortranOperand.INTEGER;
				x += 7;
			} else if (stmt.startsWith("REAL", x)) {
				type = FortranOperand.REAL;
				x += 4;
			} else if (stmt.startsWith("DOUBLEPRECISION", x)) {
				type = FortranOperand.REAL;
				x += 15;
			} else if (stmt.startsWith("LOGICAL", x)) {
				type = FortranOperand.LOGICAL;
				x += 7;
			} else if (stmt.startsWith("COMPLEX", x)) {
				type = FortranOperand.COMPLEX;
				x += 7;
			} else {
				// TODO: support "NONE"?
				pars.errsAdd("Invalid implicit type " + stmt.substring(x));
				return;
			}
			if (stmt.charAt(x) != '(') {
				pars.errsAdd("Invalid implicit syntax");
				return;
			}
			int y = pars.matchingParen(stmt, x);
			if (y < 0) {
				pars.errsAdd("Invalid implicit syntax");
				return;
			}
			String[] terms = stmt.substring(x + 1, y - 1).split(",");
			for (String t : terms) {
				if (t.matches("[A-Z]")) {
					pars.setImplicit(t.charAt(0), type);
				} else if (t.matches("[A-Z]-[A-Z]")) {
					char a = t.charAt(0);
					char b = t.charAt(2);
					if (a > b) {
						char tc = a;
						a = b;
						b = tc;
					}
					while (a <= b) {
						pars.setImplicit(a, type);
						++a;
					}
				} else {
					pars.errsAdd(String.format(
						"Malformed IMPLICIT expression \"%s\"", t));
					return;
				}
			}
			x = y + 1; // past r-paren and comma
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (pot.matches(_PAT)) {
			return new ImplStatement(pot, pars);
		}
		return null;
	}

	public void genDefs(FortranParser pars) {
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
