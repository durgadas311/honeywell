// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.regex.Pattern;

public class ImplStatement extends FortranItem {
	static final String _IPAT = "IMPLICITINTEGER[-,A-Z]*";
	static final String _RPAT = "IMPLICITREAL[-,A-Z]*";
	static final String _LPAT = "IMPLICITLOGICAL[-,A-Z]*";
	static final String _XPAT = "IMPLICITCOMPLEX[-,A-Z]*";
	private String errors = "";

	public ImplStatement(String stmt, FortranParser pars, int type) {
		int n = stmt.length();
		int x = 0;
		switch (type) {
		case FortranOperand.INTEGER:
			x += 15; // skip INTEGER
			break;
		case FortranOperand.REAL:
			x += 12; // skip REAL
			break;
		case FortranOperand.LOGICAL:
			x += 15; // skip LOGICAL
			break;
		case FortranOperand.COMPLEX:
			x += 15; // skip COMPLEX
			break;
		}
		int y = x;
		while (y < n) {
			y = stmt.indexOf(',', x);
			if (y < 0) {
				y = n;
			}
			if (y - x == 1) {
				pars.setImplicit(stmt.charAt(x), type);
			} else if (y - x == 3 && Character.isLetter(stmt.charAt(x)) &&
					Character.isLetter(stmt.charAt(y - 1)) &&
					stmt.charAt(x + 1) == '-') {
				char a = stmt.charAt(x);
				char b = stmt.charAt(y - 1);
				if (a > b) {
					char t = a;
					a = b;
					b = t;
				}
				while (a <= b) {
					pars.setImplicit(a, type);
					++a;
				}
			} else {
				pars.errsAdd(String.format(
					"Malformed IMPLICIT expression \"%s\"",
					stmt.substring(x, y)));
			}
			x = y + 1;
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (pot.matches(_IPAT)) {
			return new ImplStatement(pot, pars, FortranOperand.INTEGER);
		}
		if (pot.matches(_RPAT)) {
			return new ImplStatement(pot, pars, FortranOperand.REAL);
		}
		if (pot.matches(_LPAT)) {
			return new ImplStatement(pot, pars, FortranOperand.LOGICAL);
		}
		if (pot.matches(_XPAT)) {
			return new ImplStatement(pot, pars, FortranOperand.COMPLEX);
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
