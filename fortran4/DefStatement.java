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
		// Might contain array declarations... can't split on comma
		int y;
		while (x < n) {
			y = scanVar(stmt, x, n);
			String v = stmt.substring(x, y);
			String d = null;
			x = y + 1; // skip comma, or l-paren
			boolean ary = (y < n && stmt.charAt(y) == '(');
			if (ary) {
				y = stmt.indexOf(')', x);
				if (y < 0) {
					y = n;
				}
				d = stmt.substring(x, y);
				x = y + 2; // skip r-paren and comma
			}
			if (!v.matches("[A-Z][A-Z0-9]*")) {
				pars.errsAdd(String.format(
					"Illegal variable name \"%s\"", v));
				continue;
			}
			if (ary) {
				if (!d.matches("[0-9,]+")) {
					pars.errsAdd(String.format(
						"Illegal dimensions \"%s\"", d));
					continue;
				}
				String[] ds = d.split(",");
				int[] dims = new int[ds.length];
				for (int i = 0; i <ds.length; ++i) {
					dims[i] = Integer.valueOf(ds[i]);
				}
				pars.parseArray(v, type, dims);
			} else {
				pars.parseVariable(v, type);
			}
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

	private int scanVar(String s, int b, int n) {
		int x = s.indexOf(',', b);
		if (x < 0) x = n;
		int y = s.indexOf('(', b);
		if (y < 0) y = n;
		if (y < x) {
			return y;
		} else {
			return x;
		}
	}
}
