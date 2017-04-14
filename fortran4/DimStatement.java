// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.regex.Pattern;

public class DimStatement extends FortranItem {
	static final String _PAT = "DIMENSION[A-Z0-9,()]*";
	private String errors = "";

	public DimStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		String[] terms = stmt.substring(9).split("\\),*");
		for (String t : terms) {
			if (!t.matches("[A-Z][A-Z0-9]*\\([0-9,]+")) {
				pars.errsAdd("Invalid array def " + t + ')');
				continue;
			}
			int x = t.indexOf('(');
			int y = t.length(); // assumed last char, split off.
			String[] ds = t.substring(x + 1, y).split(",");
			int[] dims = new int[ds.length];
			for (int i = 0; i < ds.length; ++i) {
				dims[i] = Integer.valueOf(ds[i]);
			}
			pars.parseArray(t.substring(0, x), -1, dims);
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (pot.matches(_PAT)) {
			return new DimStatement(pot, pars);
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
