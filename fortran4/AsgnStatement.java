// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class AsgnStatement extends FortranItem {
	static final String _PAT = "ASSIGN[0-9]+TO[A-Z][A-Z0-9]*";
	private String errors = "";
	FortranOperand var;
	int targ;

	public AsgnStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 6; // skip ASSIGN
		int y = stmt.indexOf('T', x);
		targ = Integer.valueOf(stmt.substring(x, y));
		x = y + 2; // skip TO
		var = pars.parseVariable(stmt.substring(x));
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new AsgnStatement(pot, pars);
	}

	public void genDefs(PrintStream out, FortranParser pars) {
		// Variables already done...
	}

	public void genCode(PrintStream out, FortranParser pars) {
		pars.emit(String.format("         LCA   $I%05d,%s", targ, var.name()));
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
