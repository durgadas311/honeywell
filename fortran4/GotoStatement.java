// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class GotoStatement extends FortranItem {
	static final String _PAT = "GOTO[0-9]+";
	private String errors = "";
	int targ;

	public GotoStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 4; // skip GOTO
		targ = Integer.valueOf(stmt.substring(x));
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new GotoStatement(pot, pars);
	}

	public void genDefs(PrintStream out, FortranParser pars) {
		// Anything for us?
	}

	public void genCode(PrintStream out, FortranParser pars) {
		pars.emit(String.format("         B     $%05d", targ));
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
