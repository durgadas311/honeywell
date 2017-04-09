// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class EndStatement extends FortranItem {
	static final String _PAT = "END";
	private String errors = "";

	public EndStatement(String stmt, FortranParser pars) {
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new EndStatement(pot, pars);
	}

	public void genDefs(PrintStream out, FortranParser pars) {
		// Anything for us?
	}

	public void genCode(PrintStream out, FortranParser pars) {
		pars.emit("         H");
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
