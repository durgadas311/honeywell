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
		if (pars.inSubroutine()) {
			return new ReturnStatement("RETURN", pars);
		} else {
			return new StopStatement("STOP", pars, 4);
		}
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
