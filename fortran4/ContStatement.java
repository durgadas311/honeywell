// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class ContStatement extends FortranItem {
	static final String _PAT = "CONTINUE";
	private String errors = "";

	public ContStatement(String stmt, FortranParser pars) {
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new ContStatement(pot, pars);
	}

	public void genDefs(FortranParser pars) {
		// Anything for us?
	}

	public void genCode(FortranParser pars) {
		// Anything for us?
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
