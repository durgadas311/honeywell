// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class ProgramStatement extends FortranItem {
	static final String _PAT = "PROGRAM.*";
	private String errors = "";
	String fmtStr;

	public ProgramStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 7; // skip PROGRAM
		pars.setName(stmt.substring(x));
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new ProgramStatement(pot, pars);
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
