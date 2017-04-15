// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.regex.Pattern;

public class ReturnStatement extends FortranItem {
	static final String _PAT = "RETURN";
	private String errors = "";
	private FortranOperand subr;

	public ReturnStatement(String stmt, FortranParser pars) {
		if (!pars.inSubroutine()) {
			pars.errsAdd("RETURN outside of subroutine/function");
		}
		subr = pars.currSubr();
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new ReturnStatement(pot, pars);
	}

	public void genDefs(FortranParser pars) {
	}

	public void genCode(FortranParser pars) {
		pars.emit(String.format("         B     %s+9", subr.name()));
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
