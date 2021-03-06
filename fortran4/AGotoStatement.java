// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class AGotoStatement extends FortranItem {
	static final String _PAT = "GOTO[A-Z][A-Z0-9]*,\\([0-9][0-9,]*\\)";
	private String errors = "";
	FortranOperand var;
	int[] targs;

	public AGotoStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 4; // skip GOTO
		int y = stmt.indexOf(',', x);
		var = pars.parseVariable(stmt.substring(x, y));
		// TODO: force variable to be ADDRESS?
		if (var.type() != FortranOperand.INTEGER) {
			pars.errsAdd("Assigned GOTO variable not INTEGER");
		}
		x = y + 2; // skip l-paren, too
		y = stmt.indexOf(')', x); // should be end of string...
		String[] tg = stmt.substring(x, y).split(",");
		targs = new int[tg.length];
		for (int z = 0; z < tg.length; ++z) {
			targs[z] = Integer.valueOf(tg[z]);
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new AGotoStatement(pot, pars);
	}

	public void genDefs(FortranParser pars) {
		// TODO: validate existence of statement labels...
		for (int t : targs) {
			pars.emit(String.format("  $I%05dDSA   $%05d", t, t));
		}
	}

	public void genCode(FortranParser pars) {
		pars.emit(String.format("         B     (%s-%d)",
				var.name(), pars.addrMode() - 1));
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
