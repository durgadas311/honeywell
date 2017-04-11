// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class CGotoStatement extends FortranItem {
	static final String _PAT = "GOTO\\([0-9][0-9,]*\\),[A-Z][A-Z0-9]*";
	private String errors = "";
	FortranOperand var;
	int[] targs;
	FortranOperand max;
	FortranOperand one;
	FortranOperand tmp;

	public CGotoStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 5; // skip GOTO and l-paren
		int y = stmt.indexOf(')', x);
		String[] tg = stmt.substring(x, y).split(",");
		targs = new int[tg.length];
		for (int z = 0; z < tg.length; ++z) {
			targs[z] = Integer.valueOf(tg[z]);
		}
		x = y + 2; // skip l-paren, comma
		var = pars.parseVariable(stmt.substring(x));
		// index var is 1..max
		one = pars.parseConstant("1");
		max = pars.parseConstant(Integer.toString(targs.length));
		tmp = pars.getAdrTemp(1);
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new CGotoStatement(pot, pars);
	}

	public void genDefs(PrintStream out, FortranParser pars) {
		// This works since index is 1-based
		pars.emit(String.format("  /T%05dDSA   *", src));
		for (int t : targs) {
			pars.emit(String.format("         DSA   $%05d", t));
		}
	}

	public void genCode(PrintStream out, FortranParser pars) {
		pars.emit(String.format("         C     %s,%s", var.name(), max.name()));
		pars.emit(String.format("         BCT   /%05d,41", src));
		pars.emit(String.format("         C     %s,%s", var.name(), one.name()));
		pars.emit(String.format("         BCT   /%05d,44", src));
		pars.emit(String.format("         LCA   /T%05d,%s", src, tmp.name()));
		for (int x = 0; x < pars.addrMode(); ++x) {
			pars.emit(String.format("         BA    %s,%s",
					var.name(), tmp.name()));
		}
		pars.emit(String.format("         B     (%s-%d)",
					tmp.name(), pars.addrMode() - 1));
		pars.emit(String.format("  /%05d RESV  0"));
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
