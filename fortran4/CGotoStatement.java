// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class CGotoStatement extends FortranItem {
	static final String _PAT = "GOTO\\([0-9][0-9,]*\\),[A-Z][A-Z0-9]*";
	private String errors = "";
	String var;
	int[] targs;
	int max;

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
		var = stmt.substring(x);
		// index var is 1..max
		max = targs.length;
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new CGotoStatement(pot, pars);
	}

	public void genDefs(PrintStream out, FortranParser pars) {
		pars.setConst(1);
		pars.setConst(max);
		// This works since index is 1-based
		pars.emit(String.format("  /T%05dDSA   *", src));
		for (int t : targs) {
			pars.emit(String.format("         DSA   $%05d", t));
		}
	}

	public void genCode(PrintStream out, FortranParser pars) {
		String tmp = pars.tempAdr();
		pars.emit(String.format("         C     %s,:%d", var, max));
		pars.emit(String.format("         BCT   /%05d,41", src));
		pars.emit(String.format("         C     %s,:1", var));
		pars.emit(String.format("         BCT   /%05d,44", src));
		pars.emit(String.format("         LCA   /T%05d,%s", src, tmp));
		pars.emit(String.format("         BA    %s,%s", var, tmp));
		pars.emit(String.format("         BA    %s,%s", var, tmp));
		pars.emit(String.format("         BA    %s,%s", var, tmp));
		if (pars.addrMode() > 3) {
			pars.emit(String.format("         BA    %s,%s", var, tmp));
			pars.emit(String.format("         B     (%s-3)", tmp));
		} else {
			pars.emit(String.format("         B     (%s-2)", tmp));
		}
		pars.emit(String.format("  /%05d RESV  0"));
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
