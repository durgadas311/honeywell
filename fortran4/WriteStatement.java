// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class WriteStatement extends FortranItem {
	static final String _PAT = "WRITE\\([^)]+\\).*";
	private String errors = "";
	int dev;
	int fmt = 0;
	String[] list;

	public WriteStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 6; // skip WRITE and l-paren
		int y = stmt.indexOf(')', x);
		String[] v = stmt.substring(x, y).split(",");
		// TODO: support all forms...
		dev = Integer.valueOf(v[0]);
		if (v.length > 1) {
			fmt = Integer.valueOf(v[1]);
		}
		x = y + 1;
		// TODO: must support complex lists...
		list = stmt.substring(x).split(",");
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new WriteStatement(pot, pars);
	}

	public void genDefs(PrintStream out, FortranParser pars) {
		// Anything for us?
	}

	public void genCode(PrintStream out, FortranParser pars) {
		String tmp = pars.tempAdr();
		pars.emit("         B     $ACBOIO");
		if (fmt > 0) {
			pars.emit(String.format("         DSA   $%05d", fmt));
		} else {
			pars.emit("         DSA   0");
		}
		pars.emit(String.format(" R       DCW   #1C%02o", dev & 077));
		for (int z = 0; z < list.length; ++z) {
			pars.emit(String.format("         LCA   %s,%s", list[z], tmp));
			pars.emit("         CSM");
		}
		pars.emit("         B     $ACBOIO");
		if (fmt > 0) {
			pars.emit(String.format("         DSA   $%05d", fmt));
		} else {
			pars.emit("         DSA   0");
		}
		pars.emit(" R       DCW   #1C77");
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
