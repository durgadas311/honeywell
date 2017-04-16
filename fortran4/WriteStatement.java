// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class WriteStatement extends FortranItem {
	static final String _PAT = "WRITE\\([^)]+\\)[^=]*";
	private String errors = "";
	int dev;
	int fmt = 0;
	FortranOperand[] list;

	public WriteStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 6; // skip WRITE and l-paren
		int y = stmt.indexOf(')', x);
		String[] v = stmt.substring(x, y).split(",");
		// TODO: support all forms...
		try {
			dev = Integer.valueOf(v[0]);
			if (v.length > 1) {
				fmt = Integer.valueOf(v[1]);
			}
		} catch (Exception ee) {
			pars.errsAdd("Invalid peripheral/format");
		}
		x = y + 1;
		// TODO: must support complex lists...
		// must at least be variable/constant...
		// TODO: handle arrays/functions?
		String [] lst = stmt.substring(x).split(",");
		list = new FortranOperand[lst.length];
		for (x = 0; x < lst.length; ++x) {
			list[x] = pars.parseOperand(lst[x]);
			if (list[x] == null) {
				pars.errsAdd(String.format(
					"Invalid list item \"%s\"", lst[x]));
			}
		}
		// TODO: arbitrary expressions...
		// can't naively split on comma...
		// use pars.matchingComma(), then pars.parseExpr()...
		// Each list item is passed idependently, so no need
		// for "globally" unique temps...
		//	pars.resetTemps();
		//	expr[x].setTemp(pars, 0);
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new WriteStatement(pot, pars);
	}

	public void genDefs(FortranParser pars) {
		// Anything for us?
		// TODO: validate existence of statement label...
	}

	public void genCode(FortranParser pars) {
		int perph = pars.getDev(dev);
		pars.emit("         B     $ACBOIO");
		if (fmt > 0) {
			pars.emit(String.format("         DSA   $%05d", fmt));
		} else {
			pars.emit("         DSA   0");
		}
		pars.emit(String.format(" R       DCW   #1C%02o", perph & 077));
		for (int z = 0; z < list.length; ++z) {
			list[z].genCode(pars);
			pars.emit("         CSM");
			pars.emit(String.format(" R       DSA   %s", list[z].name()));
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
