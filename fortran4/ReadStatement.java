// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.util.Vector;
import java.io.*;

public class ReadStatement extends FortranItem {
	static final String _PAT = "READ\\([^)]+\\)[^=].*";
	private String errors = "";
	int dev;
	int fmt = 0;
	int err = 0;
	int end = 0;
	FortranOperand[] list;

	public ReadStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 5; // skip READ and l-paren
		int y = stmt.indexOf(')', x);
		String[] v = stmt.substring(x, y).split(",");
		// TODO: support all forms...
		try {
			dev = Integer.valueOf(v[0]);
			if (v.length > 1) {
				fmt = Integer.valueOf(v[1]);
			}
			for (int z = 2; z < v.length; ++z) {
				if (v[z].startsWith("ERR=")) {
					err = Integer.valueOf(v[z].substring(4));
				}
				if (v[z].startsWith("END=")) {
					end = Integer.valueOf(v[z].substring(4));
				}
			}
		} catch (Exception ee) {
			pars.errsAdd("Invalid peripheral/format");
		}
		x = y + 1; // past r-paren...
		Vector<FortranOperand> lst = new Vector<FortranOperand>();
		FortranOperand fo;
		while (x < n) {
			// parseVariable handles arrays...
			// but implied DO loops require pre-processing
			y = pars.matchingComma(stmt, x);
			if (y < 0) {
				pars.errsAdd("Unrecognizable list");
				return;
			}
			String i = stmt.substring(x, y);
			if (i.charAt(0) == '(') {
				String[] du = pars.parseImpliedDo(i);
				if (du == null) {
					pars.errsAdd("Invalid implied DO");
					return;
				}
				for (String d : du) {
					fo = pars.parseVariable(d);
					if (fo == null) {
						return;
					}
					lst.add(fo);
				}
			} else {
				fo = pars.parseVariable(i);
				if (fo == null) {
					return;
				}
				lst.add(fo);
			}
			x = y + 1;
		}
		list = lst.toArray(new FortranOperand[0]);
		for (x = 0; x < list.length; ++x) {
			if (list[x] == null) {
				pars.errsAdd("Invalid list item");
				return;
			}
			// since list items are process independently,
			// they could share temp variables.
			if (list[x] instanceof FortranOperation) {
				pars.resetTemps();
				((FortranOperation)list[x]).setTemp(pars, 0);
			}
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new ReadStatement(pot, pars);
	}

	public void genDefs(FortranParser pars) {
		// Anything for us?
		// TODO: validate existence of statement label...
	}

	public void genCode(FortranParser pars) {
		int perph = pars.getDev(dev) | 040;
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
		if (err > 0) {
			pars.emit(String.format("         BBE   $%05d,$IOFLG,04", err));
		}
		if (end > 0) {
			pars.emit(String.format("         BBE   $%05d,$IOFLG,03", end));
		}
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
