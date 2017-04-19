// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.util.Vector;
import java.io.*;

public class WriteStatement extends FortranItem {
	static final String _PAT = "WRITE\\([^)]+\\)[^=].*";
	static final String _PAT2 = "WRITE\\([^)]+\\)";
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
		Vector<FortranOperand> lst = new Vector<FortranOperand>();
		FortranOperand fo;
		while (x < n) {
			// parseVariable handles arrays...
			// but implied DO loops require pre-processing
			y = pars.matchingComma(stmt, x);
			if (y < 0) {
				y = n;
			}
			String i = stmt.substring(x, y);
			if (i.charAt(0) == '(') {
				fo = new ImpliedDoLoop(i, null, pars);
			} else {
				fo = pars.parseVariable(i);
				if (fo == null) {
					return;
				}
			}
			lst.add(fo);
			x = y + 1;
		}
		list = lst.toArray(new FortranOperand[0]);
		for (x = 0; x < list.length; ++x) {
			// since list items are process independently,
			// they could share temp variables.
			if (list[x] instanceof FortranOperation) {
				pars.resetTemps();
				((FortranOperation)list[x]).setTemp(pars, 0);
			}
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT) && !pot.matches(_PAT2)) {
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
			doItem(list[z], pars);
		}
		pars.emit("         B     $ACBOIO");
		if (fmt > 0) {
			pars.emit(String.format("         DSA   $%05d", fmt));
		} else {
			pars.emit("         DSA   0");
		}
		pars.emit(" R       DCW   #1C77");
	}

	private void doItem(FortranOperand itm, FortranParser pars) {
		if (itm instanceof ImpliedDoLoop) {
			doDo((ImpliedDoLoop)itm, pars);
		} else {
			doSimple(itm, pars);
		}
	}

	private void doDo(ImpliedDoLoop idu, FortranParser pars) {
		idu.genCode(pars);
		for (FortranOperand itm : idu.getItems()) {
			doItem(itm, pars);
		}
		idu.genLoop(pars);
	}

	private void doSimple(FortranOperand itm, FortranParser pars) {
		itm.genCode(pars);
		pars.emit("         CSM");
		pars.emit(String.format(" R       DSA   %s", itm.name()));
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
