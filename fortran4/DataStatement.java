// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.util.Vector;
import java.io.*;

public class DataStatement extends FortranItem {
	static final String _PAT = "DATA.*/";
	private String errors = "";
	Vector<FortranOperand> list;
	Vector<FortranOperand> data;
	Vector<Integer> mult;

	public DataStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 4; // skip DATA
		list = new Vector<FortranOperand>();
		data = new Vector<FortranOperand>();
		mult = new Vector<Integer>();
		FortranOperand fo;
		while (x < n) {
			// This does not allow for "/" in array subscripts...
			int y = stmt.indexOf('/', x);
			if (y < 0) {
				// error, must end with '/'
				pars.errsAdd("Malformed DATA statement");
				return;
			}
			while (x < y) {
				int z = pars.matchingComma(stmt, x);
				if (z < 0 || z > y) {
					z = y;
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
				list.add(fo);
				x = z + 1;
			}
			x = y + 1;
			y = stmt.indexOf('/', x);
			if (y < 0) {
				// error, must end with '/'
				pars.errsAdd("Malformed DATA statement");
				return;
			}
			while (x < y) {
				int z = pars.matchingComma(stmt, x);
				if (z < 0 || z > y) {
					z = y;
				}
				String i = stmt.substring(x, y);
				int m = 1;
				if (i.matches("[0-9]+\\*")) {
					int w = i.indexOf('*');
					m = Integer.valueOf(i.substring(0, w));
					i = i.substring(w + 1);
				}
				// TODO: handle multiplier
				data.add(pars.parseConstant(i));
				mult.add(m);
				x = z + 1;
			}
		}
		for (x = 0; x < list.size(); ++x) {
			// since list items are process independently,
			// they could share temp variables.
			fo = list.get(x);
			if (fo instanceof FortranOperation) {
				pars.resetTemps();
				((FortranOperation)fo).setTemp(pars, 0);
			}
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new DataStatement(pot, pars);
	}

	public void genDefs(FortranParser pars) {
		// Anything for us?
		// TODO: validate existence of statement label...
	}

	public void genCode(FortranParser pars) {
		// TODO: 
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
		pars.emit(String.format(" R       DSA   %s", itm.name()));
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
