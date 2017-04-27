// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.util.Vector;
import java.io.*;

public class DataStatement extends FortranItem {
	static final String _PAT = "DATA.*/";
	private String errors = "";

	// TODO: don't generate code to generate data, generate data!
	public DataStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 4; // skip DATA
		Vector<String> list = new Vector<String>();
		Vector<String> data = new Vector<String>();
		Vector<String> vs;
		pars.setLive(true); // compiler must reset!
		while (x < n) {
			list.clear();
			data.clear();
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
				String i = stmt.substring(x, z);
				if (i.charAt(0) == '(') {
					// These DO loops are never coded,
					// must not create loop control vars, etc.
					// These are expanded now, added to 'list'
					vs = new DataImpliedDo(i, pars).getTargets();
					if (vs == null) {
						return;
					}
					list.addAll(vs);
				} else {
					list.add(i);
				}
				x = z + 1;
			}
			// Now we have a flattened list of all vars (could be big),
			// each is a string of the variable reference
			// (scalar var or array element reference).
			x = y + 1;
			y = stmt.indexOf('/', x);
			if (y < 0) {
				// error, must end with '/'
				pars.errsAdd("Malformed DATA statement");
				return;
			}
			int idx = 0;
			String fc = "0";
			int m = 0;
			FortranArray fa = null;
			int ix = 0;
			while ((x < y || m > 0) && (idx < list.size() || fa != null)) {
				FortranOperand fo;
				if (fa != null) {
					fo = fa;
				} else {
					fo = pars.parseVariable(list.get(idx++));
					if (fo instanceof FortranArray) {
						fa = (FortranArray)fo;
						ix = 0;
					}
				}
				if (m <= 0) {
					int z = pars.matchingComma(stmt, x);
					if (z < 0 || z > y) {
						z = y;
					}
					fc = stmt.substring(x, z);
					x = z + 1;
					m = 1;
					if (fc.matches("[0-9]+\\*.*")) {
						int w = fc.indexOf('*');
						m = Integer.valueOf(fc.substring(0, w));
						fc = fc.substring(w + 1);
					}
				}
				// TODO: fix this ugliness
				if (fo instanceof FortranArray) {
					fa.setValue(ix++, fc);
					if (ix >= fa.size()) {
						fa = null;
					}
				} else if (fo instanceof FortranArrayRef) {
					((FortranArrayRef)fo).setValue(fc);
				} else {
					((FortranVariable)fo).setValue(fc);
				}
				if (m > 0) {
					--m;
				}
			}
			if (idx < list.size() || x < y) {
				pars.errsAdd("Mismatched DATA parameters");
			}
			x = y + 2;	// skip slash and comma
		}
		// No need to keep anything (?)
		// All variables now have values set, so code-gen phase
		// should use those values to generate Easycoder DCW lines.
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

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
