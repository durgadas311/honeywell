// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class TapeCtlStatement extends FortranItem {
	static final String _EPAT = "ENDFILE[^=]*";
	static final String _RPAT = "REWIND[^=]*";
	static final String _BPAT = "BACKSPACE[^=]*";
	private String errors = "";
	int dev;
	String func;

	public TapeCtlStatement(String stmt, FortranParser pars, int skip) {
		int n = stmt.length();
		int x = skip;
		// guess at function based on keyword length...
		switch (skip) {
		case 6:
			func = "$REWIND";
			break;
		case 7:
			func = "$ENDFIL";
			break;
		case 9:
			func = "$BKSPAC";
			break;
		}
		// TODO: support all forms...
		try {
			dev = Integer.valueOf(stmt.substring(x));
		} catch (Exception ee) {
			pars.errsAdd("Invalid peripheral");
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (pot.matches(_EPAT)) {
			return new TapeCtlStatement(pot, pars, 7);
		}
		if (pot.matches(_RPAT)) {
			return new TapeCtlStatement(pot, pars, 6);
		}
		if (pot.matches(_BPAT)) {
			return new TapeCtlStatement(pot, pars, 9);
		}
		return null;
	}

	public void genDefs(PrintStream out, FortranParser pars) {
		// Anything for us?
		// TODO: validate existence of statement label...
	}

	public void genCode(PrintStream out, FortranParser pars) {
		int perph = pars.getDev(dev);
		// TODO: what if 'dev' comes from variable?
		pars.emit(String.format("         B     %s", func));
		pars.emit(String.format(" R       DCW   #1C%02o", perph & 077));
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
