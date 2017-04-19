// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.regex.Pattern;

public class StopStatement extends FortranItem {
	static final String _PPAT = "PAUSE[0-7]*";
	static final String _SPAT = "STOP[0-7]*";
	private String errors = "";
	private int parm = -1;
	private boolean pause;

	// Also use by END in the case of PROGRAM
	public StopStatement(String stmt, FortranParser pars, int skip) {
		int n = stmt.length();
		pause = stmt.startsWith("PAUSE");
		int x = skip;
		if (x < n) {
			parm = Integer.valueOf(stmt.substring(x), 8);
			if (parm < 0 || parm > 01777777) {
				pars.errsAdd("Invalid halt parameter");
			}
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (pot.matches(_PPAT)) {
			return new StopStatement(pot, pars, 5);
		}
		if (pot.matches(_SPAT)) {
			return new StopStatement(pot, pars, 4);
		}
		return null;
	}

	public void genDefs(FortranParser pars) {
	}

	public void genCode(FortranParser pars) {
		if (!pause) {
			pars.emit("         B     $ENDAA"); // end runtime
		}
		if (parm >= 0) {
			pars.emit(String.format("         H     %d,%d", parm, parm));
		} else if (pause) {
			pars.emit("         H");
		}
		if (!pause) {
			pars.emit("         H     *"); // ensure no passage beyond...
		}
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
