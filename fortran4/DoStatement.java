// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class DoStatement extends FortranItem {
	static final String _PAT = "DO[0-9]+[A-Z][A-Z0-9]*=[A-Z0-9]+,[A-Z0-9]+.*";
	private String errors = "";
	private int term;	// loop-termination label
	private String var;	// loop control variable name
	private int start;	// loop control starting value
	private int end;	// loop control ending value
	private int step = 1;	// loop control step value
	private DoStatement next = null;

	public DoStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 2; // skip "DO"
		int y;
		for (y = x; y < n && Character.isDigit(stmt.charAt(y)); ++y);
		term = Integer.valueOf(stmt.substring(x, y));
		x = y;
		y = stmt.indexOf('=', x);
		var = stmt.substring(x, y);
		x = y + 1;
		y = stmt.indexOf(',', x);
		start = Integer.valueOf(stmt.substring(x, y));
		x = y + 1;
		y = stmt.indexOf(',', x);
		if (y < 0) {
			y = n;
		}
		end = Integer.valueOf(stmt.substring(x, y));
		x = y + 1;
		if (x < n) {
			step = Integer.valueOf(stmt.substring(x));
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new DoStatement(pot, pars);
	}

	public void genDefs(PrintStream out, FortranParser pars) {
		pars.setVariable(var, 0); 
		pars.setConst(start);
		pars.setConst(end);
		pars.setConst(step);
	}

	public void genCode(PrintStream out, FortranParser pars) {
		pars.emit(String.format("         LCA   :%d,%s", start, var));
		pars.emit(String.format("  /%05d RESV  0", src));
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}

	// Methods specific to DoStatement...
	public int getTerm() { return term; }
	public DoStatement getNext() { return next; }

	public void genLoop(OutputStream out, FortranParser pars) {
		pars.emit(String.format("         BA    :%d,%s", step, var));
		pars.emit(String.format("         C     :%d,%s", end, var));
		pars.emit(String.format("         BCT   /%05d,43", src));
	}
}
