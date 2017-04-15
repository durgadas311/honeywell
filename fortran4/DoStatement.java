// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class DoStatement extends FortranItem {
	static final String _PAT = "DO[0-9]+[A-Z][A-Z0-9]*=[A-Z0-9]+,[A-Z0-9]+.*";
	private String errors = "";
	private int term;	// loop-termination label
	private FortranOperand var;	// loop control variable name
	private FortranOperand start;	// loop control starting value
	private FortranOperand end;	// loop control ending value
	private FortranOperand step;	// loop control step value
	private DoStatement next = null;

	public DoStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 2; // skip "DO"
		int y;
		for (y = x; y < n && Character.isDigit(stmt.charAt(y)); ++y);
		term = Integer.valueOf(stmt.substring(x, y));
		x = y;
		y = stmt.indexOf('=', x);
		var = pars.parseVariable(stmt.substring(x, y));
		x = y + 1;
		y = stmt.indexOf(',', x);
		start = pars.parseOperand(stmt.substring(x, y));
		x = y + 1;
		y = stmt.indexOf(',', x);
		if (y < 0) {
			y = n;
		}
		end = pars.parseOperand(stmt.substring(x, y));
		x = y + 1;
		if (x < n) {
			step = pars.parseOperand(stmt.substring(x));
			if (step == null) {
				// log additional error?
				return;
			}
		} else {
			step = pars.parseConstant("1");
		}
		if (var.type() != FortranOperand.INTEGER ||
				start.type() != FortranOperand.INTEGER ||
				end.type() != FortranOperand.INTEGER ||
				step.type() != FortranOperand.INTEGER) {
			pars.errsAdd("DO parameter is not INTEGER");
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new DoStatement(pot, pars);
	}

	public void genDefs(FortranParser pars) {
		// variables and constants already gen'ed...
	}

	public void genCode(FortranParser pars) {
		pars.emit(String.format("         BS    %s", var.name()));
		pars.emit(String.format("         BA    %s,%s", start.name(), var.name()));
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
	public void setNext(DoStatement du) { next = du; }

	public void genLoop(FortranParser pars) {
		pars.emit(String.format("         BA    %s,%s", step.name(), var.name()));
		pars.emit(String.format("         C     %s,%s", end.name(), var.name()));
		pars.emit(String.format("         BCT   /%05d,43", src));
	}
}
