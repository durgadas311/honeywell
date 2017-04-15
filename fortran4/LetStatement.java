// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.regex.Pattern;

public class LetStatement extends FortranItem {
	static final String _PAT = "[A-Z].*=.*"; // TODO: arrays
	private String errors = "";
	private FortranExpr expr;
	int[] arith = null;
	FortranItem stmt = null;
	FortranOperand var;

	public LetStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 0;
		int y = stmt.indexOf('=', x);
		// This will include array dimensions!
		var = pars.parseVariable(stmt.substring(x, y));
		x = y + 1;
		expr = pars.parseExpr(stmt.substring(x));
		if (expr != null && expr.type() != var.type()) {
			pars.errsAdd("Assigment of mis-matched types");
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new LetStatement(pot, pars);
	}

	public void genDefs(PrintStream out, FortranParser pars) {
		expr.genDefs(out, pars);
	}

	public void genCode(PrintStream out, FortranParser pars) {
		pars.setExpr(expr);
		if (var instanceof FortranArrayRef) {
			((FortranArrayRef)var).genCode(out, pars);
		}
		switch (var.type()) {
		case FortranOperand.INTEGER:
		case FortranOperand.LOGICAL:
			pars.emit(String.format("         BS    %s", var.name()));
			pars.emit(String.format("         BA    %s,%s",
					expr.getResult(), var.name()));
			break;
		default:
			pars.emit(String.format("         LCA   %s,%s",
					expr.getResult(), var.name()));
			break;
		}
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
