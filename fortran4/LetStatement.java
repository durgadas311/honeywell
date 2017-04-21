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
		// Can't make this check now, if a function is used...
		// Need a 'validate' method to call at end of compile.
//		if (expr != null && expr.type() != var.type()) {
//			pars.errsAdd("Assigment of mis-matched types");
//		}
		pars.resetTemps();
		expr.setTemp(pars, 0);
		if (var instanceof FortranOperation) {
			((FortranOperation)var).setTemp(pars, 0);
		}
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new LetStatement(pot, pars);
	}

	public void genDefs(FortranParser pars) {
		expr.genDefs(pars);
	}

	public void genCode(FortranParser pars) {
		pars.setExpr(expr);
		var.genCode(pars);
		// TODO: work out src+dst types...
		switch (var.type()) {
		case FortranOperand.LOGICAL:
			pars.emit(String.format("         BS    %s", var.name()));
			pars.emit(String.format("         BA    %s,%s",
					expr.getResult(), var.name()));
			break;
		case FortranOperand.INTEGER:
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
