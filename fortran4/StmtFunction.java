// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class StmtFunction extends FortranItem {
	static final String _PAT = "[A-Z][A-Z0-9]*([^)]+)=.*";
	private String errors = "";
	private String fnc;	// Function name
	private String[] args;
	private FortranExpr expr;

	public StmtFunction(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 0;
		int y;
		y = stmt.indexOf('(', x);
		fnc = stmt.substring(x, y);
		x = y + 1;
		y = stmt.indexOf(')', x);
		args = stmt.substring(x, y).split(",");
		x = y + 1;
		y = stmt.indexOf('=', x); // should be charAt(x) already...
		x = y + 1;
		expr = pars.parseExpr(stmt.substring(x));
		// TODO: must precede executable statements!
		pars.resetTemps();
		expr.setTemp(pars, 0);
	}

	public static FortranItem parse(String pot, FortranParser pars) {
// TODO: Fix this to not conflict with ARRAY(SUB)=expr...
if (true) {
	return null;
}
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new StmtFunction(pot, pars);
	}

	public void genDefs(FortranParser pars) {
		// TODO: variables (args) are dummy/by reference...
		// only need to setup space to copy addresses into...
		// plus need space for result...
		//pars.setFuncDefs(fnc, args);
	}

	// since these statements appear before executable stmts,
	// this code will be before main program code.
	public void genCode(FortranParser pars) {
		//pars.setFuncSubr(fnc, args);
		pars.setExpr(expr); // TODO: specify location for result
		//pars.setFuncRet(fnc, expr.getResult()); // normally done by RETURN?
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
