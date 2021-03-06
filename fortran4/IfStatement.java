// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.regex.Pattern;

public class IfStatement extends FortranItem {
	static final String _PAT = "IF\\(.*"; // may have nested parens
	private String errors = "";
	private FortranExpr expr;
	int[] arith = null;
	FortranItem stmt = null;
	FortranOperand zero;

	public IfStatement(String stmt, FortranParser pars) {
		int n = stmt.length();
		int x = 2; // skip IF
		int y = pars.matchingParen(stmt, x);
		// Something must follow expression...
		if (y < 0 || y >= n) {
			pars.errsAdd("Malformed IF expression");
		}
		expr = pars.parseExpr(stmt.substring(x, y)); // strip parens?
		if (expr.error()) {
			return;
		}
		x = y; // 'y' already points past end paren
		String rest = stmt.substring(x);
		if (rest.matches("[0-9]+,[0-9]+,[0-9]+")) {
			// must be arith IF...
			String[] nn = rest.split(",");
			arith = new int[3];
			arith[0] = Integer.valueOf(nn[0]);
			arith[1] = Integer.valueOf(nn[1]);
			arith[2] = Integer.valueOf(nn[2]);
			// expr must be REAL or INTEGER
			if (expr != null && expr.type() != FortranOperand.REAL &&
					expr.type() != FortranOperand.INTEGER) {
				pars.errsAdd("Arith IF expression must be numeric");
			}
			if (expr.type() == FortranOperand.REAL) {
				zero = FortranConstant.get(pars, 0.0);
			} else {
				zero = FortranConstant.get(pars, 0);
			}
		} else {
			// TODO: restricted statements, must skip...
			this.stmt = pars.recurse(rest);
			// expr must be LOGICAL
			if (expr != null && expr.type() != FortranOperand.LOGICAL) {
				pars.errsAdd("Logical IF expression must be LOGICAL");
			}
			if (this.stmt == null) {
				pars.errsAdd("Invalid IF action statement");
			}
		}
		pars.resetTemps();
		expr.setTemp(pars, 0);
	}

	public static FortranItem parse(String pot, FortranParser pars) {
		if (!pot.matches(_PAT)) {
			return null;
		}
		return new IfStatement(pot, pars);
	}

	public void genDefs(FortranParser pars) {
		// Anything for us?
		if (stmt != null) {
			stmt.genDefs(pars);
		}
	}

	public void genCode(FortranParser pars) {
		pars.setExpr(expr);
		if (stmt != null) {
			// FortranOperand.LOGICAL:
			pars.emit(String.format("         BCE   /%05d,%s,00",
						src, expr.getResult()));
			stmt.genCode(pars);
			pars.emit(String.format("  /%05d RESV  0", src));
		} else if (arith != null) {
			switch (expr.type()) {
			case FortranOperand.INTEGER:
			case FortranOperand.LOGICAL: // should not happen
			case FortranOperand.ADDRESS: // should not happen
				pars.emit(String.format("         C     %s,%s",
						zero.name(), expr.getResult()));
				pars.emit(String.format("         BCT   $%05d,41",
								arith[0]));
				pars.emit(String.format("         BCT   $%05d,42",
								arith[1]));
				pars.emit(String.format("         B     $%05d",
								arith[2]));
				break;
			case FortranOperand.REAL:
				pars.emit(String.format("         FMA   %s,00,02",
								expr.getResult()));
				pars.emit(String.format("         FMA   $%05d,02,04",
								arith[0])); // X<0
				pars.emit(String.format("         FMA   $%05d,01,04",
								arith[1])); // X=0
				pars.emit(String.format("         B     $%05d",
								arith[2])); // X>0
				break;
			case FortranOperand.COMPLEX:
				// TODO: implement this?
				break;
			}
		} else {
			// error!
			pars.emit("         H     *");
		}
	}

	public boolean error() {
		return errors.length() > 0;
	}

	public String errorMessages() {
		return errors;
	}
}
