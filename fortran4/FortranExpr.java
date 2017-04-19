// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranExpr {
	private String exprStr;
	private int idx = 0;
	private int len = 0;
	private int level = 0;
	private FortranParser pars;
	private FortranOperand expr;

	public FortranExpr(String expr, FortranParser pars) {
		this.pars = pars;
		exprStr = expr;
		len = expr.length();
		idx = 0;
		level = 0;
		this.expr = parse(null, 0);
//System.err.format("EXPR %s:\n", exprStr);
//dump(this.expr, 0);
//System.exit(0);
	}

	public boolean error() { return (expr == null); }
	public int type() { return expr.type(); }
	public String getResult() {
		return expr.name();
	}

	public void setTemp(FortranParser pars, int level) {
		if (expr instanceof FortranOperation) {
			((FortranOperation)expr).setTemp(pars, level);
		}
	}

	public void genDefs(FortranParser pars) {
		// Variables and temps already defined...
		// anything else?
		if (expr instanceof FortranOperator || expr instanceof FortranArrayRef) {
			expr.genDefs(pars);
		}
	}

	public void genCode(FortranParser pars) {
		expr.genCode(pars);
		// No code of our own?
	}

	private FortranOperand parse(FortranOperator parent, int level) {
		int y = idx;
		FortranOperand fo = null;
		FortranOperator op = null;
		char c = exprStr.charAt(idx);
		if (Character.isLetter(c)) {
			// must be variable, array, or function...
			while (idx < len) {
				c = exprStr.charAt(idx);
				if (!Character.isLetter(c) && !Character.isDigit(c)) {
					break;
				}
				++idx;
			}
			String f = exprStr.substring(y, idx);
			if (idx < len && exprStr.charAt(idx) == '(') {
				// array or function.  arrays must be pre-defined,
				// so if nothing yet exists in symTab it must be
				// a function (?)
				int x = pars.matchingParen(exprStr, idx);
				if (x < 0) {
					pars.errsAdd("Unmatched parenthesis");
					return null;
				}
				// 'x' points beyond r-paren
				String e = exprStr.substring(idx + 1, x - 1);
				idx = x;
				fo = pars.getSym(f);
				if (fo == null || fo.kind() == FortranOperand.FUNCTION) {
					// 'fo' could be null going in to this...
					fo = pars.parseFuncCall((FortranSubprogram)fo, f, e);
				} else if (fo.kind() == FortranOperand.ARRAY) {
					fo = pars.parseArrayRef((FortranArray)fo, e);
				} else {
					pars.errsAdd("Not array or function: " + f);
					return null;
				}
			} else {
				fo = pars.parseVariable(f);
			}
		} else if (c == '(') {
			// TODO: could be COMPLEX, also...
			++idx;
			++level;
			fo = parse(parent, level + 1); // pass parent or null?
			--level;
			// Can we assume/insist we have ')'?
			if (idx < len && exprStr.charAt(idx) == ')') {
				++idx;
			}
		} else {
			// must be constant? need to find end of it...
			// also need to handle things like "3.EQ."
			if (c == '.') {
				// could be TRUE FALSE...
				if (exprStr.startsWith(".TRUE.", idx)) {
					fo = FortranConstant.get(pars, true);
					idx += 6;
					return fo;
				} else if (exprStr.startsWith(".FALSE.", idx)) {
					fo = FortranConstant.get(pars, false);
					idx += 7;
					return fo;
				}
			}
			fo = parseNum();
			if (fo == null && c == '-') {
				// unary minus...
				// TODO: implement something
			}
		}
		if (idx < len && exprStr.charAt(idx) != ')') {
			// TODO: error if level != 0
			if (fo == null && c == '-') {
				op = new FortranOperator(FortranOperator.NEG, level, pars);
			} else {
				op = FortranOperator.get(exprStr, idx, level, pars);
			}
			if (op == null) {
				// This is where most errors end up,
				// regardless of whether this was intended
				// as an operator...
				pars.errsAdd("Invalid operator " + exprStr.substring(idx));
				return null;
			}
			idx += op.parseLen();
			// there must be more...
			op.setLeft(fo);
			fo = parse(op, level);
			op = balance(op, fo);
			return op;
		}
		// Only a single operand, done
		return fo;
	}

	// Default is one.setRight(two)...
	private FortranOperator balance(FortranOperator one, FortranOperand two) {
		// might need to re-balance based on precedence...
		if (two instanceof FortranOperator) {
			FortranOperator ofo = (FortranOperator)two;
			if (ofo.level() == one.level() &&
					ofo.preced() >= one.preced()) {
				two = ofo.getLeft();
				one = balance(one, two);
				ofo.setLeft(one);
				return ofo;
			}
		}
		one.setRight(two);
		return one;
	}

	private FortranOperand parseNum() {
		boolean decimal = false;
		boolean exp = false;
		boolean sign = true;
		boolean mant = false; // digits
		boolean expo = false; // digits
		int y = idx;
		char c;
		for (; idx < len; ++idx) {
			c = exprStr.charAt(idx);
			if (sign && (c == '-' || c == '+')) {
				sign = false;
				continue;
			}
			sign = false;
			if (Character.isDigit(c)) {
				if (!exp) {
					mant = true;
				} else {
					expo = true;
				}
				continue;
			}
			if (!exp && !decimal && c == '.') {
				// must ensure this is not ".EQ."...
				if (FortranOperator.relCheck(exprStr, idx)) {
					break;
				}
				decimal = true;
				continue;
			}
			if (!exp && c == 'E') {
				exp = true;
				sign = true;
				continue;
			}
			break;
		}
		// make sure we got some digits
		if (!mant || (exp && !expo)) {
			idx = y;
			return null; // caller must make another guess
		}
		String num = exprStr.substring(y, idx);
		try {
			if (exp || decimal) {
				double d = Double.valueOf(num);
				return FortranConstant.get(pars, d);
			} else {
				int i = Integer.valueOf(num);
				return FortranConstant.get(pars, i);
			}
		} catch (Exception ee) { }
		pars.errsAdd(String.format("Invalid number \"%s\"", num));
		idx = y;
		return null;
	}

	static private void dump(FortranOperand op, int level) {
		if (op == null) {
			System.err.format("[%d] NULL\n", level);
			return;
		}
		if (op instanceof FortranOperator) {
			FortranOperator fo = (FortranOperator)op;
			System.err.format("[%d] OP %d: %s %d %d\n",
				level, fo.oper(), fo.name(), fo.type(), fo.kind());
			dump(fo.getLeft(), level + 1);
			dump(fo.getRight(), level + 1);
			return;
		}
		System.err.format("[%d] %s: %d %d %d (%s)\n",
			level, op.name(), op.type(), op.kind(), op.precision(),
			op.getClass().getName());
	}
}
