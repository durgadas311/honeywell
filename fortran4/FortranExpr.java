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
		this.expr = parse(null);
System.err.format("EXPR %s:\n", exprStr);
dump(this.expr, 0);
//System.exit(0);
	}

	public int type() { return expr.type(); }
	public String getResult() {
		return expr.name();
	}

	public void genDefs(PrintStream out, FortranParser pars) {
		// Variables and temps already defined...
		// anything else?
	}

	public void genCode(PrintStream out, FortranParser pars) {
		if (expr instanceof FortranOperation) {
			((FortranOperation)expr).genCode(out, pars);
		} else {
			// no code?
		}
	}

	private FortranOperand parse(FortranOperation parent) {
		int y = idx;
		FortranOperand fo = null;
		FortranOperation op = null;
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
			// TODO: charAt(idx) == '(' means array or func...
			fo = pars.parseVariable(exprStr.substring(y, idx));
		} else if (c == '(') {
			// TODO: could be COMPLEX, also...
			++idx;
			++level;
			fo = parse(parent); // pass parent or null?
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
		}
		if (idx < len && exprStr.charAt(idx) != ')') {
			// TODO: error if level != 0
			op = FortranOperation.get(exprStr, idx);
			if (op == null) {
				// This is where most errors end up,
				// regardless of whether this was intended
				// as an operator...
				pars.errsAdd("Invalid operator");
				return null;
			}
			idx += op.parseLen();
			// there must be more...
			op.setLeft(fo);
			fo = parse(op);
			// might need to re-balance based on precedence...
			if (fo instanceof FortranOperation) {
				FortranOperation ofo = (FortranOperation)fo;
				if (ofo.preced() > op.preced()) {
					op.setRight(ofo.getLeft());
					ofo.setLeft(op);
					op.setTemp(pars, level + 1);
					ofo.setTemp(pars, level);
					op = ofo;
					return op;
				}
			}
			op.setRight(fo);
			op.setTemp(pars, level);
			return op;
		}
		// Only a single operand, done
		return fo;
	}

	private FortranOperand parseNum() {
		boolean decimal = false;
		boolean exp = false;
		boolean sign = true;
		int y = idx;
		char c;
		for (; idx < len; ++idx) {
			c = exprStr.charAt(idx);
			if (sign && (c == '-' || c == '+')) {
				sign = false;
				continue;
			}
			sign = false;
			if (Character.isDigit(c)) continue;
			if (!exp && !decimal && c == '.') {
				// must ensure this is not ".EQ."...
				if (FortranOperation.relCheck(exprStr, idx)) {
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
		// TODO: make sure we got some digits?
		String num = exprStr.substring(y, idx);
		try {
			if (exp || decimal) {
				double d = Double.valueOf(num);
				return FortranConstant.get(pars, d);
			} else {
				int i = Integer.valueOf(num);
				return FortranConstant.get(pars, i);
			}
		} catch (Exception ee) {
		}
		pars.errsAdd(String.format("Invalid number \"%s\"", num));
		return null;
	}

	static private void dump(FortranOperand op, int level) {
		if (op == null) {
			System.err.format("[%d] null\n", level);
			return;
		}
		if (op instanceof FortranOperation) {
			FortranOperation fo = (FortranOperation)op;
			System.err.format("[%d] OP %d: %s %d %d\n",
				level, fo.oper(), fo.name(), fo.type(), fo.kind());
			dump(fo.getLeft(), level + 1);
			dump(fo.getRight(), level + 1);
			return;
		}
		System.err.format("[%d] %s: %d %d %d\n",
			level, op.name(), op.type(), op.kind(), op.precision());
	}
}
