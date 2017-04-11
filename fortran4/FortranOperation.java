// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranOperation extends FortranOperand {
	static final int PWR = 1;
	static final int MULT = 2;
	static final int DIV = 3;
	static final int ADD = 4;
	static final int SUB = 5;
	static final int LE = 6;
	static final int LT = 7;
	static final int GT = 8;
	static final int GE = 9;
	static final int EQ = 10;
	static final int NE = 11;
	static final int AND = 12;
	static final int OR = 13;
	static final int NOT = 14;

	private FortranOperand left;
	private FortranOperand right;

	public FortranOperation(int op) {
		super(0, 0);
		left = null;
		right = null;
	}

	public int kind() { return OPERATOR; }
	public String name() { return null; } // TODO: what is 'name' for us?

	public void genDefs(PrintStream out, FortranParser pars) {
		// TODO: might reference an external function
		if (left != null) {
			left.genDefs(out, pars);
		}
		if (right != null) {
			right.genDefs(out, pars);
		}
	}

	// Only for FortranOperation:
	public FortranOperand getLeft() { return left; }
	public FortranOperand getRight() { return right; }
	public void setLeft(FortranOperand opd) { left = opd; }
	public void setRight(FortranOperand opd) { right = opd; }

	public void genCode(PrintStream out, FortranParser pars) {
		// TODO: how does this work... or is it done externally?
		if (left != null) {
			left.genCode(out, pars);
		}
		if (right != null) {
			right.genCode(out, pars);
		}
	}
}
