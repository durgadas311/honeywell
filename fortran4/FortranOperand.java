// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public abstract class FortranOperand {
	static final int INTEGER = 1;
	static final int REAL = 2;
	static final int LOGICAL = 3;
	static final int COMPLEX = 4;

	static final int CONSTANT = 1;
	static final int VARIABLE = 2;
	static final int FUNCTION = 3;
	static final int OPERATOR = 4;

	protected int type = 0;
	protected int prec = 0; // "0" means not yet known

	protected FortranOperand(int type, int prec) {
		this.type = type;
		this.prec = prec;
	}

	public int type() { return type; }
	public int precision() { return prec; }
	abstract int kind();
	abstract String name();
	abstract void genDefs(PrintStream out, FortranParser pars);
}