// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public abstract class FortranOperand {
	// types...
	static final int VOID = 0;
	static final int INTEGER = 1;
	static final int REAL = 2;
	static final int LOGICAL = 3;
	static final int COMPLEX = 4;
	static final int ADDRESS = 5; // not a FORTRAN type, per se.

	// kinds...
	static final int CONSTANT = 1;
	static final int VARIABLE = 2;
	static final int FUNCTION = 3;
	static final int OPERATOR = 4;
	static final int PARAMETER = 5;
	static final int ARRAY = 6;
	static final int ARRAYREF = 7;
	static final int FUNCTIONCALL = 8;
	static final int IMPLIEDDO = 9;

	protected int type = 0;
	protected int prec = 0; // "0" means not yet known

	protected FortranOperand(int type, int prec) {
		this.type = type;
		this.prec = prec;
	}

	public int type() { return type; }
	public int precision() { return prec; }
	public int sizeof() {
		switch (type) {
		case VOID: return 0;
		case INTEGER: return prec;
		case REAL: return 8;
		case LOGICAL: return 1;
		case COMPLEX: return 16;
		case ADDRESS: return prec;
		default: return 0;
		}
	}
	public String getType() {
		switch (type) {
		case VOID: return "VOID";
		case INTEGER: return "INTEGER";
		case REAL: return "REAL";
		case LOGICAL: return "LOGICAL";
		case COMPLEX: return "COMPLEX";
		case ADDRESS: return "ADDRESS";
		default: return "?";
		}
	}
	abstract int kind();
	abstract String name();
	abstract void genDefs(FortranParser pars);
	abstract void genCode(FortranParser pars); // mostly unused
}
