// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public interface FortranOperation {
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

	FortranOperand left();
	FortranOperand right();
	int resultType();
	int operandType();

	void genDefs(PrintStream out, FortranParser pars);
	void genCode(PrintStream out, FortranParser pars);
}
