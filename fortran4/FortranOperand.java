// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public interface FortranOperand {
	static final int INTEGER = 1;
	static final int REAL = 2;
	static final int LOGICAL = 3;
	static final int COMPLEX = 4;

	int type();
	String name();
	int precision();
	void genDefs(PrintStream out, FortranParser pars);
}
