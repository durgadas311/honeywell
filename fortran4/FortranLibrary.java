// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class FortranLibrary {
	// System runtime calls - not callable from FORTRAN statements
	static final String name = "FORTRAN";
	static final int _EXIT = 0;
	static final int _ACBOIO = 1;
	static final int _ACBOIO_ = 2;
	static final int _ACBFPH = 3;
	static final int _ACBFXP = 4;
	static final int _ENDFIL = 5;
	static final int _REWIND = 6;
	static final int _BKSPAC = 7;
	// FORTRAN builtin functions
	static final int EOF = 8;
	static final int EOT = 9;
	static final int AINT = 10;
	static final int INT = 11;
	static final int SQRT = 12;
	static final int IAND = 13;
	static final int IOR = 14;
	static final int ICOMPL = 15;
	static final int IEXCLR = 16;
	static final int FLOAT = 17;
	static final int IFIX = 18;
	static final int ABS = 19;
	static final int IABS = 20;
	static final int ATAN = 21;
	static final int ATAN2 = 22;
	static final int COS = 23;
	static final int SIN = 24;
	static final int TANH = 25;
	static final int ALOG = 26;
	static final int ALOG10 = 27;
	static final int AMOD = 28;
	static final int MOD = 29;
	static final int EXP = 30;
}
