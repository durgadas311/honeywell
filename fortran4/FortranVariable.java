// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranVariable extends FortranOperand {
	private String name;

	public FortranVariable(String name, int type, int prec) {
		super(type, prec);
		// This is the internal, unique, EasyCoder, name
		this.name = name;
	}

	public int kind() { return VARIABLE; }
	public String name() { return name; }

	public void genDefs(PrintStream out, FortranParser pars) {
		String val;
		switch (type) {
		case INTEGER:
			val = String.format("#%dB0", prec);
			break;
		case LOGICAL:
			val = "#1B0";
			break;
		case REAL:
			// TODO: word marks
			val = String.format("#%dB0", prec + 3);
			break;
		case COMPLEX:
			// TODO: word marks
			val = String.format("#%dB0", 2 * (prec + 3));
			break;
		}
		pars.emit(String.format("  %-7sDCW   %s", name, val));
	}
}
