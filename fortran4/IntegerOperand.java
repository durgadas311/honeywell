// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class IntegerOperand implements FortranOperand {
	private String name;
	private int v;
	private String value;
	private int prec;

	public IntegerOperand(String nm, int val, int pre) {
		// TODO: handle negative values...
		if (nm == null) {
			// constant, not variable
			name = String.format(":%d", val);
		} else {
			name = nm;
		}
		if (pre <= 0) {
			// TODO: compute min precision
			prec = 3;
		} else {
			prec = pre;
		}
		value = String.format("  %-7sDCW   #%dB%d", name, prec, val);
		v = val;
	}

	public int type() {
		return INTEGER;
	}

	public String name() {
		return name;
	}

	public int precision() {
		return prec;
	}

	public void genDefs(PrintStream out, FortranParser pars) {
		pars.emit(value);
	}
}
