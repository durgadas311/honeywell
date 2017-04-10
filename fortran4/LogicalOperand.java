// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class LogicalOperand implements FortranOperand {
	private String name;
	private boolean v;
	private String value;

	public LogicalOperand(String nm, boolean val) {
		if (nm == null) {
			// constant, not variable
			name = String.format(":%d", val ? 1 : 0);
		} else {
			name = nm;
		}
		value = String.format("  %-7sDCW   #1B%d", name, val ? 1 : 0);
		v = val;
	}

	public int type() {
		return LOGICAL;
	}

	public String name() {
		return name;
	}

	public int precision() {
		return 1;
	}

	public void genDefs(PrintStream out, FortranParser pars) {
		pars.emit(value);
	}
}
