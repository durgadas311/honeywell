// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranParameter extends FortranVariable {
	public FortranParameter(String name, int type) {
		super(name, type);
	}

	@Override
	public int kind() { return PARAMETER; }
	@Override
	public String name() { return '(' + name + ')'; }

	@Override
	public void genDefs(PrintStream out, FortranParser pars) {
	}

	// Specific to FortranParameter
	public String ref() { return name; }
}
