// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranSubprogram extends FortranVariable {
	protected int nargs;
	protected FortranOperand ret = null;

	public FortranSubprogram(String name, int type, int argc, FortranParser pars) {
		super(name, type);
		nargs = argc;
		if (type != VOID && pars != null) {
			setRetVal(pars);
		}
	}

	@Override
	public int kind() { return FUNCTION; }

	@Override
	public void genDefs(FortranParser pars) {
	}

	public void setRetVal(FortranParser pars) {
		if (type == VOID || name == null) {
			return;
		}
		// Create return value
		String sym = pars.uniqueName();
		ret = new FortranVariable(sym, type, pars.intPrecision());
		pars.addSym(name + "." + name, ret);
	}

	public String getResult() { return ret.name(); }
	public int numArgs() { return nargs; }
}
