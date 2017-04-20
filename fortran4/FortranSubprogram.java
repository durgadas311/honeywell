// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranSubprogram extends FortranVariable {
	protected int nargs;
	protected FortranVariable ret = null;

	public FortranSubprogram(String name, int type, int argc, FortranParser pars) {
		super(name, type);
		nargs = argc;
		if (type >= 0 && type != VOID && pars != null) {
			setRetVal(pars);
		}
	}

	@Override
	public int kind() { return FUNCTION; }

	@Override
	public void genDefs(FortranParser pars) { }

	public void setType(int type, FortranParser pars) {
		if (this.type == type && ret != null) {
			return;
		}
		this.type = type;
		setRetVal(pars);
	}
	public void setRetVal(FortranParser pars) {
		if (type == VOID || name == null) {
			return;
		}
		// Create return value
		// TODO: how to cleanup if old one existed.
		// at least, could re-use uniqueName.
		String sym = pars.uniqueName();
		ret = new FortranVariable(sym, type, pars.intPrecision());
		pars.addSym(name + ".$RET", ret);
	}

	public String getResult() { return ret.name(); }
	public int numArgs() { return nargs; }
}
