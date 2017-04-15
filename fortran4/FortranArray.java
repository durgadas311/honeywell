// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranArray extends FortranVariable {
	private int[] dims;
	private String ind;

	public FortranArray(String name, int type, int prec, int[] dims) {
		super(name, type, prec);
		this.dims = dims;
	}

	@Override
	public int kind() { return ARRAY; }

	@Override
	public void genDefs(FortranParser pars) {
		ind = pars.uniqueName();
		pars.emit(String.format("  %-7sDSA   %s", ind, name));
	}

	// For Arrays only:
	public String ref() { return ind; }
	public int numDims() { return dims.length; }
	public int[] getDims() { return dims; }
	public int getDim(int x) { return dims[x]; } // TODO: range check
	public void genData(FortranParser pars) {
		int size = sizeof();
		for (int x = 0; x < dims.length; ++x) {
			size *= dims[x];
		}
		pars.emit(String.format("   %-6sRESV  %d", name, size));
	}
}
