// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Map;
import java.util.HashMap;
import java.io.*;

public class FortranArray extends FortranVariable {
	private int size;
	private int[] dims;
	private String ind;
	private int[] xvi;
	private boolean[] xvl;
	private double[] xvr;
	private double[] xvx;
	private boolean init = false;

	public FortranArray(String name, int type, int prec, int[] dims) {
		super(name, type, prec);
		this.dims = dims;
		size = 1;
		for (int x = 0; x < dims.length; ++x) {
			size *= dims[x];
		}
	}

	@Override
	public int kind() { return ARRAY; }

	@Override
	public void genDefs(FortranParser pars) {
		ind = pars.uniqueName();
		// compute address of element (1,1,1,...)
		int i = sizeof();
		int off = i;
		for (int x = dims.length - 2; x >= 0; --x) {
			off = (off * dims[x]) + i;
		}
		off -= i - 1;
		pars.emit(String.format("  %-7sDSA   %s-%d", ind, name, off));
	}

	// For Arrays only:
	public String ref() { return ind; }
	public int numDims() { return dims.length; }
	public int[] getDims() { return dims; }
	public int getDim(int x) { return dims[x]; } // TODO: range check
	private void setValue(int idx, int val) {
		if (xvi == null) {
			init = true;
			xvi = new int[size];
		}
		xvi[idx] = val;
	}
	private void setValue(int idx, boolean val) {
		if (xvl == null) {
			init = true;
			xvl = new boolean[size];
		}
		xvl[idx] = val;
	}
	private void setValue(int idx, double val) {
		if (xvr == null) {
			init = true;
			xvr = new double[size];
		}
		xvr[idx] = val;
	}
	private void setValue(int idx, double val, double img) {
		if (xvx == null) {
			init = true;
			xvr = new double[size];
			xvx = new double[size];
		}
		xvr[idx] = val;
		xvx[idx] = img;
	}
	public void setValue(int adr, String val) {
		// compute address of element (1,1,1,...)
		int i = sizeof();
		int off = i;
		for (int x = dims.length - 2; x >= 0; --x) {
			off = (off * dims[x]) + i;
		}
		off -= i - 1;
		int idx = (adr - off) / i;
		// Parse 'val' according to type...
		switch (type) {
		case INTEGER:
			setValue(idx, Integer.valueOf(val));
			break;
		case LOGICAL:
			// TODO: be more selective?
			setValue(idx, val.equals(".TRUE."));
			break;
		case REAL:
			setValue(idx, Double.valueOf(val));
			break;
		case COMPLEX:
			// TODO: parse "(%f,%f)"...
			//setValue(idx, Double.valueOf(val));
			break;
		// No ADDRESS possible? VOID is no-op
		}
	}
	public void genData(FortranParser pars) {
		if (init) {
			pars.emit(String.format("   %-6sRESV  0", name));
			// TODO: generate (possibly sparse?) initializers
			// Use REP directive?
			String nm = name;
			name = ""; // safe? big kludge!
			int c = 1;
			switch (type) {
			case INTEGER: vi = xvi[0]; break;
			case REAL: vr = xvr[0]; break;
			case LOGICAL: vl = xvl[0]; break;
			case COMPLEX: vr = xvr[0]; vx = xvx[0]; break;
			}
			for (int x = 1; x < size; ++x) {
				boolean same = false;
				switch (type) {
				case INTEGER: same = (vi == xvi[x]); break;
				case REAL: same = (vr == xvr[x]); break;
				case LOGICAL: same = (vl == xvl[x]); break;
				case COMPLEX: same = (vr == xvr[x] && vx == xvx[x]); break;
				}
				if (same) {
					++c;
					continue;
				}
				if (c > 1) {
					// TODO: doesn't work for COMPLEX!
					pars.emit(String.format("         REP   %d", c));
				}
				super.genDefs(pars);
				c = 1;
				switch (type) {
				case INTEGER: vi = xvi[x]; break;
				case REAL: vr = xvr[x]; break;
				case LOGICAL: vl = xvl[x]; break;
				case COMPLEX: vr = xvr[x]; vx = xvx[x]; break;
				}
			}
			if (c > 0) {
				if (c > 1) {
					pars.emit(String.format("         REP   %d", c));
				}
				super.genDefs(pars);
			}
			name = nm; // big kludge
		} else {
			pars.emit(String.format("   %-6sRESV  %d", name, size * sizeof()));
		}
	}

	public int subscriptValue(String[] dims) {
		int ex = 0;
		for (int x = dims.length - 1; x > 0; --x) {
			ex = getDim(x - 1) * (ex + Integer.valueOf(dims[x]));
		}
		ex = sizeof() * (ex + Integer.valueOf(dims[0]));
		return ex;
	}

	public String subscriptExpr(String[] dims) {
		// TODO: if FortranParameter, call reference()...
		// TODO: optimize constants? may not be efficient if
		// end up with constant for every possible subscript value.
		String ex = String.format("%d*(%s", sizeof(), dims[0]);
		int p = 1;
		for (int x = 1; x < dims.length; ++x) {
			ex += String.format("+%d*(%s", getDim(x - 1), dims[x]);
			++p;
		}
		while (p > 0) {
			ex += ')';
			--p;
		}
		return ex;
	}
}
