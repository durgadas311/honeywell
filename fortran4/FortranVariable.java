// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranVariable extends FortranConstant {

	public FortranVariable(String name, int type, int prec) {
		super(type, prec);
		// This is the internal, unique, EasyCoder, name
		this.name = name;
	}

	public FortranVariable(String name, int type) {
		super(type, 0);
		this.name = name;
	}

	@Override
	public int kind() { return VARIABLE; }

	private void setValue(int val) { vi = val; }
	private void setValue(boolean val) { vl = val; }
	private void setValue(double val) { vr = val; }
	private void setValue(double val, double img) { vr = val; vx = img; }
	public void setValue(String val) {
		// Parse 'val' according to type...
		switch (type) {
		case INTEGER:
			setValue(Integer.valueOf(val));
			break;
		case LOGICAL:
			// TODO: be more selective?
			setValue(val.equals(".TRUE."));
			break;
		case REAL:
			setValue(Double.valueOf(val));
			break;
		case COMPLEX:
			// TODO: parse "(%f,%f)"...
			//setValue(Double.valueOf(val));
			break;
		// No ADDRESS possible? VOID is no-op
		}
	}

	public void genDefs(FortranParser pars) {
		switch (type) {
		case ADDRESS:
			// TODO: is this used? does it need a value?
			pars.emit(String.format("  %-7sDSA   0", name));
			break;
		default:
			super.genDefs(pars);
			break;
		}
	}

	public void genCode(FortranParser pars) {}
}
