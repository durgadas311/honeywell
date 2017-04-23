// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranConstant extends FortranOperand {
	protected String name;
	protected int vi = 0;
	protected boolean vl = false;
	protected double vr = 0.0;
	protected double vx = 0.0;

	public FortranConstant(int type, int prec) {
		super(type, prec);
	}
	public static FortranConstant get(int val, int prec) {
		FortranConstant fc = new FortranConstant(INTEGER, prec);
		fc.vi = val;
		return fc;
	}
	public static FortranConstant get(FortranParser pars, int val) {
		String id;
		if (val >= 0) {
			id = String.format(":%d", val);
		} else {
			id = String.format(":N%d", -val);
		}
		FortranOperand fo = pars.getSym(id);
		if (fo != null) {
			return (FortranConstant)fo;
		}
		int p = pars.intPrecision();
		FortranConstant fc = get(val, p);
		if (p > 3) {
			fc.name = pars.uniqueName();
		} else {
			fc.name = id;
		}
		pars.addSym(id, fc);
		return fc;
	}
	public static FortranConstant get(FortranParser pars, boolean val) {
		// TODO: also save true value?
		String id = String.format(":%d", val ? 077 : 0);
		FortranOperand fo = pars.getSym(id);
		if (fo != null) {
			return (FortranConstant)fo;
		}
		FortranConstant fc = new FortranConstant(LOGICAL, 1);
		fc.vl = val;
		fc.name = id;
		pars.addSym(id, fc);
		return fc;
	}
	public static FortranConstant get(FortranParser pars, double val) {
		String id = String.format(":R%g", val);
		FortranOperand fo = pars.getSym(id);
		if (fo != null) {
			return (FortranConstant)fo;
		}
		FortranConstant fc = new FortranConstant(REAL, 0);
		fc.vr = val;
		fc.name = pars.uniqueName();
		pars.addSym(id, fc);
		return fc;
	}
	public static FortranConstant get(FortranParser pars, double[] val) {
		String id = String.format(":R%gI%g", val[0], val[1]);
		FortranOperand fo = pars.getSym(id);
		if (fo != null) {
			return (FortranConstant)fo;
		}
		FortranConstant fc = new FortranConstant(COMPLEX, 0);
		fc.vr = val[0];
		fc.vx = val[1];
		fc.name = pars.uniqueName();
		pars.addSym(id, fc);
		return fc;
	}

	public int kind() { return CONSTANT; }
	public String name() { return name; } // no names for constants...???

	// Used by DATA statement (FortranExpr.computeInt())
	public int getIntVal() { return vi; }

	// Will only be called once, no matter how many references
	public void genDefs(FortranParser pars) {
		switch (type) {
		case INTEGER:
			pars.emit(String.format("  %-7sDCW   #%dB%d", name, prec, vi));
			break;
		default:
		case LOGICAL:
			pars.emit(String.format("  %-7sDCW   #1B%d", name, vl ? 077 : 0));
			break;
		case COMPLEX:
			pars.emit(String.format("         DCW   F%.11E", vx));
			// FALLTHROUGH
		case REAL:
			pars.emit(String.format("  %-7sDCW   F%.11E", name, vr));
			break;
		}
	}

	public void genCode(FortranParser pars) {}
}
