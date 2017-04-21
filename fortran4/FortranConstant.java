// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranConstant extends FortranOperand {
	private String name;
	private int vi;
	private boolean vl;
	private double vr;
	private double vx;

	public FortranConstant(int type, int prec) {
		super(type, prec);
	}
		// TODO: also save true value?
	public static FortranOperand get(FortranParser pars, int val) {
		String id = String.format(":%d", val);
		int p = 32 - Integer.numberOfLeadingZeros(val);
		p = (p + 5) / 6;
		if (p <= 0) p = 1;
		FortranOperand fo = pars.getSym(id);
		if (fo != null) {
			return fo;
		}
		FortranConstant fc = new FortranConstant(INTEGER, p);
		fc.vi = val;
		if (p > 3) {
			fc.name = pars.uniqueName();
		} else {
			fc.name = id;
		}
		pars.addSym(id, fc);
		return fc;
	}
	public static FortranOperand get(FortranParser pars, boolean val) {
		String id = String.format(":%d", val ? 1 : 0);
		FortranOperand fo = pars.getSym(id);
		if (fo != null) {
			return fo;
		}
		FortranConstant fc = new FortranConstant(LOGICAL, 1);
		fc.vl = val;
		fc.name = id;
		pars.addSym(id, fc);
		return fc;
	}
	public static FortranOperand get(FortranParser pars, double val) {
		String id = String.format(":R%g", val);
		FortranOperand fo = pars.getSym(id);
		if (fo != null) {
			return fo;
		}
		FortranConstant fc = new FortranConstant(REAL, 0);
		fc.vr = val;
		fc.name = pars.uniqueName();
		pars.addSym(id, fc);
		return fc;
	}
	public static FortranOperand get(FortranParser pars, double[] val) {
		String id = String.format(":R%gI%g", val[0], val[1]);
		FortranOperand fo = pars.getSym(id);
		if (fo != null) {
			return fo;
		}
		FortranConstant fc = new FortranConstant(COMPLEX, 0);
		fc.vr = val[0];
		fc.vx = val[1];
		fc.name = pars.uniqueName();
		pars.addSym(id, fc);
		return fc;
	}

	public int kind() { return VARIABLE; }
	public String name() { return name; } // no names for constants...???

	// Will only be called once, no matter how many references
	public void genDefs(FortranParser pars) {
		switch (type) {
		case INTEGER:
			pars.emit(String.format("  %-7sDCW   #%dB%d", name, prec, vi));
			break;
		default:
		case LOGICAL:
			pars.emit(String.format("  %-7sDCW   #1B%d", name, vl ? 1 : 0));
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
