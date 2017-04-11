// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranConstant extends FortranOperand {
	private String name;
	private int vi;
	private boolean vl;
	private float vr;
	private float vx;

	public FortranConstant(int type, int prec) {
		super(type, prec);
	}
		// TODO: also save true value?
	public static FortranConstant get(FortranParser pars, int val) {
		String id = String.format(":%d", val);
		int p = 32 - Integer.numberOfLeadingZeros(val);
		p = (p + 5) / 6;
		FortranOperand fo = pars.getSym(id);
		if (fo == null) {
			fo = new FortranVariable(INTEGER, p);
			fo.vi = val;
			if (p > 3) {
				fo.name = pars.uniqueName();
			} else {
				fo.name = id;
			}
			pars.addSym(id, fo);
		}
		return fo;
	}
	public static FortranConstant get(FortranParser pars, boolean val) {
		String id = String.format(":%d", val ? 1 : 0);
		FortranOperand fo = pars.getSym(id);
		if (fo == null) {
			fo = new FortranVariable(LOGICAL, 1);
			fo.vl = val;
			fo.name = id;
			pars.addSym(id, fo);
		}
		return fo;
	}
	public static FortranConstant get(FortranParser pars, float val) {
		String id = String.format("R%g", val);
		FortranOperand fo = pars.getSym(id);
		if (fo == null) {
			fo = new FortranVariable(REAL, 7);
			fo.vr = val;
			fo.name = pars.uniqueName();
			pars.addSym(id, fo);
		}
		return fo;
	}
	public static FortranConstant get(FortranParser pars, float[] val) {
		String id = String.format("R%gI%g", vf, vx);
		FortranOperand fo = pars.getSym(id);
		if (fo == null) {
			fo = new FortranVariable(COMPLEX, 7);
			fo.vr = val[0];
			fo.vx = val[1];
			fo.name = pars.uniqueName();
			pars.addSym(id, fo);
		}
		return fo;
	}

	public int kind() { return VARIABLE; }
	public String name() { return name; } // no names for constants...???

	// Will only be called once, no matter how many references
	public void genDefs(PrintStream out, FortranParser pars) {
		switch (type) {
		case INTEGER:
			pars.emit(String.format("  %-7sDCW   #%dB%d", name, prec, vi));
			break;
		default:
		case LOGICAL:
			pars.emit(String.format("  %-7sDCW   #1B%d", name, vl ? 1 : 0));
			break;
		case REAL:
			emitReal(pars, name, vf);
			break;
		case COMPLEX:
			emitReal(pars, name, vf);
			emitReal(pars, "", vx);
			break;
		}
	}

	private void emitReal(FortranParser pars, String n, float f) {
		int d = Float.floatToIntBits(f);
		// TODO TODO TODO TODO TODO TODO TODO
		// TODO: get actual shift/mask values... etc
		byte ms = (byte)((d >> 31) & 1);
		if (ms == 1) {
			ms = '-';
		} else {
			ms = '+';
		}
		int m = (d >> 8) & 0x0ffffff;
		// implied "1"...
		m |= 0x01000000L;
		int x = (d >> 24) & 0xff;
		x -= 255;
		byte xs = '+';
		if (x < 0) {
			x = -x;
			xs = '-';
		}
		String fmt = String.format("  %-7sDCW   %%c%%0%dd", n, prec);
		pars.emit(String.format(fmt, ms, m));
		pars.emit(String.format("         DCW   %c%02d", xs, x));
		return 
	}
}
