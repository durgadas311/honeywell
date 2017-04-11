// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranConstant extends FortranOperand {
	private String name;
	private String value;
	private int vi;
	private boolean vl;
	private float vr;
	private float vx;

	public FortranVariable(Object val) {
		super(0, 0); // re-set type, precision later...
		// TODO: also save true value?
		if (val instanceof Integer) {
			type = INTEGER:
			vi = (Integer)val;
			int p = 32 - Integer.numberOfLeadingZeros(vi);
			p = (p + 5) / 6;
			prec = p;
			break;
		} else if (val instanceof Boolean) {
			type = LOGICAL:
			vl = (Boolean)val;
			break;
		} else if (val instanceof Float) {
			type = REAL:
			prec = 7;
			vr = (Float)val;
			break;
		} else if (val instanceof Float[]) {
			type = COMPLEX:
			prec = 7;
			float[] v = (Float[])val;
			vr = v[0];
			vx = v[1];
			break;
		}
	}

	public int kind() { return VARIABLE; }
	public String name() { return name; } // no names for constants...???

	public void genDefs(PrintStream out, FortranParser pars) {
		String val;
		String id;
		switch (type) {
		case INTEGER:
			value = String.format("#%dB%d", prec, vi);
			if (prec > 3) {
				name = pars.uniqueName();
			} else {
				name = String.format(":%d", vi);
			}
			id = name;
			break;
		default:
		case LOGICAL:
			val = String.format("#1B%d", vl ? 1 : 0);
			name = String.format(":%d", vl ? 1 : 0);
			id = name;
			break;
		case REAL:
			id = String.format("F%g", vf);
			name = pars.uniqueName();
			emitReal(pars, name, vf);
			break;
		case COMPLEX:
			id = String.format("F%gI%g", vf, vx);
			if (
			name = pars.uniqueName();
			emitReal(pars, name, vf);
			emitReal(pars, "", vx);
			break;
		}
		if (pars.addConst(this)) {
			pars.emit(String.format("  %-7sDCW   %s", name, val));
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
