// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranVariable extends FortranOperand {
	private String name;

	public FortranVariable(String name, int type, int prec) {
		super(type, prec);
		// This is the internal, unique, EasyCoder, name
		this.name = name;
	}

	public static FortranOperand get(FortranParser pars, String id) {
		// TODO: when to generate unique name...
		FortranOperand fo = pars.getSym(id);
		if (fo == null) {
			// TODO: guess at type...
			fo = new FortranVariable(id, INTEGER, 3);
			pars.addSym(id, fo);
		}
		return fo;
	}

	public int kind() { return VARIABLE; }
	public String name() { return name; }

	public void genDefs(PrintStream out, FortranParser pars) {
		switch (type) {
		case INTEGER:
			pars.emit(String.format("  %-7sDCW   #%dB0", name, prec));
			break;
		case LOGICAL:
			pars.emit(String.format("  %-7sDCW   #1B0", name));
			break;
		case COMPLEX:
			pars.emit("         DCW   F0");
			// FALLTHROUGH
		case REAL:
			pars.emit(String.format("  %-7sDCW   F0", name));
			break;
		case ADDRESS:
			pars.emit(String.format("  %-7sDSA   0", name));
			break;
		}
	}
}
