// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Vector;
import java.io.*;

public class FortranFuncCall extends FortranOperation {
	private FortranExpr[] args;
	private FortranSubprogram fnc;
	private String name;
	private FortranOperand tmp;

	public FortranFuncCall(FortranSubprogram f, String s, String a, FortranParser pars) {
		super(0, 0); // 'f' could be null, need to validate first.
		// Each arg must have it's own location - if it's not a direct variable ref
		Vector<FortranExpr> vx = new Vector<FortranExpr>();
		int n = a.length();
		int x = 0;
		do {
			int y = pars.matchingComma(a, x);
			if (y < 0) {
				y = n;
			}
			vx.add(new FortranExpr(a.substring(x, y), pars));
			// TODO: check errors?
			x = y + 1;
		} while (x < n);
		args = vx.toArray(new FortranExpr[0]);
		if (f == null) {
			// Foward declaration... '-2' means don't set type
			// TODO: could guess at type from context, but
			// can't do that here (caller must).
			f = pars.parseSubprogram(s, -2, args.length);
		}
		fnc = f;
		name = f.name();
		type = f.type();
		prec = f.precision();
		if (args.length != f.numArgs()) {
			pars.errsAdd("Wrong number of function parameters");
			return;
		}
	}

	@Override
	public int kind() { return FUNCTIONCALL; }

	@Override
	public void genDefs(FortranParser pars) {
	}

	@Override
	public String name() { return fnc.getResult(); }

	@Override
	public void genCode(FortranParser pars) {
		int x;
		for (x = 0; x < args.length; ++x) {
			args[x].genCode(pars);
		}
		pars.emit(String.format("         B     %s", fnc.name()));
		// If functions could have 0 params, this needs to be different...
		pars.emit(String.format("         DSA   %s", fnc.getResult()));
		for (x = 0; x < args.length - 1; ++x) {
			pars.emit(String.format("         DSA   %s", args[x].getResult()));
		}
		pars.emit(String.format(" R       DSA   %s", args[x].getResult()));
	}

	@Override
	public void setTemp(FortranParser pars, int level) {
		tmp = pars.getAdrTemp(level);
		name = String.format("(%s-%d)", tmp.name(), pars.addrMode() - 1);
		// Each must have their own temp var???
		for (int x = 0; x < args.length; ++x) {
			args[x].setTemp(pars, level + x + 1);
		}
	}
}
