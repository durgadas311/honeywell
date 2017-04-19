// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.util.Stack;
import java.util.Vector;

public class ImpliedDoLoop extends FortranOperation {
	private Vector<FortranOperand> targs; // may include ImpliedDoLoop(s)
	private boolean inIo;
	private FortranParser pars;
	private ImpliedDoLoop up = null;
	private String stl;
	private String vvar;
	private FortranOperand var;
	private FortranOperand start;
	private FortranOperand end;
	private FortranOperand step;

	public ImpliedDoLoop(String stmt, ImpliedDoLoop parent, FortranParser pars) {
		super(0, 0);
		up = parent;
		this.pars = pars;
		targs = new Vector<FortranOperand>();
		stl = pars.uniqueName();	// our unique scope
		// 'stmt' includes parens...
		if (!impliedDo(stmt)) {
			pars.errsAdd("Malformed implied-DO");
			return;
		}
	}

	public String name() { return null; } // never called (?)
	public int kind() { return IMPLIEDDO; } // never called (?)
	public void setTemp(FortranParser pars, int level) {
		for (FortranOperand fo : targs) {
			if (fo instanceof FortranOperation) {
				((FortranOperation)fo).setTemp(pars, level);
			}
		}
	}

	// '(' list ',' var '=' start ',' end [ ',' step ] ')'
	private boolean impliedDo(String du) {
		int n = du.length();
		if (n < 1 || du.charAt(n - 1) != ')') { return false; }
		int x = 1;
		--n;
		int y = du.lastIndexOf('=');	// ... var '=' ...
		if (y < 0) { return false; }
		int z = du.lastIndexOf(',', y);	// ... ',' var '=' ...
		if (z < 0) { return false; }
		if (z - x < 1) { return false; }
		String ref = du.substring(x, z);	// list
		// y => '=', z => ','
		vvar = du.substring(z + 1, y);	// var name
		z = du.indexOf(',', y);
		if (z < 0) { return false; }
		String s = du.substring(y + 1, z);	// start
		y = z + 1;
		z = du.indexOf(',', y);
		if (z < 0) {
			z = n;
		}
		String e = du.substring(y, z);	// end
		String t = "1";
		if (z < n) {
			t = du.substring(z + 1, n);	// step
		}
		String vv = String.format("%s.%s", stl, vvar); // a unique name
		// Simple variable, never exists, ... bypass the cruft...
		var = new FortranVariable(pars.uniqueName(), FortranOperand.INTEGER,
					pars.intPrecision());
		pars.addSym(vv, var);
		// TODO: some of these could be references to outer loops...
		start = pars.parseConstant(s);
		end = pars.parseConstant(e);
		step = pars.parseConstant(t);
		if (var == null || start == null || end == null || step == null) {
			return false;
		}
		if (start.type() != FortranOperand.INTEGER ||
				end.type() != FortranOperand.INTEGER ||
				step.type() != FortranOperand.INTEGER) {
			return false;
		}
		pars.setScope(this);
		x = 0;
		n = ref.length();
		FortranOperand it;
		// must not 'return' inside this loop
		while (x < n) {
			y = pars.matchingComma(ref, x);
			if (y < 0) {
				y = n;
			}
			String i = ref.substring(x, y);
			if (i.charAt(0) == '(') {
				it = new ImpliedDoLoop(i, this, pars);
				// TODO: check for errors?
			} else {
				// All variables must already exist... (?)
				it = pars.parseVariable(i);
			}
			targs.add(it);
			x = y + 1;
		}
		pars.setScope(up);
		return true;
	}

	public FortranOperand getVariable(String v) {
		if (vvar.equals(v)) {
			return var;
		}
		if (up != null) {
			return up.getVariable(v);
		}
		return null;
	}

	public void genDefs(FortranParser pars) {
		// variables and constants already gen'ed... ?
	}

	public void genCode(FortranParser pars) {
		pars.emit(String.format("         BS    %s", var.name()));
		pars.emit(String.format("         BA    %s,%s", start.name(), var.name()));
		pars.emit(String.format("  %-7sRESV  0", stl));
	}
	public Vector<FortranOperand> getItems() {
		return targs;
	}
	public void genLoop(FortranParser pars) {
		pars.emit(String.format("         BA    %s,%s", step.name(), var.name()));
		pars.emit(String.format("         C     %s,%s", end.name(), var.name()));
		pars.emit(String.format("         BCT   %s,43", stl));
	}
}
