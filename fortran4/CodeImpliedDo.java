// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.util.Stack;
import java.util.Vector;

public class CodeImpliedDo extends ImpliedDoLoop {
	private CodeImpliedDo up;
	private String stl;
	private String vvar;
	private FortranOperand var;
	private FortranOperand start;
	private FortranOperand end;
	private FortranOperand step;
	private Vector<FortranOperand> targs; // may include ImpliedDoLoop(s)

	// CTOR for code-generating versions (READ, WRITE)
	public CodeImpliedDo(String stmt, CodeImpliedDo parent, FortranParser pars) {
		super(stmt, pars);
		up = parent;
		if (_targs == null) {
			return;
		}
		// DoLoop parsed, now convert it into CodeImpliedDo
		newCodeImpliedDo();
	}

	public ImpliedDoLoop nest(String stmt, ImpliedDoLoop parent, FortranParser pars) {
		return new CodeImpliedDo(stmt, (CodeImpliedDo)parent, pars);
	}

	public boolean init() {
		// Used for loop label and variable scope.
		stl = pars.uniqueName();	// our unique scope
		vvar = String.format("%s.%s", stl, _var); // a unique name
		// Simple variable, never exists, ... bypass the cruft...
		var = new FortranVariable(pars.uniqueName(), FortranOperand.INTEGER,
					pars.intPrecision());
		pars.addSym(vvar, var);
		// TODO: some of these could be references to outer loops...
		start = pars.parseConstant(_start);
		end = pars.parseConstant(_end);
		step = pars.parseConstant(_step);
		if (var == null || start == null || end == null || step == null) {
			return false;
		}
		if (start.type() != FortranOperand.INTEGER ||
				end.type() != FortranOperand.INTEGER ||
				step.type() != FortranOperand.INTEGER) {
			return false;
		}
		return true;
	}

	private void newCodeImpliedDo() {
		targs = new Vector<FortranOperand>();
		pars.setScope(this);
		FortranOperand nit;
		for (Object it : _targs) {
			if (it instanceof CodeImpliedDo) {
				nit = (CodeImpliedDo)it;
			} else {
				String i = (String)it;
				nit = pars.parseVariable(i);
			}
			targs.add(nit);
		}
		pars.setScope(up);
	}

	public void setTemp(FortranParser pars, int level) {
		for (FortranOperand fo : targs) {
			if (fo instanceof FortranOperation) {
				((FortranOperation)fo).setTemp(pars, level);
			}
		}
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
