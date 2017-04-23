// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.util.Stack;
import java.util.Vector;

// Build a flattened list of expanded vars - could be large
public class DataImpliedDo extends ImpliedDoLoop {
	private Vector<String> targs = null;

	public DataImpliedDo(String stmt, FortranParser pars) {
		super(stmt, pars);
		if (_targs == null) {
			return;
		}
		// DoLoop parsed, now expand it into strings
		int s, e, t;
		try {
			s = Integer.valueOf(_start);
			e = Integer.valueOf(_end);
			t = Integer.valueOf(_step);
		} catch (Exception ee) {
			return;
		}
		targs = new Vector<String>();
		String re = "(.*[\\(,])" + _var + "([\\),].*)";
		for (int d = s; d <= e; d += t) {
			String rep = String.format("$1%d$2", d);
			for (Object _t : _targs) {
				if (_t instanceof DataImpliedDo) {
					Vector<String> vs = ((DataImpliedDo)_t).getTargets();
					if (vs != null) for (String o : vs) {
						targs.add(o.replaceFirst(re, rep));
					}
				} else if (_t instanceof String) {
					targs.add(((String)_t).replaceFirst(re, rep));
				} else {
					pars.errsAdd("Malformed DATA implied-DO");
				}
			}
		}
	}

	public ImpliedDoLoop nest(String stmt, ImpliedDoLoop parent, FortranParser pars) {
		return new DataImpliedDo(stmt, pars);
	}
	public boolean init() { return true; }

	public Vector<String> getTargets() {
		// TODO: is it necessary/safe to free memory here?
		Vector<String> vs = targs;
		//targs = null;
		return vs;
	}

	public void setTemp(FortranParser pars, int level) {}
	public void genDefs(FortranParser pars) {}
	public void genCode(FortranParser pars) {}
}
