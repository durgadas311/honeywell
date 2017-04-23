// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.util.Vector;

public abstract class ImpliedDoLoop extends FortranOperation {
	protected FortranParser pars;
	protected String _var;
	protected String _start;
	protected String _end;
	protected String _step;
	protected Vector<Object> _targs; // ImpliedDoLoop or String

	public ImpliedDoLoop(String stmt, FortranParser pars) {
		super(0, 0);
		this.pars = pars;
		_targs = new Vector<Object>();
		if (!impliedDo(stmt)) {
			pars.errsAdd("Malformed implied-DO");
			_targs = null; // error indicator
			return;
		}
	}

	public String name() { return null; } // never called (?)
	int kind() { return IMPLIEDDO; } // never called (?)
	abstract void genDefs(FortranParser pars);
	abstract void genCode(FortranParser pars);

	abstract ImpliedDoLoop nest(String stmt, ImpliedDoLoop parent, FortranParser pars);
	abstract boolean init();

	// '(' list ',' var '=' start ',' end [ ',' step ] ')'
	protected boolean impliedDo(String du) {
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
		_var = du.substring(z + 1, y);	// var name
		z = du.indexOf(',', y);
		if (z < 0) { return false; }
		_start = du.substring(y + 1, z);	// start
		y = z + 1;
		z = du.indexOf(',', y);
		if (z < 0) {
			z = n;
		}
		_end = du.substring(y, z);	// end
		_step = "1";
		if (z < n) {
			_step = du.substring(z + 1, n);	// step
		}
		if (!init()) { return false; }
		x = 0;
		n = ref.length();
		// must not 'return' inside this loop
		Object it;
		while (x < n) {
			y = pars.matchingComma(ref, x);
			if (y < 0) {
				y = n;
			}
			String i = ref.substring(x, y);
			if (i.charAt(0) == '(') {
				it = nest(i, this, pars);
				// TODO: check for errors?
			} else {
				it = i;
			}
			_targs.add(it);
			x = y + 1;
		}
		return true;
	}
}
