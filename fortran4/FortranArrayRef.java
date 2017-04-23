// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranArrayRef extends FortranOperation {
	private FortranExpr adr;
	private int iadr;
	private FortranArray ary;
	private String name;
	private FortranOperand tmp;

	public FortranArrayRef(FortranArray a, FortranExpr x) {
		super(a.type(), a.precision());
		adr = x;
		ary = a;
		name = "XXX"; // TODO: fix this - if needed
	}

	public FortranArrayRef(FortranArray a, int x) {
		super(a.type(), a.precision());
		adr = null;
		ary = a;
		iadr = x;
		name = "XXX"; // TODO: fix this - if needed
	}

	@Override
	public int kind() { return ARRAYREF; }

	public void setValue(String val) {
		if (adr != null) {
			int idx = adr.computeInt();
			ary.setValue(idx, val);
		} else {
			ary.setValue(iadr, val);
		}
	}

	@Override
	public void genDefs(FortranParser pars) {
		// 'tmp' already allocated by normal variable process...
		// pars.emit(String.format("  %-7sDSA   0", tmp.name()));
	}

	@Override
	public String name() { return name; }

	@Override
	public void genCode(FortranParser pars) {
		pars.setExpr(adr);
		pars.emit(String.format("         LCA   %s,%s", ary.ref(), tmp.name()));
		pars.emit(String.format("         BA    %s,%s", adr.getResult(), tmp.name()));
	}

	@Override
	public void setTemp(FortranParser pars, int level) {
		tmp = pars.getAdrTemp(level);
		name = String.format("(%s-%d)", tmp.name(), pars.addrMode() - 1);
		adr.setTemp(pars, level + 1);
	}
}
