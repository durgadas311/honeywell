// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public abstract class FortranOperation extends FortranOperand {
	protected FortranOperation(int type, int prec) {
		super(type, prec);
	}

	abstract void setTemp(FortranParser pars, int level);
}
