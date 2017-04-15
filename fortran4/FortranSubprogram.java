// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranSubprogram extends FortranVariable {
	public FortranSubprogram(String name, int type) {
		super(name, type);
	}

	@Override
	public int kind() { return FUNCTION; }

	@Override
	public void genDefs(FortranParser pars) {
	}
}
