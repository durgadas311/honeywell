// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

import java.io.*;

public abstract class FortranItem {
	public int label = -1;
	public int src = -1;
	public abstract void genDefs(PrintStream out, FortranParser pars);
	public abstract void genCode(PrintStream out, FortranParser pars);
	public abstract boolean error();
	public abstract String errorMessages(); // return "" if !error()
	// All implementers should also have:
	//     static FortranItem parse(String pot);
	// which returns null if 'pot' is not a potential match.
}
