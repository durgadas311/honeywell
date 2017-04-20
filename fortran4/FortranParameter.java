// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

// Parameters might change type (immediately) after declaration.
// Need to allow this, but not if parameter has already been referenced(?)
public class FortranParameter extends FortranVariable {
	private boolean referenced = false;
	public FortranParameter(String name, int type) {
		super(name, type);
	}

	@Override
	public int kind() { return PARAMETER; }
	@Override
	public String name() { return '(' + name + ')'; }

	@Override
	public void genDefs(FortranParser pars) {
	}

	// Specific to FortranParameter
	public String ref() { return name; }
	public boolean setType(int type) {
		if (this.type == type) {
			return true;
		}
		if (referenced) {
			return false;
		}
		this.type = type;
		return true;
	}
	public void reference() { referenced = true; }
}
