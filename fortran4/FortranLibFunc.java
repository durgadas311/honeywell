// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class FortranLibFunc extends FortranSubprogram {
	private int code;

	public FortranLibFunc(String name, int code, int type, int argc,
			FortranParser pars) {
		super(name, type, argc, pars);
		this.code = code;
	}

	@Override
	public void genDefs(FortranParser pars) {
		// No one else does this for us...
		if (ret != null) {
			pars.emit(String.format("   %-6sDSA   0", ret.name()));
		}
	}
	@Override
	public void genCode(FortranParser pars) {}

	public int code() { return code; }
}
