public class FortranLibFunc extends FortranSubprogram {
	private int code;

	public FortranLibFunc(String name, int code, int type, int argc,
			FortranParser pars) {
		super(name, type, argc, pars);
		this.code = code;
	}

	@Override
	public void genDefs(FortranParser pars) {}
	@Override
	public void genCode(FortranParser pars) {}

	public int code() { return code; }
}
