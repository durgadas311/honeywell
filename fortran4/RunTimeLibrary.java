public interface RunTimeLibrary {
	FortranSubprogram refLibFunc(String fnc, FortranParser pars);
	void genLib(FortranParser pars);
	void genCode(FortranParser pars);
	void genExit(FortranParser pars);
}
