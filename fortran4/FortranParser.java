// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

// Convenience methods for statements to access parser
public interface FortranParser {
	int getLine();	// Source file line number for current code
	void setName(String var);
	void setFuncSubr(String name, String[] args);
	void setExpr(FortranExpr expr);
	void setFuncDefs(String fnc, String[] args);
	void setFuncRet(String fnc, String res);
	void emit(String code);
	FortranExpr parseExpr(String expr);
	FortranItem recurse(String stmt);

	int addrMode();

	String uniqueName();
	int getDev(int code);
	FortranOperand getSym(String id);
	void addSym(String id, FortranOperand op);
	void errsAdd(String err);		// for compile-time errors (curLine)
	void errsAdd(int line, String err);	// for generate-time errors

	void setImplicit(char ltr, int type);
	FortranOperand parseConstant(String id);
	FortranOperand parseVariable(String id);
	FortranOperand parseVariable(String id, int type);
	FortranParameter parseParameter(String id, FortranOperand scope);
	FortranSubprogram parseSubprogram(String id, int type);
	FortranArray parseArray(String id, int type, int[] dims);
	FortranOperand parseOperand(String id);
	boolean inSubroutine();
	boolean inMainProg();
	FortranOperand currSubr();
	FortranOperand getIntTemp(int id);
	FortranOperand getLogTemp(int id);
	FortranOperand getRealTemp(int id);
	FortranOperand getCplxTemp(int id);
	FortranOperand getAdrTemp(int id);
}
