// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

// Convenience methods for statements to access parser
public interface FortranParser {
	int getLine();	// Source file line number for current code
	void setName(String var);
	void setExpr(FortranExpr expr);
	void emit(String code);
	FortranExpr parseExpr(String expr);
	FortranItem recurse(String stmt);

	int addrMode();
	int intPrecision();

	String uniqueName();
	int getDev(int code);
	FortranOperand getSym(String id);
	void addSym(String id, FortranOperand op);
	void errsAdd(String err);		// for compile-time errors (curLine)
	void errsAdd(int line, String err);	// for generate-time errors
	int matchingParen(String str, int lparen);
	int matchingComma(String str, int start);

	void setImplicit(char ltr, int type);
	FortranOperand parseConstant(String id);
	FortranOperand parseVariable(String id);
	FortranOperand parseVariable(String id, int type);
	FortranParameter parseParameter(String id, FortranOperand scope, int type);
	FortranSubprogram parseSubprogram(String id, int type, int argc);
	FortranArray parseArray(String id, int type, int[] dims);
	FortranArrayRef parseArrayRef(FortranArray ary, String dims);
	String[] parseImpliedDo(String du);
	FortranFuncCall parseFuncCall(FortranSubprogram fnc, String nm, String args);
	FortranSubprogram refLibFunc(String fnc);
	FortranOperand parseOperand(String id);
	boolean inSubroutine();
	boolean inMainProg();
	FortranOperand currSubr();
	void setScope(CodeImpliedDo ido);
	void setLive(boolean live);
	void resetTemps();
	FortranOperand getIntTemp(int id);
	FortranOperand getLogTemp(int id);
	FortranOperand getRealTemp(int id);
	FortranOperand getCplxTemp(int id);
	FortranOperand getAdrTemp(int id);
	FortranOperand getTemp(int id, int type);
}
