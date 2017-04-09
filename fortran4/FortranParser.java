// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

// Convenience methods for statements to access parser
public interface FortranParser {
	int getLine();	// Source file line number for current code
	void setVariable(String var, int val);
	void setConst(int val);
	void setLocalVar(String scope, String var, int val);
	void setFuncSubr(String name, String[] args);
	void setExpr(FortranExpr expr);
	void setFuncDefs(String fnc, String[] args);
	void setFuncRet(String fnc, String res);
	void emit(String code);
	FortranExpr parseExpr(String expr);
	FortranItem recurse(String stmt);

	String tempAdr();
	String tempInt();
	String tempReal();
	String tempLog();
	String tempComplex();
	int addrMode();
}
