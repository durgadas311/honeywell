// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

// Convenience methods for statements to access parser
public interface FortranParser {
	int getLine();	// Source file line number for current code
	boolean addSymbol(String sym); // 'true' if new (does not exist yet).
	void setVariable(String var, int val);
	void setConst(int val);
	void setLocalVar(String scope, String var, int val);
	void setFuncSubr(String name, String[] args);
	void setFuncRet(String name);
	void setExpr(FortranExpr expr);
	void emit(String code);
}
