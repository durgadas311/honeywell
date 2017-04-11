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
	FortranOperand getSym(String id);
	void addSym(String id, FortranOperand op);

	FortranOperand parseConstant(String id);
	FortranOperand parseVariable(String id);
	FortranOperand parseOperand(String id);
	FortranOperand getIntTemp(int id);
	FortranOperand getAdrTemp(int id);
}
