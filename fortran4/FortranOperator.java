// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class FortranOperator extends FortranOperation {
	static final int NEG = 0;
	static final int PWR = 1;
	static final int MULT = 2;
	static final int DIV = 3;
	static final int ADD = 4;
	static final int SUB = 5;
	static final int LE = 6;
	static final int LT = 7;
	static final int GT = 8;
	static final int GE = 9;
	static final int EQ = 10;
	static final int NE = 11;
	static final int NOT = 12;
	static final int AND = 13;
	static final int OR = 14;

	static String[] parse = new String[]{
		"-", // skipped in searches...
		"**", "*", "/", "+", "-",
		".LE.", ".LT.", ".GT.", ".GE.", ".EQ.", ".NE.",
		".NOT.",
		".AND.", ".OR.",
	};
	static int[] precedence = new int[]{
		0, 1, 2, 2, 3, 3,
		4, 4, 4, 4, 4, 4,
		5, 6, 7
	};

	private int op;
	private int lev;
	private int opType;
	private FortranOperand left;
	private FortranOperand right;

	FortranOperand tmp = null;
	FortranOperand tru = null;

	// TODO: need to ensure constant "1" exists...
	public FortranOperator(int op, int lev, FortranParser pars) {
		super(0, 0);
		this.op = op;
		this.lev = lev;
		left = null;
		right = null;
		if (op >= LE) {
			// .FALSE. is 0 but we only use BS
			tru = pars.parseConstant(".TRUE.");
		}
	}

	public int kind() { return OPERATOR; }
	public String name() { return tmp == null ? "null" : tmp.name(); }
	public int preced() { return precedence[op]; }
	public int level() { return lev; }

	public void genDefs(FortranParser pars) {
		// TODO: need to get temp var type and num...
		// TODO: might reference an external function
		if (left instanceof FortranOperation) {
			left.genDefs(pars);
		}
		if (right instanceof FortranOperation) {
			right.genDefs(pars);
		}
	}

	public void setTemp(FortranParser pars, int level) {
		switch (type) {
		case INTEGER:
			tmp = pars.getIntTemp(level);
			break;
		case LOGICAL:
			tmp = pars.getLogTemp(level);
			break;
		case REAL:
			tmp = pars.getRealTemp(level);
			break;
		case COMPLEX:
			tmp = pars.getCplxTemp(level);
			break;
		default:
		}
		if (left instanceof FortranOperation) {
			((FortranOperation)left).setTemp(pars, level + 1);
		}
		if (right instanceof FortranOperation) {
			((FortranOperation)right).setTemp(pars, level + 1);
		}
	}

	// ----- Only for FortranOperator -----
	public int oper() { return op; }

	public int parseLen() { return parse[op].length(); }

	static public boolean relCheck(String op, int idx) {
		for (int x = LE; x <= OR; ++x) {
			if (op.startsWith(parse[x], idx)) {
				return true;
			}
		}
		return false;
	}

	// TODO: unary "-" ?
	static public FortranOperator get(String op, int idx, int lev, FortranParser pars) {
		for (int x = 1; x < parse.length; ++x) {
			if (op.startsWith(parse[x], idx)) {
				return new FortranOperator(x, lev, pars);
			}
		}
		return null;
	}

	public FortranOperand getLeft() { return left; }
	public FortranOperand getRight() { return right; }
	public void setLeft(FortranOperand opd) {
		if (op == NOT || op == NEG) {
			// internal error? esp. if opd != null
			return;
		}
		left = opd;
		resetType();
	}

	public void setRight(FortranOperand opd) {
		right = opd;
		resetType();
	}

	// TODO: add validate chain (all expr elements) to check types.
	private void resetType() {
		int ltyp = 0;
		int rtyp = 0;
		if (left != null) {
			ltyp = left.type();
		} else if (right != null) {
			ltyp = right.type();
		}
		if (right != null) {
			rtyp = right.type();
		}
		if (ltyp != rtyp) {
			// TODO: promote...
		}
		// if (op <= NE) {
		//	must be numeric opType...
		// } else {
		//	LOGICAL
		// }
		opType = rtyp;
		if (op >= LE) {
			type = LOGICAL;
		} else {
			type = rtyp;
		}
	}

	public void genCode(FortranParser pars) {
		// TODO: how does this work... or is it done externally?
		if (left != null) {
			left.genCode(pars);
		}
		// TODO: unary op handling...
		if (right != null) {
			right.genCode(pars);
		}
		switch (type) {
		case INTEGER:
			genCodeInt(pars);
			break;
		case LOGICAL:
			genCodeLog(pars);
			break;
		case REAL:
			genCodeReal(pars);
			break;
		case COMPLEX:
			genCodeCplx(pars);
			break;
		}
	}

	private void genCodeInt(FortranParser pars) {
		int acbfxp = 0;
		// TODO: work out type differences...
		if (left != null && !left.name().equals(tmp.name())) {
			pars.emit(String.format("         BS    %s", tmp.name()));
			pars.emit(String.format("         BA    %s,%s",
						left.name(), tmp.name()));
		}
		switch (op) {
		case NEG:
			pars.emit(String.format("         BS    %s", tmp.name()));
			pars.emit(String.format("         BS    %s,%s",
						right.name(), tmp.name()));
			return;
		case ADD:
			pars.emit(String.format("         BA    %s,%s",
						right.name(), tmp.name()));
			return;
		case SUB:
			pars.emit(String.format("         BS    %s,%s",
						right.name(), tmp.name()));
			return;
		case MULT:
			acbfxp = 020; // we're just making this up...
			break;
		case DIV:
			acbfxp = 021; // we're just making this up...
			break;
		case PWR:
			acbfxp = 022; // we're just making this up...
			break;
		}
		pars.emit(              "         B     $ACBFXP");
		pars.emit(String.format("         DSA   %s", right.name()));
		pars.emit(String.format("         DSA   %s", tmp.name()));
		pars.emit(String.format(" R       DC    #1C%02o", acbfxp));
	}

	private void genCodeLog(FortranParser pars) {
		// Logical ops only guarantee one character is valid
		int sst = pars.addrMode() * 2 + 2;	// length of SST (as used here)
		int bce = pars.addrMode() * 2 + 2;	// length of BCE (as used here)
		switch (op) {
		case AND:
			if (left != null && !left.name().equals(tmp.name())) {
				pars.emit(String.format("         SST   %s,%s,77",
						left.name(), tmp.name()));
			}
			pars.emit(String.format("         EXT   %s,%s",
						right.name(), tmp.name()));
			break;
		case OR:
			if (left != null && !left.name().equals(tmp.name())) {
				pars.emit(String.format("         SST   %s,%s,77",
						left.name(), tmp.name()));
			}
			pars.emit(String.format("         BA    %s,%s",
						right.name(), tmp.name()));
			pars.emit(String.format("         BCE   *+%d,%s,00",
						sst + bce, tmp.name()));
			pars.emit(String.format("         SST   %s,%s,77",
						tru.name(), tmp.name()));
			break;
		case NOT:
			if (right != null && !right.name().equals(tmp.name())) {
				pars.emit(String.format("         SST   %s,%s,77",
						right.name(), tmp.name()));
			}
			pars.emit(String.format("         HA    %s,%s",
							tru.name(), tmp.name()));
			break;
		default:
			genCodeRel(pars);
			break;
		}
	}

	private void genCodeRel(FortranParser pars) {
		int ba = pars.addrMode() * 2 + 1;	// length of BA (as used here)
		int bct = pars.addrMode() + 2;		// length of BCT (as used here)
		pars.emit(String.format("         C     %s,%s",
						right.name(), left.name()));
		pars.emit(String.format("         BS    %s", tmp.name()));
		// Note reversal of relation, since we assume FALSE above
		int var = 047; // Unconditional; what is a good default?
		switch (op) {
		case LE:
			var = 044;	// B>A
			break;
		case LT:
			var = 046;	// B>=A
			break;
		case GE:
			var = 041;	// B<A
			break;
		case GT:
			var = 043;	// B<=A
			break;
		case EQ:
			var = 045;	// B!=A
			break;
		case NE:
			var = 042;	// B=A
			break;
		}
		pars.emit(String.format("         BCT   *+%d,%02o", ba + bct, var));
		pars.emit(String.format("         BA    %s,%s",
					tru.name(), tmp.name()));
	}

	private void genCodeReal(FortranParser pars) {
		int acbfph = 0;
		if (left != null && !left.name().equals(tmp.name())) {
			// All REAL are same size, LCA is safe...
			pars.emit(String.format("         LCA   %s,%s",
						left.name(), tmp.name()));
		}
		// TODO: add conversion calls?
		switch (op) {
		case NEG:
			acbfph = 015; // we're just making this up...
			break;
		case ADD:
			acbfph = 016; // we're just making this up...
			break;
		case SUB:
			acbfph = 017; // we're just making this up...
			break;
		case MULT:
			acbfph = 020; // we're just making this up...
			break;
		case DIV:
			acbfph = 021; // we're just making this up...
			break;
		case PWR:
			acbfph = 022; // we're just making this up...
			break;
		}
		pars.emit(              "         B     $ACBFPH");
		pars.emit(String.format("         DSA   %s", right.name()));
		pars.emit(String.format("         DSA   %s", tmp.name()));
		pars.emit(String.format(" R       DC    #1C%02o", acbfph));
	}

	private void genCodeCplx(FortranParser pars) {
	}
}
