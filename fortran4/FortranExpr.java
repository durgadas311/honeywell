// Copyright (c) 2017 douglas Miller <durgadas311@gmail.com>

public class FortranExpr {
	public static final int INTEGER = 1;
	public static final int REAL = 2;
	public static final int LOGICAL = 3;
	public static final int COMPLEX = 4;

	public FortranExpr(String expr) {
	}

	public int type() { return INTEGER; }
	public String getResult() {
		switch (type) {
		case INTEGER:
			return "$TMPI";
		case REAL:
			return "$TMPR;
		case COMPLEX:
			return "$TMPX;
		case LOGICAL:
			return "$TMPL;
		}
		return "$FOO";
	}
}
