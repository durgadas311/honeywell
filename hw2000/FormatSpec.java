// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class FormatSpec {
	public int width;
	public char spec;
	public String format;
	public int offset = -1;
	public boolean parm = true;

	public FormatSpec(int c, int w, int d) {
		spec = (char)c;
		width = w;
		switch (c) {
		case 'A':
			format = String.format("%%-%ds", w);
			break;
		case 'I':
			format = String.format("%%%dd", w);
			break;
		case 'L':
			format = String.format("%%%ds", w);
			break;
		case 'O':
			format = String.format("%%%do", w);
			break;
		case 'F':
			format = fpFormat('f', w, d);
			break;
		case 'E':
			format = fpFormat('E', w, d);
			break;
		case 'G':
			format = fpFormat('G', w, d);
			break;
		case '/':
			format = "\n"; // symbolic, mostly
			parm = false;
			break;
		default:
			spec = 'H';
		case 'X':
			format = String.format("%%%ds", w);
			format = String.format(format, "");
			parm = false;
			break;
		}
	}

	// For 'H' spec, or quoted string constants
	public FormatSpec(int c, int w, String h, int off) {
		// assert c == 'H'... and h.length() == w
		spec = (char)c;
		width = w;
		format = h;
		offset = off;
		parm = false;
	}

	private String fpFormat(char c, int w, int d) {
		if (d < 0) {
			return String.format("%%%d%c", w, c);
		} else {
			return String.format("%%%d.%d%c", w, d, c);
		}
	}
}
