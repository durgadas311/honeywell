// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public interface Loader {
	boolean begin(int adr, String prg, String seg);
	boolean setCode(int adr, byte[] code);
	boolean clear(int start, int end, byte fill);
	boolean range(int start, int end);
	boolean exec(int start);	// for EX, XFR directives
	boolean segment(String prg, String seg);
	boolean end(int start);	// for END directive
	int getError();
}
