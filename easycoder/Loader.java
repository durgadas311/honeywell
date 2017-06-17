// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public interface Loader {
	void begin(int adr, String prg, String seg, String rev, long vis);
	void setCode(int adr, byte[] code);
	void clear(int start, int end, byte fill);
	void range(int start, int end);
	void exec(int start);	// for EX, XFR directives
	void segment(String prg, String seg, String rev, long vis);
	void end(int start);	// for END directive
}
