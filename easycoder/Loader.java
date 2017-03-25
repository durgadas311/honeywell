// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public interface Loader {
	void begin(int adr);
	void setCode(int adr, byte[] code);
	void clear(int start, int end, byte fill);
	void end(int start);
}
