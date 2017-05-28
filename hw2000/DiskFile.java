// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public interface DiskFile {
	// Open done by ctor and caller.
	boolean getItem(CoreMemory itm, int adr);
	boolean repItem(CoreMemory itm, int adr);
	boolean putItem(CoreMemory itm, int adr);
	boolean close();
	// Non-standard: (Not part of MOD1 i/f)
	byte[] getName();
	String getError();
	int itemLen();
	int recordLen();
	int blockLen();
	boolean rewind();
	boolean sync();
	boolean seek(int cyl, int trk, int rec, int itm);
	boolean seek(int[] ctri);
	int[] getAddress();
}
