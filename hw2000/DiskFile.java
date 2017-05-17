// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public interface DiskFile {
	boolean getItem(byte[] itm);
	boolean repItem(byte[] itm);
	boolean putItem(byte[] itm);
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
