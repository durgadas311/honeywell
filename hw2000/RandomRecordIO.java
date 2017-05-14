// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public interface RandomRecordIO {
	boolean begin(int unit);	// 'true' if media mounted.
	boolean seekRecord(int cyl, int trk, int rec); // 'false' on error
	byte[] readRecord();	// HW codes. null on error
	void writeRecord(byte[] buf, int start, int len); // HW codes
	void initTrack(int cyl, int trk, int reclen);
	void end();
}
