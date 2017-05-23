// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public interface RandomRecordIO {
	boolean begin(int unit);	// 'true' if media mounted.
	boolean seekRecord(int cyl, int trk, int rec); // 'false' on error
	// -1 on error, FLG char on success
	int readRecord(byte[] buf, int start, int len);	// HW codes
	int writeRecord(byte[] buf, int start, int len); // HW codes
	boolean initTrack(int flg, int cyl, int trk, int reclen, int rectrk,
							int tCyl, int tTrk);
	void end();
}
