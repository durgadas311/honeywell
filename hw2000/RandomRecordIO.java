// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public interface RandomRecordIO {
	int numRecords(int recLen);
	int getError();
	int numTracks();
	int numCylinders();
	//
	boolean begin(int unit);	// 'true' if media mounted.
	// -1 on error, FLG char on success
	int seekRecord(int cyl, int trk, int rec); // 'false' on error
	boolean readRecord(CoreMemory buf, int start, int len);	// HW codes
	boolean writeRecord(CoreMemory buf, int start, int len); // HW codes
	boolean initTrack(int flg, int cyl, int trk, int reclen, int rectrk,
							int tCyl, int tTrk);
	void end();
}
