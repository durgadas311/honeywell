// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

// If begin() returns false, device has special properties:
//  *	Input and Output are physically separate.
//  *	Fixed physical record size.
//  *	No rewind or backspace possible.
//  *	No insert, delete, or erase possible.
//  *	Strictly uni-directional sequential I/O, with simple
//	read-modify-write allowed under special circumstances:
//	    *	Can only add data to record, cannot alter existing data.
//  *	Device does not report END, user must provide metadata like "1EOF ".
//  *	Device cannot both read existing data and create new data unless
//	special arrangements are made (e.g. blank cards interspersed with
//	pre-punched cards).
//
// Also, user must ensure no file name conflicts.
//
// If begin() returns true,
//  *	Input and Output are the same physical medium.
//  *	appendRecord() overwrites input and establishes new EOF (no more
//	nextRecord() allowed).
//  *	backspace() positions output to start of last nextRecord() read.
//
// CardReaderPunch: begin() returns 'false',
// MagneticTape: begin() returns 'true'.
//
public interface SequentialRecordIO {
	static final byte[] _1HDR = new byte[]{ 001, 030, 024, 051, 015}; // 1HDR_
	static final byte[] _1EOF = new byte[]{ 001, 025, 046, 026, 015}; // 1EOF_
	static final byte[] _1ERI = new byte[]{ 001, 025, 051, 031, 015}; // 1ERI_
	boolean begin(int unit);// 'true' if full-function
	boolean ready();	// 'true' if media mounted.
	boolean empty();	// 'true' if media mounted and has no data.
	boolean rewind();	// 'false' if no media mounted. may be no-op otherwise
	boolean backspace();	// 'false' if no media mounted. may be no-op otherwise
	byte[] nextRecord();	// HW codes. null on EOF
	boolean appendBulk(byte[] buf, int start, int len); // HW codes
	boolean appendRecord(byte[] buf, int start, int len); // HW codes
	void end();
	int getError();
}
