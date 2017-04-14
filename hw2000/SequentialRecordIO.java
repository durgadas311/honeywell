// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

// If begin() returns false, caller must copy records read to output.
// Also, user must ensure no file name conflicts.
// If begin() returns true,
//        appendRecord() overwrites input (no more nextRecord() allowed).
//        backspace() positions output to start of last nextRecord() read.
//
// CardReaderPunch: begin() returns 'false', rewind()/backspace() are no-op.
// MagneticTape: begin() returns 'true', rewind()/backspace() set RW pointer back.
// Disk: begin() returns 'true', rewind()/backspace() set "file" positions back. (TBD)
//
public interface SequentialRecordIO {
	boolean begin(int unit);// 'true' if input == output
	boolean ready();	// 'true' if media mounted.
	boolean empty();	// 'true' if media mounted and has no data.
	boolean rewind();	// 'false' if no media mounted. may be no-op otherwise
	boolean backspace();	// 'false' if no media mounted. may be no-op otherwise
	byte[] nextRecord();	// HW codes. null on EOF
	void appendBulk(byte[] buf, int start, int len); // HW codes
	void appendRecord(byte[] buf, int start, int len); // HW codes
	void end();
}
