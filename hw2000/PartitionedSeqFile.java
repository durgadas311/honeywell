// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

// Partitioned Sequential files can:
//	GET - (IN,IN/OUT) sequential read of member items
//	REP - (IN/OUT) replace of current member item (previous GET)
//	PUT - (OUT) write member items sequentially
//	SETM - open/create a partition/member
//	ENDM - close current partition/member
//	MALTER - modify member
//	MREL - erase all members

public class PartitionedSeqFile extends SequentialFile {
	int idxLen;
	int mmbIdxLen;

	// Caller locates *VOLNAMES*, etc items and passes info to this ctor.
	public PartitionedSeqFile(RandomRecordIO dsk, int unit, byte[] name, boolean prot,
			CoreMemory blkBuf, int blkAdr,
			int itmLen, int recLen, int recTrk, int recBlk,
			int idxLen, int mmbIdxLen,
			DiskUnit[] alloc) {
		super(dsk, unit, name, prot, blkBuf, blkAdr,
				itmLen, recLen, recTrk, recBlk, alloc);
		this.idxLen = idxLen;
		this.mmbIdxLen = mmbIdxLen;
	}

	// Create a R/O clone of this file. Not used?
	@Override
	public DiskFile dup() {
		PartitionedSeqFile dup = new PartitionedSeqFile(dsk, unit, name, true,
				null, 0, itmLen, recLen, recTrk, recBlk,
				idxLen, mmbIdxLen, units);
		return dup;
	}

	//
	// These routines are for the MOVE item delivery mode...
	//
	@Override
	public boolean getItem(CoreMemory itm, int adr) {
		return false;
	}

	@Override
	public boolean repItem(CoreMemory itm, int adr) {
		return false;
	}

	@Override
	public boolean putItem(CoreMemory itm, int adr) {
		return false;
	}

	@Override
	public boolean close() {
		boolean ok1 = endMemb();
		boolean ok2 = super.close();
		return (ok1 && ok2);
	}

	@Override
	public int getType() { return DiskFile.PART_SEQ; }

	@Override
	public boolean setMemb(CoreMemory memb, int adr, int mode) {
		// Open or create member.
		error = 00434;
		return false;
	}

	@Override
	public boolean endMemb() {
		// Must update member index for blocks used,
		// for both this member and the *UNUSED* member.
		// Also, terminate member data with [EOD^.
		// Also must tolerate being called when no active member.
		put = false;
		error = 00434;
		return false;
	}

	@Override
	public boolean alterMemb(CoreMemory memb, int adr, int op,
				CoreMemory newm, int nadr) {
		error = 00434;
		return false;
	}

	@Override
	public boolean release() {
		// This must handle the case of a new file,
		// i.e. no existing *ENDINDEX* member.
		error = 00434;
		return false;
	}
}
