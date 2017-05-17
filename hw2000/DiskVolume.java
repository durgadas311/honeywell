// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;

public class DiskVolume {
	public static final byte[] _1VOL = new byte[]{ 001, 065, 046, 043, 015}; // 1VOL_
	public static final byte[] _EOD_ = new byte[]{ 076, 025, 046, 024, 077}; // [EOD^
	public static final byte[] VOLNAMES = new byte[]
		{ 054, 065, 046, 043, 045, 021, 044, 025, 062, 054 }; // "*VOLNAMES*"
	public static final byte[] VOLDESCR = new byte[]
		{ 054, 065, 046, 043, 024, 025, 062, 023, 051, 054 }; // "*VOLDESCR*"
	public static final byte[] VOLALLOC = new byte[]
		{ 054, 065, 046, 043, 021, 043, 043, 046, 023, 054 }; // "*VOLALLOC*"
	RandomRecordIO dsk = null;
	int unit;
	boolean mounted = false;
	String error = null;
	byte[] name;	// a.k.a. volume name (i.e. label - may change)
	byte[] snum;	// a.k.a. volume serial number (i.e. UUID)
	static final int def_reclen = 250;
	// These should never be closed (except by unmount())
	DiskFile volNames;
	DiskFile volDescr;
	DiskFile volAlloc;

	// Each operation must be atomic, i.e. bounded by dsk.begin()/dsk.end().
	// This is because the peripheral cannot operate on multiple units
	// at the same time (single buffer, etc).
	// cyl 0, trk 0 = bootstrap
	// cyl 0, trk 1 = volume header/label (first record only)
	// cyl 0, trk 2 = (3-7 tracks) volume directory
	//              *VOLNAMES* == 1 track (cyl 0, trk 2)
	//              *VOLDESCR* <= 3 tracks (cyl 0, trk 3...) *
	//              *VOLALLOC* <= 3 tracks (cyl 0, trk 3+...) *
	// * Based on MAX FILES parameter specified at init time.
	//   *VOLDESCR* and *VOLALLOC* are the same length (in tracks).
	// Or...
	// Volume Header specifies beginning CCTT of *VOLNAMES* (*VOLDESCR*... follow?)
	//
	public DiskVolume(RandomRecordIO dsk, int unit) {
		this.dsk = dsk;
		this.unit= unit;
		// Both of these appear to be strings (characters, not binary)
		name = new byte[6];
		snum = new byte[6];
		Arrays.fill(name, (byte)015);
	}

	public String getError() {
		return error;
	}

	public byte[] getName() {
		return name;
	}

	public byte[] getSerial() {
		return snum;
	}

	public DiskFile getVolNames() {
		return volNames;
	}

	public DiskFile getVolDescr() {
		return volDescr;
	}

	public DiskFile getVolAlloc() {
		return volAlloc;
	}

	public boolean mount() {
		// "mount" the volume.
		if (!dsk.begin(unit)) {
			error = dsk.getError();
			return false;
		}
		// TODO: Optionally use alloc values from Volume header.
		int cyl = 0;
		int trk = 2;
		try {
			byte[] r = new byte[def_reclen];
			int f = dsk.seekRecord(0, 1, 0);
			if (f < 0) {
				error = dsk.getError();
				return false;
			}
			if (!dsk.readRecord(r, 0, -1)) {
				error = dsk.getError();
				return false;
			}
			if (!compare(_1VOL, r, 0)) {
				// Not previously initialized
				error = "Disk pack not initialized";
				return false;
			}
			// TODO: trim trailing blanks?
			System.arraycopy(r, 5, name, 0, 6);
			System.arraycopy(r, 12, snum, 0, 6);
			// TODO: verify device type (what vlaue is 278?)
			// TODO: what other data?
			if ((r[19] & 077) == 010) {
				cyl = (r[23] & 077) << 6;
				cyl |= (r[24] & 077);
				trk = (r[25] & 077) << 6;
				trk |= (r[26] & 077);
			}
			mounted = true;
		} finally {
			dsk.end();
		}
		// Length of data may vary, but starting cyl/trk cannot.
		volNames = newVolNames(dsk, unit, cyl, trk);
		trk += 1;
		volDescr = newVolDescr(dsk, unit, cyl, trk);
		trk += 3;
		volAlloc = newVolAlloc(dsk, unit, cyl, trk);
		trk += 3;
		return true;
	}

	static public DiskFile newVolNames(RandomRecordIO dsk, int unit,
							int cyl, int trk) {
		if (cyl < 0) {
			cyl = 0;
		}
		if (trk < 0) {
			trk = 2;
		}
		int recTrk = dsk.numRecords(def_reclen);
		// TODO: compute records/block... following assumes 250 char record.
		return new SequentialFile(dsk, unit, VOLNAMES,
					30, def_reclen, recTrk, 3,
					cyl, trk, cyl, trk + 0);
	}

	static public DiskFile newVolDescr(RandomRecordIO dsk, int unit,
							int cyl, int trk) {
		if (cyl < 0) {
			cyl = 0;
		}
		if (trk < 0) {
			trk = 3;
		}
		int recTrk = dsk.numRecords(def_reclen);
		// TODO: compute records/block... following assumes 250 char record.
		return new SequentialFile(dsk, unit, VOLDESCR,
					100, def_reclen, recTrk, 2,
					cyl, trk, cyl, trk + 2);
	}

	static public DiskFile newVolAlloc(RandomRecordIO dsk, int unit,
							int cyl, int trk) {
		if (cyl < 0) {
			cyl = 0;
		}
		if (trk < 0) {
			trk = 6;
		}
		int recTrk = dsk.numRecords(def_reclen);
		// TODO: compute records/block... following assumes 250 char record.
		return new SequentialFile(dsk, unit, VOLALLOC,
					20, def_reclen, recTrk, 2,
					cyl, trk, cyl, trk + 2);
	}

	public void unmount() {
		// must commit all buffered data... is there any?
		volNames.close();
		volDescr.close();
		volAlloc.close();
		volNames = null;
		volDescr = null;
		volAlloc = null;
		mounted = false;
	}

	// TODO: Support access to *VOLNAMES*, etc., here?
	// TODO: name, blockBuffer, workBuffer, moveLocate, inOut,
	public DiskFile openFile(byte[] name) {
		byte[] nItm = new byte[volNames.itemLen()];
		volNames.rewind();
		while (true) {
			if (!volNames.getItem(nItm)) {
				error = volNames.getError();
				return null;	// error
			}
			if ((nItm[0] & 077) == 077) {
				continue;
			}
			if (compare(_EOD_, nItm, 0)) {
				error = "No file";
				return null;	// not found
			}
			if (compare(name, nItm, 0)) {
				break;
			}
		}
		byte[] dItm = new byte[volDescr.itemLen()];
		byte[] aItm = new byte[volAlloc.itemLen()];
		// TODO: volume sequence number, etc.
		int[] ctri = getSome(nItm, 14, 4);
		// get *VOLDESCR* item...
		if (!volDescr.seek(ctri)) {
			error = volDescr.getError();
			return null;
		}
		if (!volDescr.getItem(dItm)) {
			error = volDescr.getError();
			return null;
		}
		int type = dItm[0] & 077;
		int[] desc = getSome(dItm, 1, 5);
		// TODO: overflow, etc.
		DiskUnit[] units = new DiskUnit[6];
		ctri = getSome(nItm, 22, 4);
		// get *VOLALLOC* item(s)...
		for (int y = 0; y < units.length; ++y) {
			if ((ctri[0] != 0 || ctri[1] != 0 || ctri[2] != 0 || ctri[3] != 0) &&
					!volAlloc.seek(ctri)) {
				error = volAlloc.getError();
				return null;
			}
			if (!volAlloc.getItem(aItm)) {
				error = volAlloc.getError();
				return null;
			}
			int sts = aItm[0] & 077;
			if (sts == 077) { // error (?)
				error = "Empty file";
				return null;
			}
			int[] ctct = getSome(aItm, 4, 4);
			units[y] = new DiskUnit(ctct);
			if (sts == 040) {
				break;
			}
			if (sts != 060) {
				// TODO: support multi-volume and overflow
				error = "Unsupported allocation";
				return null;
			}
			ctri = getSome(aItm, 12, 4);
		}
		if (type == 1) {
			return new SequentialFile(dsk, unit, nItm,
					desc[0], desc[1], desc[4], desc[3],
					units);
		} else {
			// TODO: support other file organizations
			error = "Unsupported file organization";
			return null;
		}
	}

	// Get one or more 2-char integers from a buffer.
	public int[] getSome(byte[] buf, int start, int num) {
		int z = start;
		int[] ints = new int[num];
		for (int x = 0; x < num; ++x) {
			ints[x] = (buf[z++] & 077) << 6;
			ints[x] |= (buf[z++] & 077);
		}
		return ints;
	}

	// Put one or more 2-char integers into a buffer.
	public void putSome(int[] ints, byte[] buf, int start) {
		int z = start;
		for (int x = 0; x < ints.length; ++x) {
			buf[z++] = (byte)((ints[x] >> 6) & 077);
			buf[z++] = (byte)(ints[x] & 077);
		}
	}
	public void putOne(int val, byte[] buf, int start) {
		int z = start;
		buf[z++] = (byte)((val >> 6) & 077);
		buf[z++] = (byte)(val & 077);
	}

	static public void initVOL(byte[] rec, byte[] name, byte[] snum) {
		Arrays.fill(rec, (byte)0);
		System.arraycopy(_1VOL, 0, rec, 0, _1VOL.length);
		System.arraycopy(name, 0, rec, 5, 6);
		System.arraycopy(snum, 0, rec, 12, 6);
		rec[11] = 013; // TODO: this is model 274? need 278?
		// TODO: *VOLNAMES* starting cyl/trk?
	}

	static public void setEOD(byte[] var, int start) {
		System.arraycopy(_EOD_, 0, var, start, _EOD_.length);
	}

	static public boolean isEOD(byte[] var, int start) {
		return compare(_EOD_, var, start);
	}

	static public boolean compare(byte[] con, byte[] var, int start) {
		if (con.length + start > var.length) {
			return false;
		}
		for (int x = 0; x < con.length; ++x) {
			if (con[x] != var[x + start]) {
				return false;
			}
		}
		return true;
	}
}
