// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;

public class DiskVolume {
	private static final byte[] _1VOL = new byte[]{ 001, 065, 046, 043, 015}; // 1VOL_
	RandomRecordIO dsk = null;
	int unit;
	boolean initialized = false;
	byte[] name;

	// Each operation must be atomic, i.e. bounded by dsk.begin()/dsk.end().
	// This is because the peripheral cannot operate on multiple units
	// at the same time (single buffer, etc).
	public DiskVolume(RandomRecordIO dsk, int unit) {
		if (!dsk.begin(unit)) {
			return;
		}
		// "mount" the volume.
		// Mark everything OK, although might not be initialized
		this.dsk = dsk;
		this.unit= unit;
		name = new byte[6];
		if (!dsk.seekRecord(0, 1, 0)) {
			dsk.end();
			return;
		}
		byte[] r = dsk.readRecord();
		if (!compare(_1VOL, r, 0)) {
			// Not previously initialized
			dsk.end();
			return;
		}
		// TODO: trim trailing blanks?
		System.arraycopy(r, 5, name, 0, 6);
		initialized = true;
		dsk.end();
	}

	public byte[] getName() {
		return name;
	}

	public void initVolume(byte[] volnam) {
		if (!dsk.begin(unit)) {
			return;
		}
		int n = volnam.length;
		if (n > name.length) {
			n = name.length;
		}
		System.arraycopy(volnam, 5, name, 0, n);
		while (n < name.length) {
			name[n++] = 015;
		}
		for (int cyl = 0; cyl < 203; ++cyl) {
			for (int trk = 0; trk < 20; ++trk) {
				dsk.initTrack(cyl, trk, 250);
			}
		}
		byte[] rec = new byte[250];
		Arrays.fill(rec, (byte)0);
		System.arraycopy(_1VOL, 0, rec, 0, _1VOL.length);
		System.arraycopy(name, 0, rec, 5, 6);
		rec[11] = 013; // always 203 cylinders (but 20 tracks)
		dsk.seekRecord(0, 1, 0);
		dsk.writeRecord(rec, 0, -1);
		initialized = true;
		dsk.end();
	}

	public void initTrack(int cyl, int trk, int reclen) {
		// Do not allow overwrite of system area
		if (cyl < 1) {
			return;
		}
		if (!dsk.begin(unit)) {
			return;
		}
		dsk.initTrack(cyl, trk, reclen);
		dsk.end();
	}

	public void unmount() {
		// must commit all buffered data... is there any?
		// TODO: prevent further access - or allow re-mount
		dsk = null;
	}

	private boolean compare(byte[] con, byte[] var, int start) {
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
