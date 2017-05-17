// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;

public class DiskUnit {
	public int sCyl;
	public int sTrk;
	public int eCyl;
	public int eTrk;

	public DiskUnit(int sCyl, int sTrk, int eCyl, int eTrk) {
		this.sCyl = sCyl;
		this.sTrk = sTrk;
		this.eCyl = eCyl;
		this.eTrk = eTrk;
	}

	public DiskUnit(int[] ctct) {
		sCyl = ctct[0];
		sTrk = ctct[1];
		eCyl = ctct[2];
		eTrk = ctct[3];
	}

	public int[] get() {
		int[] ints = new int[4];
		ints[0] = sCyl;
		ints[1] = sTrk;
		ints[2] = eCyl;
		ints[3] = eTrk;
		return ints;
	}
}
