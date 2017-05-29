// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.awt.Rectangle;

public class DiskUnit {
	public int sCyl;
	public int sTrk;
	public int eCyl;
	public int eTrk;
	public Rectangle rect;

	public DiskUnit(int sCyl, int sTrk, int eCyl, int eTrk) {
		this.sCyl = sCyl;
		this.sTrk = sTrk;
		this.eCyl = eCyl;
		this.eTrk = eTrk;
		rect = new Rectangle(sCyl, sTrk,
			(eCyl - sCyl + 1), (eTrk - sTrk + 1));
	}

	public DiskUnit(int[] ctct) {
		sCyl = ctct[0];
		sTrk = ctct[1];
		eCyl = ctct[2];
		eTrk = ctct[3];
		rect = new Rectangle(sCyl, sTrk,
			(eCyl - sCyl + 1), (eTrk - sTrk + 1));
	}

	public int[] get() {
		int[] ints = new int[4];
		ints[0] = sCyl;
		ints[1] = sTrk;
		ints[2] = eCyl;
		ints[3] = eTrk;
		return ints;
	}

	public boolean intersects(DiskUnit other) {
		return rect.intersects(other.rect);
	}
}
