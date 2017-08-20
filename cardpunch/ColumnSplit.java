// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class ColumnSplit {
	ColumnSplitComm comm;
	ColumnSplitZone xpun;
	ColumnSplitZone rpun;
	ColumnSplitDigit dpun;

	public ColumnSplit(int w, boolean r) {
		comm = new ColumnSplitComm(w);
		dpun = new ColumnSplitDigit(w);
		if (r) {
			rpun = new ColumnSplitZone(w, 0x800);
			xpun = new ColumnSplitZone(w, 0x400);
		} else {
			rpun = null;
			xpun = new ColumnSplitZone(w, 0xc00);
		}
		comm.setXD(rpun, xpun, dpun);
		dpun.setC(comm);
		xpun.setC(comm);
	}

	public ProgItem R() { return rpun; }
	public ProgItem X() { return xpun; }
	public ProgItem D() { return dpun; }
	public ProgItem C() { return comm; }

	public void commit() {
		comm.commit();
	}
}
