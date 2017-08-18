// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class ColumnSplit {
	ColumnSplitComm comm;
	ColumnSplitZone xpun;
	ColumnSplitDigit dpun;

	public ColumnSplit(int w) {
		comm = new ColumnSplitComm(w);
		xpun = new ColumnSplitZone(w);
		dpun = new ColumnSplitDigit(w);
		comm.setXD(xpun, dpun);
		dpun.setC(comm);
		xpun.setC(comm);
	}

	public ProgItem X() { return xpun; }
	public ProgItem D() { return dpun; }
	public ProgItem C() { return comm; }

	public void commit() {
		comm.commit();
	}
}
