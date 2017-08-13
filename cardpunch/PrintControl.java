// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Vector;

public class PrintControl {
	ProgItem suppress;
	ProgItem single;
	ProgItem dubble;
	ProgItem triple;

	public PrintControl() {
		suppress = new SingleEntry();
		single = new SingleEntry();
		dubble = new SingleEntry();
		triple = new SingleEntry();
	}

	public void reset() {
		suppress.reset();
		single.reset();
		dubble.reset();
		triple.reset();
	}

	public ProgItem SS() { return suppress; }
	public ProgItem S1() { return single; }
	public ProgItem S2() { return dubble; }
	public ProgItem S3() { return triple; }
}
