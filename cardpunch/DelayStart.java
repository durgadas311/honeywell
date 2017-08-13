// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class DelayStart extends ProgStart {
	boolean flag;

	public DelayStart() {
		super(false);
		flag = false;
	}

	@Override
	public void set(boolean b) {
		if (bool == b) return;
		bool = b;
		if (b) return;
		trigger(flag); // send previous value along...
	}

	public void setFlag(boolean b) {
		flag = b;
	}
}
