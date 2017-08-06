// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public abstract class ProgExit {
	ProgExit _next;

	public ProgExit() {
		_next = null;
	}

	// TODO: reject duplicates
	public void setNext(ProgExit ext) { _next = ext; }

	abstract void processExits();
}
