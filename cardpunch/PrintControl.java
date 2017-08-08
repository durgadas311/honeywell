// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Vector;

public class PrintControl {
	Vector<ProgStart> suppress;
	Vector<ProgStart> single;
	Vector<ProgStart> dubble;
	Vector<ProgStart> triple;

	public PrintControl() {
		suppress = new Vector<ProgStart>();
		single = new Vector<ProgStart>();
		dubble = new Vector<ProgStart>();
		triple = new Vector<ProgStart>();
	}

	public void addSuppress(ProgStart ps) {
		suppress.add(ps);
	}

	public void addSingle(ProgStart ps) {
		single.add(ps);
	}

	public void addDouble(ProgStart ps) {
		dubble.add(ps);
	}

	public void addTriple(ProgStart ps) {
		triple.add(ps);
	}

	public boolean isSuppress() {
		for (ProgStart ps : suppress) {
			if (ps.is()) {
				return true;
			}
		}
		return false;
	}

	public boolean isSingle() {
		for (ProgStart ps : single) {
			if (ps.is()) {
				return true;
			}
		}
		return false;
	}

	public boolean isDouble() {
		for (ProgStart ps : dubble) {
			if (ps.is()) {
				return true;
			}
		}
		return false;
	}

	public boolean isTriple() {
		for (ProgStart ps : triple) {
			if (ps.is()) {
				return true;
			}
		}
		return false;
	}
}
