// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class Comparator {
	int width;
	ComparingEntry[] ents1;
	ComparingEntry[] ents2;
	ComparingExit[] xits;	// could be simple list...

	// Wiring:
	// COMP EXIT -> PROG START (MI/IN/MA) -> TOTAL PROG (1/2/3) -+
	//                                                           +-> COUNTER TOTAL
	// Properties:                                  FINAL TOTAL -+
	// [counter] total=minor
	// [counter] total=inter
	// [counter] total=major
	// [counter] total=final
	// [compare] start=minor
	// [compare] start=inter
	// [compare] start=major
	//
	// c[1-9][0-9]* = <src1> <src2> start={major|inter|minor}

	public Comparator(int wid) {
		super();
		width = wid;
		ents1 = new ComparingEntry[wid];
		ents2 = new ComparingEntry[wid];
		xits = new ComparingExit[wid];
	}

	private void setEntry(ComparingEntry[] ents, int pos, ComparingEntry ent) {
		if (pos < 0 || pos >= width) {
			return;
		}
		// No daisy-chaining allowed here...
		ents[pos] = ent;
	}

	public void setEntryA(int pos, ComparingEntry ent) {
		setEntry(ents1, pos, ent);
	}

	public void setEntryB(int pos, ComparingEntry ent) {
		setEntry(ents2, pos, ent);
	}

	public void setExit(int pos, int wid, ProgStart srt) {
		if (pos < 0 || pos + wid > width) {
			return;
		}
		// TODO: check overlaps, conflicts?
		xits[pos] = new ComparingExit(pos, wid, srt);
	}

	private void compare(ComparingExit xit) {
		if (xit == null) return;
		int n = xit.width();
		int c = xit.position();
		while (n > 0) {
			if (!ents1[c].compare(ents2[c])) {
				xit.start();
				return;
			}
			++c;
			--n;
		}
	}

	public void processExits() {
		for (int x = 0; x < xits.length; ++x) {
			compare(xits[x]);
		}
	}
}
