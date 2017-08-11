// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Vector;

public class Comparator {
	int width;
	ComparingEntry[] ents1; // there can be only one... each
	ComparingEntry[] ents2; // there can be only one... each
	Vector<ComparingExit> xits;

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
		width = wid;
		ents1 = new ComparingEntry[wid];
		ents2 = new ComparingEntry[wid];
		xits = new Vector<ComparingExit>();
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
		xits.add(new ComparingExit(pos, wid, srt));
	}

	private void compare(ComparingExit xit) {
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
		for (ComparingExit xit : xits) {
			compare(xit);
		}
	}
}
