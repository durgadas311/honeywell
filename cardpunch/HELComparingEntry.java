// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class HELComparingEntry extends ProgStart {
	int punch;
public char chr;

	public HELComparingEntry() {
		super(true);
	}

	@Override
	public void putCol(int p, char c) {
		punch = p;
	}

	// blank == 0;
	// special (zones 12, 11, 0, -) ([12],[11],[0][1] special cases)
	// alpha (zones 12, 11, 0)
	// numeric (no zone)
	private int xlat(int p) {
		int n = 0;
		int c = 0;
		if (p == 0x0000) return 0;
		if (p == 0x0800) return (0 << 4) + 16;
		if (p == 0x0400) return (1 << 4) + 16;
		if (p == 0x0300) return (2 << 4) + 1;
		// 'c' < 0 if no zone punch... else 0-2
		c += 11 - Integer.numberOfTrailingZeros(p & 0x0e00);
		if (c < 0) c = 3;
		if ((p & 0x0002) != 0 && (p & 0x00fc) != 0) { // [8][2-7]
			n = 8 + (9 - Integer.numberOfTrailingZeros(p & 0x00fc));
		} else {
			c += 4;
			// 'n' < 0 if no num punch... else 1-9
			n = 9 - Integer.numberOfTrailingZeros(p & 0x01ff);
			if (n < 0) n = 16; //
		}
		return (c << 4) + n;
	}

	// Order is: BLANK, special, A-Z, 0-9
	// Returns 0 if equal, -1 if this < 'ent', +1 if this > 'ent'
	public int compare(HELComparingEntry ent) {
		if (punch == ent.punch) return 0;
		if (xlat(punch) < xlat(ent.punch)) return -1;
		return 1;
	}
}
