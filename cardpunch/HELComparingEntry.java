// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class HELComparingEntry extends ProgStart {
	int punch = 0;
public char chr = ' ';

	public HELComparingEntry() {
		super(true);
	}

	@Override
	public void putCol(int p, char c) {
		punch = p;
chr = c;
	}

	// blank == 0;
	// special (zones 12, 11, 0, -) ([12],[11],[0][1] special cases)
	// alpha (zones 12, 11, 0)
	// numeric (no zone)
	// TODO: 087 ZONE switch enables zone punch detection.
	// TODO: 085 only detects digit 1-9 punches?
	private int xlat(boolean mode, int p) {
		int n = 0;
		int c = 0;
		if (!mode) {
			p &= 0x01ff; // digit 1-9 only
		}
		if (p == 0x0000) return 0;
		if (p == 0x0800) return (0 << 4) + 16;
		if (p == 0x0400) return (1 << 4) + 16;
		if (p == 0x0300) return (2 << 4) + 1;
		// 'c' < 0 if no zone punch... else 0-2
		c += 11 - Integer.numberOfTrailingZeros(p & 0x0e00);
		// for 'R' c = 0...
		// for 'X' c = 1...
		// for '0' c = 2...
		if (c < 0) c = 3;
		if ((p & 0x0002) != 0 && (p & 0x00fc) != 0) { // [8][2-7]
			n = 8 + (9 - Integer.numberOfTrailingZeros(p & 0x00fc));
		} else {
			c += 4;
			// 'n' < 0 if no num punch... else 1-9
			n = 9 - Integer.numberOfTrailingZeros(p & 0x01ff);
			if (n < 0) n = 16;	// '0' is c=6, n=16 = 0x70
						// '1' is c=7, n=1 = 0x71
		}
		return (c << 4) + n;
	}

	// Order is: BLANK, special, A-Z, 0-9
	// Returns 0 if equal, -1 if this < 'ent', +1 if this > 'ent'
	public int compare(boolean mode, HELComparingEntry ent) {
		if (punch == ent.punch) return 0;
		int c1 = xlat(mode, punch);
		int c2 = xlat(mode, ent.punch);
		if (c1 == c2) return 0;
		if (c1 < c2) return -1;
		return 1;
	}
}
