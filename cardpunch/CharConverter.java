// Copyright (c) 2010,2014 Douglas Miller

import java.util.Arrays;

class CharConverter {

	private byte[] xlate_pun;
	private String[] spcl_pun;
	private short[] xlate_char;
	private byte[] xlate_hw;
	private byte[] hw2lp;
	private byte[] ascii2hw;
	private byte[] bb = new byte[1];

	private void setup_xlate(CardPunchOptions opts) {
		// This defines how punch codes (condensed) are printed on card.
		xlate_pun = new byte[128];
		Arrays.fill(xlate_pun, (byte)0);
		xlate_pun[0x00] = ' ';
		xlate_pun[0x09] = '9';
		xlate_pun[0x08] = '8';
		xlate_pun[0x07] = '7';
		xlate_pun[0x06] = '6';
		xlate_pun[0x05] = '5';
		xlate_pun[0x04] = '4';
		xlate_pun[0x03] = '3';
		xlate_pun[0x02] = '2';
		xlate_pun[0x01] = '1';
		xlate_pun[0x10] = '0';
		xlate_pun[0x20] = '-';
		xlate_pun[0x40] = '&';

		xlate_pun[0x49] = 'I';
		xlate_pun[0x48] = 'H';
		xlate_pun[0x47] = 'G';
		xlate_pun[0x46] = 'F';
		xlate_pun[0x45] = 'E';
		xlate_pun[0x44] = 'D';
		xlate_pun[0x43] = 'C';
		xlate_pun[0x42] = 'B';
		xlate_pun[0x41] = 'A';

		xlate_pun[0x29] = 'R';
		xlate_pun[0x28] = 'Q';
		xlate_pun[0x27] = 'P';
		xlate_pun[0x26] = 'O';
		xlate_pun[0x25] = 'N';
		xlate_pun[0x24] = 'M';
		xlate_pun[0x23] = 'L';
		xlate_pun[0x22] = 'K';
		xlate_pun[0x21] = 'J';
		xlate_pun[0x30] = '\005';	// hw 057/040 ('!') [11][0]

		xlate_pun[0x19] = 'Z';
		xlate_pun[0x18] = 'Y';
		xlate_pun[0x17] = 'X';
		xlate_pun[0x16] = 'W';
		xlate_pun[0x15] = 'V';
		xlate_pun[0x14] = 'U';
		xlate_pun[0x13] = 'T';
		xlate_pun[0x12] = 'S';
		xlate_pun[0x11] = '/';

		xlate_pun[0x0a] = ':';
		xlate_pun[0x0b] = '#';
		xlate_pun[0x0c] = '@';
		xlate_pun[0x0d] = '\'';
		xlate_pun[0x0e] = '=';
		xlate_pun[0x0f] = '"';

		xlate_pun[0x50] = '\003';	// hw 037 ('?') [12][0]
		xlate_pun[0x4a] = '\001';	// cent
		xlate_pun[0x4b] = '.';
		xlate_pun[0x4c] = '<';
		xlate_pun[0x4d] = '(';
		xlate_pun[0x4e] = '+';
		xlate_pun[0x4f] = '|';		// Not HW

		xlate_pun[0x2a] = '!';
		xlate_pun[0x2b] = '$';
		xlate_pun[0x2c] = '*';
		xlate_pun[0x2d] = ')';
		xlate_pun[0x2e] = ';';
		xlate_pun[0x2f] = '\002';	// broken bar (not HW)

		xlate_pun[0x1a] = 0;	// No keypunch symbol - no HW equiv
		xlate_pun[0x1b] = ',';
		xlate_pun[0x1c] = '%';
		xlate_pun[0x1d] = '_';
		xlate_pun[0x1e] = '>';
		xlate_pun[0x1f] = '?';
		if (opts != null && opts.ibm026) {
			xlate_pun[0x4a] = 0;
			xlate_pun[0x4c] = '\004'; // op-loz
			xlate_pun[0x4d] = 0;
			xlate_pun[0x4e] = 0;
			xlate_pun[0x4f] = 0;
			xlate_pun[0x2a] = 0;
			xlate_pun[0x2d] = 0;
			xlate_pun[0x2e] = 0;
			xlate_pun[0x2f] = 0;
			xlate_pun[0x1d] = 0;
			xlate_pun[0x1e] = 0;
			xlate_pun[0x1f] = 0;
			xlate_pun[0x0a] = 0;
			xlate_pun[0x0d] = 0;
			xlate_pun[0x0e] = 0;
			xlate_pun[0x0f] = 0;
			if (opts.fortran) {
				xlate_pun[0x0b] = '=';
				xlate_pun[0x0c] = '\'';
				xlate_pun[0x1c] = '(';
				xlate_pun[0x4c] = ')';
				xlate_pun[0x40] = '+';
			}
		}

		spcl_pun = new String[10];
		spcl_pun['\001'] = "\u00a2";	// cent
		spcl_pun['\002'] = "\u00ac";	// broken bar
		spcl_pun['\003'] = "\u00bf";	// undef (inverted ?)
		spcl_pun['\004'] = "\u00a7";	// no sym - section
		spcl_pun['\005'] = "\u00bd";	// hw alt 057 - 1/2
		spcl_pun['\006'] = "\u25a0";	// solid loz
		spcl_pun['\007'] = "\u2260";	// not-eq
		spcl_pun['\010'] = "\u00a9";	// c/r (does not exist, use (c))
		spcl_pun['\011'] = "\u25a1";	// open loz

		// This defines how keystrokes convert to punch codes.
		xlate_char = new short[128];
		Arrays.fill(xlate_char, (short)0x1000);
		xlate_char[' '] = (short)0x800;
		xlate_char['&'] = (short)0x800;
		xlate_char['-'] = (short)0x400;
		xlate_char['0'] = (short)0x200;
		xlate_char['1'] = (short)0x100;
		xlate_char['2'] = (short)0x080;
		xlate_char['3'] = (short)0x040;
		xlate_char['4'] = (short)0x020;
		xlate_char['5'] = (short)0x010;
		xlate_char['6'] = (short)0x008;
		xlate_char['7'] = (short)0x004;
		xlate_char['8'] = (short)0x002;
		xlate_char['9'] = (short)0x001;

		xlate_char['A'] = (short)0x900;
		xlate_char['B'] = (short)0x880;
		xlate_char['C'] = (short)0x840;
		xlate_char['D'] = (short)0x820;
		xlate_char['E'] = (short)0x810;
		xlate_char['F'] = (short)0x808;
		xlate_char['G'] = (short)0x804;
		xlate_char['H'] = (short)0x802;
		xlate_char['I'] = (short)0x801;

		xlate_char['J'] = (short)0x500;
		xlate_char['K'] = (short)0x480;
		xlate_char['L'] = (short)0x440;
		xlate_char['M'] = (short)0x420;
		xlate_char['N'] = (short)0x410;
		xlate_char['O'] = (short)0x408;
		xlate_char['P'] = (short)0x404;
		xlate_char['Q'] = (short)0x402;
		xlate_char['R'] = (short)0x401;

		xlate_char['/'] = (short)0x300;
		xlate_char['S'] = (short)0x280;
		xlate_char['T'] = (short)0x240;
		xlate_char['U'] = (short)0x220;
		xlate_char['V'] = (short)0x210;
		xlate_char['W'] = (short)0x208;
		xlate_char['X'] = (short)0x204;
		xlate_char['Y'] = (short)0x202;
		xlate_char['Z'] = (short)0x201;

		xlate_char[':'] = (short)0x082;
		xlate_char['#'] = (short)0x042;
		xlate_char['@'] = (short)0x022;
		xlate_char['\''] = (short)0x012;
		xlate_char['='] = (short)0x00a;
		xlate_char['"'] = (short)0x006;

		xlate_char['\003'] = (short)0xa00;	// hw 020 ('+')
		xlate_char['\001'] = (short)0x882;	// cent
		xlate_char['.'] = (short)0x842;
		xlate_char['<'] = (short)0x822;
		xlate_char['('] = (short)0x812;
		xlate_char['+'] = (short)0x80a;
		xlate_char['|'] = (short)0x806;		// Not HW

		xlate_char['\005'] = (short)0x600;	// hw 040 ('-')
		xlate_char['!'] = (short)0x482;
		xlate_char['$'] = (short)0x442;
		xlate_char['*'] = (short)0x422;
		xlate_char[')'] = (short)0x412;
		xlate_char[';'] = (short)0x40a;
		xlate_char['\002'] = (short)0x406;	// broken bar

		xlate_char['\004'] = (short)0x282;	// No keypunch symbol
		xlate_char[','] = (short)0x242;
		xlate_char['%'] = (short)0x222;
		xlate_char['_'] = (short)0x212;
		xlate_char['>'] = (short)0x20a;
		xlate_char['?'] = (short)0x206;
		if (opts != null && opts.ibm026) {
			xlate_char['<'] = (short)0x1000;
			xlate_char['\001'] = (short)0x1000;
			xlate_char['|'] = (short)0x1000;
			xlate_char['!'] = (short)0x1000;
			xlate_char[';'] = (short)0x1000;
			xlate_char['\002'] = (short)0x1000;	// broken bar
			xlate_char['_'] = (short)0x1000;
			xlate_char['>'] = (short)0x1000;
			xlate_char['?'] = (short)0x1000;
			xlate_char[':'] = (short)0x1000;
			xlate_char['"'] = (short)0x1000;
			if (opts.fortran) {
				xlate_char['&'] = (short)0x1000;
				xlate_char['+'] = (short)0x800;
				xlate_char[')'] = (short)0x822;
				xlate_char['%'] = (short)0x1000;
				xlate_char['('] = (short)0x222;
				xlate_char['#'] = (short)0x1000;
				xlate_char['='] = (short)0x042;
				xlate_char['@'] = (short)0x1000;
				xlate_char['\''] = (short)0x022;
			} else {
				xlate_char['('] = (short)0x1000;
				xlate_char[')'] = (short)0x1000;
				xlate_char['+'] = (short)0x1000;
				xlate_char['^'] = (short)0x822;
				xlate_char['\''] = (short)0x1000;
				xlate_char['='] = (short)0x1000;
			}
		}

		// This defines how (condensed) punch codes convert to HW cpu codes.
		xlate_hw = new byte[128];
		Arrays.fill(xlate_hw, (byte)0200);
		xlate_hw[0x10] = (byte)000;
		xlate_hw[0x01] = (byte)001;
		xlate_hw[0x02] = (byte)002;
		xlate_hw[0x03] = (byte)003;
		xlate_hw[0x04] = (byte)004;
		xlate_hw[0x05] = (byte)005;
		xlate_hw[0x06] = (byte)006;
		xlate_hw[0x07] = (byte)007;
		xlate_hw[0x08] = (byte)010;
		xlate_hw[0x09] = (byte)011;

		xlate_hw[0x41] = (byte)021;
		xlate_hw[0x42] = (byte)022;
		xlate_hw[0x43] = (byte)023;
		xlate_hw[0x44] = (byte)024;
		xlate_hw[0x45] = (byte)025;
		xlate_hw[0x46] = (byte)026;
		xlate_hw[0x47] = (byte)027;
		xlate_hw[0x48] = (byte)030;
		xlate_hw[0x49] = (byte)031;

		xlate_hw[0x21] = (byte)041;
		xlate_hw[0x22] = (byte)042;
		xlate_hw[0x23] = (byte)043;
		xlate_hw[0x24] = (byte)044;
		xlate_hw[0x25] = (byte)045;
		xlate_hw[0x26] = (byte)046;
		xlate_hw[0x27] = (byte)047;
		xlate_hw[0x28] = (byte)050;
		xlate_hw[0x29] = (byte)051;

		xlate_hw[0x11] = (byte)061;
		xlate_hw[0x12] = (byte)062;
		xlate_hw[0x13] = (byte)063;
		xlate_hw[0x14] = (byte)064;
		xlate_hw[0x15] = (byte)065;
		xlate_hw[0x16] = (byte)066;
		xlate_hw[0x17] = (byte)067;
		xlate_hw[0x18] = (byte)070;
		xlate_hw[0x19] = (byte)071;

		xlate_hw[0x0a] = (byte)012;	// [8][2]
		xlate_hw[0x0b] = (byte)013;	// [8][3]
		xlate_hw[0x0c] = (byte)014;	// [8][4]
		xlate_hw[0x0d] = (byte)060;	// [8][5]
		xlate_hw[0x0e] = (byte)016;	// [8][6]
		xlate_hw[0x0f] = (byte)017;	// [8][7]

		xlate_hw[0x40] = (byte)0137;	// [12] = '?' or hw 020 '+'
		xlate_hw[0x50] = (byte)0120;	// [12][0] = '+' or hw 037 ('?')
		xlate_hw[0x4a] = (byte)032;	// [12][8][2]
		xlate_hw[0x4b] = (byte)033;	// [12][8][3]
		xlate_hw[0x4c] = (byte)034;	// [12][8][4]
		xlate_hw[0x4d] = (byte)035;	// [12][8][5]
		xlate_hw[0x4e] = (byte)036;	// [12][8][6]
		xlate_hw[0x4f] = (byte)0200;	// [12][8][7] Not HW

		xlate_hw[0x20] = (byte)0157;	// [11] = '!' or hw 040 '-'
		xlate_hw[0x30] = (byte)0140;	// [11][0] = '-' or hw 057 ('!')
		xlate_hw[0x2a] = (byte)052;	// [11][8][2]
		xlate_hw[0x2b] = (byte)053;	// [11][8][3]
		xlate_hw[0x2c] = (byte)054;	// [11][8][4]
		xlate_hw[0x2d] = (byte)055;	// [11][8][5]
		xlate_hw[0x2e] = (byte)056;	// [11][8][6]
		xlate_hw[0x2f] = (byte)0201;	// [11][8][7] Not HW (broken bar)

		xlate_hw[0x1a] = (byte)072;	// [0][8][2] No keypunch symbol, '@'
		xlate_hw[0x1b] = (byte)073;	// [0][8][3]
		xlate_hw[0x1c] = (byte)074;	// [0][8][4]
		xlate_hw[0x1d] = (byte)075;	// [0][8][5]
		xlate_hw[0x1e] = (byte)076;	// [0][8][6]
		xlate_hw[0x1f] = (byte)077;	// [0][8][7]

		// These convert HW cpu codes to HW LinePrinter codes (ASCII equiv glyph)
		hw2lp = new byte[64];
		Arrays.fill(hw2lp, (byte)0);
		hw2lp[000] = '0';
		hw2lp[001] = '1';
		hw2lp[002] = '2';
		hw2lp[003] = '3';
		hw2lp[004] = '4';
		hw2lp[005] = '5';
		hw2lp[006] = '6';
		hw2lp[007] = '7';
		hw2lp[010] = '8';
		hw2lp[011] = '9';

		hw2lp[021] = 'A';
		hw2lp[022] = 'B';
		hw2lp[023] = 'C';
		hw2lp[024] = 'D';
		hw2lp[025] = 'E';
		hw2lp[026] = 'F';
		hw2lp[027] = 'G';
		hw2lp[030] = 'H';
		hw2lp[031] = 'I';
		hw2lp[041] = 'J';
		hw2lp[042] = 'K';
		hw2lp[043] = 'L';
		hw2lp[044] = 'M';
		hw2lp[045] = 'N';
		hw2lp[046] = 'O';
		hw2lp[047] = 'P';
		hw2lp[050] = 'Q';
		hw2lp[051] = 'R';
		hw2lp[062] = 'S';
		hw2lp[063] = 'T';
		hw2lp[064] = 'U';
		hw2lp[065] = 'V';
		hw2lp[066] = 'W';
		hw2lp[067] = 'X';
		hw2lp[070] = 'Y';
		hw2lp[071] = 'Z';

		hw2lp[012] = '\'';
		hw2lp[013] = '=';
		hw2lp[014] = ':';
		hw2lp[015] = ' ';
		hw2lp[016] = '>';
		hw2lp[017] = '&';
		hw2lp[020] = '+';

		hw2lp[032] = ';';
		hw2lp[033] = '.';
		hw2lp[034] = ')';
		hw2lp[035] = '%';
		hw2lp[036] = '\006';	// solid lozenge
		hw2lp[037] = '?';
		hw2lp[040] = '-';

		hw2lp[052] = '#';
		hw2lp[053] = '$';
		hw2lp[054] = '*';
		hw2lp[055] = '"';
		hw2lp[056] = '\007';	// not-equal
		hw2lp[057] = '!';	// 1/2 on older printers
		hw2lp[060] = '<';
		hw2lp[061] = '/';

		hw2lp[072] = '@';
		hw2lp[073] = ',';
		hw2lp[074] = '(';
		hw2lp[075] = '\010';	// c/r
		hw2lp[076] = '\011';	// open lozenge
		hw2lp[077] = '\001';	// cent

		// These convert ASCII to HW cpu codes
		ascii2hw = new byte[128];
		Arrays.fill(ascii2hw, (byte)0100);
		ascii2hw['0'] = 000;
		ascii2hw['1'] = 001;
		ascii2hw['2'] = 002;
		ascii2hw['3'] = 003;
		ascii2hw['4'] = 004;
		ascii2hw['5'] = 005;
		ascii2hw['6'] = 006;
		ascii2hw['7'] = 007;
		ascii2hw['8'] = 010;
		ascii2hw['9'] = 011;

		ascii2hw['A'] = 021;
		ascii2hw['B'] = 022;
		ascii2hw['C'] = 023;
		ascii2hw['D'] = 024;
		ascii2hw['E'] = 025;
		ascii2hw['F'] = 026;
		ascii2hw['G'] = 027;
		ascii2hw['H'] = 030;
		ascii2hw['I'] = 031;
		ascii2hw['J'] = 041;
		ascii2hw['K'] = 042;
		ascii2hw['L'] = 043;
		ascii2hw['M'] = 044;
		ascii2hw['N'] = 045;
		ascii2hw['O'] = 046;
		ascii2hw['P'] = 047;
		ascii2hw['Q'] = 050;
		ascii2hw['R'] = 051;
		ascii2hw['S'] = 062;
		ascii2hw['T'] = 063;
		ascii2hw['U'] = 064;
		ascii2hw['V'] = 065;
		ascii2hw['W'] = 066;
		ascii2hw['X'] = 067;
		ascii2hw['Y'] = 070;
		ascii2hw['Z'] = 071;

		ascii2hw['\''] = 012;
		ascii2hw['='] = 013;
		ascii2hw[':'] = 014;
		ascii2hw[' '] = 015;
		ascii2hw['>'] = 016;
		ascii2hw['&'] = 017;
		ascii2hw['+'] = 020;

		ascii2hw[';'] = 032;
		ascii2hw['.'] = 033;
		ascii2hw[')'] = 034;
		ascii2hw['%'] = 035;
		ascii2hw['\006'] = 036;	// solid lozenge
		ascii2hw['?'] = 037;
		ascii2hw['-'] = 040;

		ascii2hw['#'] = 052;
		ascii2hw['$'] = 053;
		ascii2hw['*'] = 054;
		ascii2hw['"'] = 055;
		ascii2hw['\007'] = 056;	// not-equal
		ascii2hw['!'] = 057;	// 1/2 on older printers
		ascii2hw['<'] = 060;
		ascii2hw['/'] = 061;

		ascii2hw['@'] = 072;
		ascii2hw[','] = 073;
		ascii2hw['('] = 074;
		ascii2hw['\010'] = 075;	// c/r
		ascii2hw['\011'] = 076;	// open lozenge
		ascii2hw['\001'] = 077;	// cent
	}

	public static final String hwAsciiSup = "^[]~\\";
	public static final String hwAsciiRep = "\001\011\006\010\007";

	public CharConverter() {
		setup_xlate(null);
	}

	public CharConverter(CardPunchOptions opts) {
		setup_xlate(opts);
	}

	// Converts Hollerith-based punch code to compact code (NOT binary punch!).
	// Returns byte "0yxznnnn" where 'y' is zone 12, 'x' is zone 11,
	// 'z' is zone zero, 'nnnn' = 1-9 if exactly one punch 1-9,
	// 'nnnn' = 10-15 for punch [8][2-7], 'nnnn' = 0 if none (zone only).
	// Returns 1xxxxxxx for invalid punch codes.
	public byte punToCode(int pun) {
		// some HW punch codes have 2 zone punches ([12][0] and [11][0]).
		byte b = (byte)((pun & 0x0e00) >> 5); // -yxz----
		int p = (pun & 0x01ff);
		if (p == 0) { // zone-only
			return b;
		}
		if ((p & (p - 1)) == 0) { // only one punch... simple
			int n = Integer.numberOfTrailingZeros(p); // 0 means 9...
			b |= (byte)(9 - n);	// 1-9 for punches 1-9
			return b;
		}
		if ((p & 0x0103) == 0x0002) { // [8]+ but no [1] or [9]
			p &= ~0x0002;
			if ((p & (p - 1)) == 0) { // only one other punch... valid
				int n = Integer.numberOfTrailingZeros(p);
				// 2 means 7, 3 means 6,...
				// want 2 => 15, 3 => 14, ...
				b |= (byte)(17 - n);
				return b;
			}
		}
		return (byte)0x80;
	}

	// This is used to print the punch code only - not for general conversion
	public String punToAscii(int pun) {
		byte b = punToCode(pun);
		if (b == 0) { // might be faster path
			return " ";
		}
		if ((b & 0x80) != 0) {
			return null;
		}
		bb[0] = xlate_pun[b];
		if (bb[0] == 0) {
			return null;
		}
		if (bb[0] < ' ') {
			return spcl_pun[bb[0]];
		}
		return new String(bb);
	}

	// This is used to convert keystrokes to punch codes only
	public int asciiToPun(int ch) {
		if (ch < ' ' || ch > '~') {
			return -1;
		}
		if (ch == ' ') {
			return 0;
		}
		int p = xlate_char[ch];
		if (p == 0x1000) {
			return -1;
		}
		return p;
	}

	// HW2000 "cpu code" from punch code
	public int punToHW(int pun, boolean alt) {
		byte b = punToCode(pun);
		if ((b & 0x80) != 0) {
			return -1;
		}
		int c = xlate_hw[b] & 0x0ff;
		if (c >= 0200) {
			return -1;
		}
		if (c >= 0100) {
			if (alt) {
				c ^= 017;
			}
			c &= 077;
		}
		return c;
	}

	// HW2000 "cpu" code to LinePrinter glyph
	public String hwToLP(byte c) {
		bb[0] = hw2lp[c];
		if (bb[0] == 0) {
			return null;
		}
		if (bb[0] < ' ') {
			return spcl_pun[bb[0]];
		}
		return new String(bb);
	}

	// ASCII to HW2000 "cpu" code
	public byte asciiToHw(byte c) {
		byte b = ascii2hw[c];
		return b;
	}
}
