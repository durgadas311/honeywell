// Copyright (c) 2010,2014 Douglas Miller

import java.util.Arrays;

class CharConverter {

	private byte[] xlate_pun;
	private String[] spcl_pun;
	private short[] xlate_char;
	private byte[] xlate_hw;
	private byte[] hw2lp;
	private byte[] bb = new byte[1];

	private void setup_xlate(CardPunchOptions opts) {
		xlate_pun = new byte[4096];
		Arrays.fill(xlate_pun, (byte)0);
		xlate_pun[0x000] = ' ';
		xlate_pun[0x001] = '9';
		xlate_pun[0x002] = '8';
		xlate_pun[0x004] = '7';
		xlate_pun[0x008] = '6';
		xlate_pun[0x010] = '5';
		xlate_pun[0x020] = '4';
		xlate_pun[0x040] = '3';
		xlate_pun[0x080] = '2';
		xlate_pun[0x100] = '1';
		xlate_pun[0x200] = '0';
		xlate_pun[0x400] = '-';
		xlate_pun[0x800] = '&';

		xlate_pun[0x801] = 'I';
		xlate_pun[0x802] = 'H';
		xlate_pun[0x804] = 'G';
		xlate_pun[0x808] = 'F';
		xlate_pun[0x810] = 'E';
		xlate_pun[0x820] = 'D';
		xlate_pun[0x840] = 'C';
		xlate_pun[0x880] = 'B';
		xlate_pun[0x900] = 'A';
		xlate_pun[0xc00] = 'm';

		xlate_pun[0x401] = 'R';
		xlate_pun[0x402] = 'Q';
		xlate_pun[0x404] = 'P';
		xlate_pun[0x408] = 'O';
		xlate_pun[0x410] = 'N';
		xlate_pun[0x420] = 'M';
		xlate_pun[0x440] = 'L';
		xlate_pun[0x480] = 'K';
		xlate_pun[0x500] = 'J';
		xlate_pun[0x600] = '\005';		// hw 057 ('!')

		xlate_pun[0x201] = 'Z';
		xlate_pun[0x202] = 'Y';
		xlate_pun[0x204] = 'X';
		xlate_pun[0x208] = 'W';
		xlate_pun[0x210] = 'V';
		xlate_pun[0x220] = 'U';
		xlate_pun[0x240] = 'T';
		xlate_pun[0x280] = 'S';
		xlate_pun[0x300] = '/';

		xlate_pun[0x082] = ':';
		xlate_pun[0x042] = '#';
		xlate_pun[0x022] = '@';
		xlate_pun[0x012] = '\'';
		xlate_pun[0x00a] = '=';
		xlate_pun[0x006] = '"';

		xlate_pun[0xa00] = '\003';	// hw 037 ('?')
		xlate_pun[0x882] = '\001';	// cent
		xlate_pun[0x842] = '.';
		xlate_pun[0x822] = '<';
		xlate_pun[0x812] = '(';
		xlate_pun[0x80a] = '+';
		xlate_pun[0x806] = '|';		// Not HW

		xlate_pun[0x482] = '!';
		xlate_pun[0x442] = '$';
		xlate_pun[0x422] = '*';
		xlate_pun[0x412] = ')';
		xlate_pun[0x40a] = ';';
		xlate_pun[0x406] = '\002';	// broken bar (not HW)

		xlate_pun[0x282] = 0;	// No keypunch symbol
		xlate_pun[0x242] = ',';
		xlate_pun[0x222] = '%';
		xlate_pun[0x212] = '_';
		xlate_pun[0x20a] = '>';
		xlate_pun[0x206] = '?';
		if (opts.ibm026) {
			xlate_pun[0x822] = '\004'; // op-loz
			xlate_pun[0x882] = 0;
			xlate_pun[0x812] = 0;
			xlate_pun[0x806] = 0;
			xlate_pun[0x80a] = 0;
			xlate_pun[0x482] = 0;
			xlate_pun[0x412] = 0;
			xlate_pun[0x40a] = 0;
			xlate_pun[0x406] = 0;
			xlate_pun[0x212] = 0;
			xlate_pun[0x20a] = 0;
			xlate_pun[0x206] = 0;
			xlate_pun[0x082] = 0;
			xlate_pun[0x012] = 0;
			xlate_pun[0x00a] = 0;
			xlate_pun[0x006] = 0;
			if (opts.fortran) {
				xlate_pun[0x042] = '=';
				xlate_pun[0x022] = '\'';
				xlate_pun[0x222] = '(';
				xlate_pun[0x822] = ')';
				xlate_pun[0x800] = '+';
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
		if (opts.ibm026) {
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

		xlate_hw = new byte[128];
		Arrays.fill(xlate_hw, (byte)0200);
		xlate_hw['0'] = (byte)000;
		xlate_hw['1'] = (byte)001;
		xlate_hw['2'] = (byte)002;
		xlate_hw['3'] = (byte)003;
		xlate_hw['4'] = (byte)004;
		xlate_hw['5'] = (byte)005;
		xlate_hw['6'] = (byte)006;
		xlate_hw['7'] = (byte)007;
		xlate_hw['8'] = (byte)010;
		xlate_hw['9'] = (byte)011;

		xlate_hw['A'] = (byte)021;
		xlate_hw['B'] = (byte)022;
		xlate_hw['C'] = (byte)023;
		xlate_hw['D'] = (byte)024;
		xlate_hw['E'] = (byte)025;
		xlate_hw['F'] = (byte)026;
		xlate_hw['G'] = (byte)027;
		xlate_hw['H'] = (byte)030;
		xlate_hw['I'] = (byte)031;

		xlate_hw['J'] = (byte)041;
		xlate_hw['K'] = (byte)042;
		xlate_hw['L'] = (byte)043;
		xlate_hw['M'] = (byte)044;
		xlate_hw['N'] = (byte)045;
		xlate_hw['O'] = (byte)046;
		xlate_hw['P'] = (byte)047;
		xlate_hw['Q'] = (byte)050;
		xlate_hw['R'] = (byte)051;

		xlate_hw['/'] = (byte)061;
		xlate_hw['S'] = (byte)062;
		xlate_hw['T'] = (byte)063;
		xlate_hw['U'] = (byte)064;
		xlate_hw['V'] = (byte)065;
		xlate_hw['W'] = (byte)066;
		xlate_hw['X'] = (byte)067;
		xlate_hw['Y'] = (byte)070;
		xlate_hw['Z'] = (byte)071;

		xlate_hw[':'] = (byte)012;	// [8][2]
		xlate_hw['#'] = (byte)013;	// [8][3]
		xlate_hw['@'] = (byte)014;	// [8][4]
		xlate_hw['\''] = (byte)060;	// [8][5]
		xlate_hw['='] = (byte)016;	// [8][6]
		xlate_hw['"'] = (byte)017;	// [8][7]

		xlate_hw['\003'] = (byte)0120;	// [12][0] = '+' or hw 037 ('?')
		xlate_hw['\001'] = (byte)032;	// [12][8][2]
		xlate_hw['.'] = (byte)033;	// [12][8][3]
		xlate_hw['<'] = (byte)034;	// [12][8][4]
		xlate_hw['('] = (byte)035;	// [12][8][5]
		xlate_hw['+'] = (byte)036;	// [12][8][6]
		xlate_hw['&'] = (byte)0137;	// [12] = '?' or hw 020 '+'
		xlate_hw['|'] = (byte)0200;	// [12][8][7] Not HW

		xlate_hw['\005'] = (byte)0140;	// [11][0] = '-' or hw 057 ('!')
		xlate_hw['!'] = (byte)052;	// [11][8][2]
		xlate_hw['$'] = (byte)053;	// [11][8][3]
		xlate_hw['*'] = (byte)054;	// [11][8][4]
		xlate_hw[')'] = (byte)055;	// [11][8][5]
		xlate_hw[';'] = (byte)056;	// [11][8][6]
		xlate_hw['-'] = (byte)0157;	// [11] = '!' or hw 040 '-'
		xlate_hw['\002'] = (byte)0201;	// [11][8][7] Not HW (broken bar)

		xlate_hw['\004'] = (byte)072;	// [0][8][2] No keypunch symbol, '@'
		xlate_hw[','] = (byte)073;	// [0][8][3]
		xlate_hw['%'] = (byte)074;	// [0][8][4]
		xlate_hw['_'] = (byte)075;	// [0][8][5]
		xlate_hw['>'] = (byte)076;	// [0][8][6]
		xlate_hw['?'] = (byte)077;	// [0][8][7]

		hw2lp = new byte[64];
		Arrays.fill(xlate_hw, (byte)0);
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
	}

	public CharConverter(CardPunchOptions opts) {
		setup_xlate(opts);
	}

	public String punToAscii(int code) {
		int c = code & 0x0fff;
		if (c == 0) { // might be faster path
			return " ";
		}
		bb[0] = xlate_pun[c];
		if (bb[0] == 0) {
			return null;
		}
		if (bb[0] < ' ') {
			return spcl_pun[bb[0]];
		}
		return new String(bb);
	}

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
	public int punToHW(int code, boolean alt) {
		int c = code & 0x0fff;
		// TODO: handle "alt" code scheme...
		byte b = xlate_pun[c];	// never negative
		if (b == 0) {
			return -1;
		}
		c = xlate_hw[b] & 0x0ff;
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
}
