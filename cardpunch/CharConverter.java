// Copyright (c) 2010,2014 Douglas Miller

import java.util.Arrays;

class CharConverter {

	private byte[] xlate_pun;
	private String[] spcl_pun;
	private short[] xlate_char;
	private byte[] bb = new byte[1];

	private void setup_xlate() {
		xlate_pun = new byte[4096];
		Arrays.fill(xlate_pun, (byte)0);
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
		xlate_pun[0x600] = 'm';

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
		xlate_pun[0x406] = '\002';	// broken bar

		xlate_pun[0x282] = '\004';	// No keypunch symbol
		xlate_pun[0x242] = ',';
		xlate_pun[0x222] = '%';
		xlate_pun[0x212] = '_';
		xlate_pun[0x20a] = '>';
		xlate_pun[0x206] = '?';

		spcl_pun = new String[8];
		spcl_pun['\001'] = "\u00a2";	// cent
		spcl_pun['\002'] = "\u00ac";	// broken bar
		spcl_pun['\003'] = "\u00bf";	// undef
		spcl_pun['\004'] = "\u00a7";	// no sym - section

		xlate_char = new short[128];
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

		xlate_char['\003'] = (short)0xa00;	// hw 037 ('?')
		xlate_char['\001'] = (short)0x882;	// cent
		xlate_char['.'] = (short)0x842;
		xlate_char['<'] = (short)0x822;
		xlate_char['('] = (short)0x812;
		xlate_char['+'] = (short)0x80a;
		xlate_char['|'] = (short)0x806;		// Not HW

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
	}

	public CharConverter() {
		setup_xlate();
	}

	public String punToAscii(int code) {
		if (code == 0) {
			return " ";
		}
		bb[0] = xlate_pun[code];
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
		if (p == 0) {
			return -1;
		}
		return p;
	}
}
