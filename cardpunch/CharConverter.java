// Copyright (c) 2010,2014 Douglas Miller

import java.util.Arrays;

class CharConverter {

	private byte[] xlate_pun;

	private void setup_pun_xlate() {
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

		xlate_pun[0x082] = 'm';
		xlate_pun[0x042] = '#';
		xlate_pun[0x022] = '@';
		xlate_pun[0x012] = 'm';
		xlate_pun[0x00a] = 'm';
		xlate_pun[0x006] = 'm';

		xlate_pun[0xa00] = 'm';
		xlate_pun[0x882] = 'm';
		xlate_pun[0x842] = 'm';
		xlate_pun[0x822] = 'm';
		xlate_pun[0x812] = 'm';
		xlate_pun[0x80a] = 'm';
		xlate_pun[0x806] = 'm';

		xlate_pun[0x482] = 'm';
		xlate_pun[0x442] = '$';
		xlate_pun[0x422] = '*';
		xlate_pun[0x412] = 'm';
		xlate_pun[0x40a] = 'm';
		xlate_pun[0x406] = 'm';

		xlate_pun[0x282] = 'm';
		xlate_pun[0x242] = ',';
		xlate_pun[0x222] = '%';
		xlate_pun[0x212] = 'm';
		xlate_pun[0x20a] = 'm';
		xlate_pun[0x206] = 'm';
	}

	public CharConverter() {
		setup_pun_xlate();
	}

	public byte punToAscii(int code) {
		byte b;
		if (code == 0) {
			return ' ';
		}
		b = xlate_pun[code];
		return b;
	}
}
