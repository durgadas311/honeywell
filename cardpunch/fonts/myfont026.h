// first byte of each char is encoding value...
// every 8 bytes starts new char...
unsigned char fontTable[0x1000] = {
	0x56,	//  0,0	'V'
	0b10001,
	0b10001,
	0b10001,
	0b01010,
	0b01010,
	0b00100,
	0b00100,
	0x53,	//  1,0 'S'
	0b01110,
	0b10001,
	0b01000,
	0b00100,
	0b00010,
	0b10001,
	0b01110,
	0x2f,	//  2,0	'/'
	0b00000,
	0b00001,
	0b00010,
	0b00100,
	0b01000,
	0b10000,
	0b00000,
	0x4f,	//  3,0	'O'
	0b01110,
	0b10001,
	0b10001,
	0b10001,
	0b10001,
	0b10001,
	0b01110,
	0x54,	//  4,0	'T'
	0b11111,
	0b00100,
	0b00100,
	0b00100,
	0b00100,
	0b00100,
	0b00100,
	0x55,	//  5,0	'U'
	0b10001,
	0b10001,
	0b10001,
	0b10001,
	0b10001,
	0b10001,
	0b01110,
	0x58,	//  6,0	'X'
	0b10001,
	0b10001,
	0b01010,
	0b00100,
	0b01010,
	0b10001,
	0b10001,
	0x4e,	//  0,1	'N'
	0b10001,
	0b11001,
	0b10101,
	0b10011,
	0b10001,
	0b10001,
	0b10001,
	0x4b,	//  1,1	'K'
	0b10001,
	0b10010,
	0b10100,
	0b11000,
	0b10100,
	0b10010,
	0b10001,
	0x4a,	//  2,1	'J'
	0b00001,
	0b00001,
	0b00001,
	0b00001,
	0b00001,
	0b10001,
	0b01110,
	0x2d,	//  3,1	'-'
	0b00000,
	0b00000,
	0b00000,
	0b00000,
	0b11111,
	0b00000,
	0b00000,
	0x4c,	//  4,1	'L'
	0b10000,
	0b10000,
	0b10000,
	0b10000,
	0b10000,
	0b10000,
	0b11111,
	0x4d,	//  5,1	'M'
	0b10001,
	0b11011,
	0b10101,
	0b10101,
	0b10001,
	0b10001,
	0b10001,
	0x50,	//  6,1	'P'
	0b11110,
	0b10001,
	0b10001,
	0b11110,
	0b10000,
	0b10000,
	0b10000,
	0x45,	//  0,2	'E'
	0b11111,
	0b10000,
	0b10000,
	0b11100,
	0b10000,
	0b10000,
	0b11111,
	0x42,	//  1,2	'B'
	0b11110,
	0b01001,
	0b01001,
	0b01110,
	0b01001,
	0b01001,
	0b11110,
	0x41,	//  2,2	'A'
	0b00100,
	0b01010,
	0b10001,
	0b10001,
	0b11111,
	0b10001,
	0b10001,
	0x2b,	//  3,2	'+'
	0b00100,
	0b00100,
	0b00100,
	0b11111,
	0b00100,
	0b00100,
	0b00100,
	0x43,	//  4,2	'C'
	0b01110,
	0b10001,
	0b10000,
	0b10000,
	0b10000,
	0b10001,
	0b01110,
	0x44,	//  5,2	'D'
	0b11110,
	0b01001,
	0b01001,
	0b01001,
	0b01001,
	0b01001,
	0b11110,
	0x47,	//  6,2	'G'
	0b01111,
	0b10000,
	0b10000,
	0b10000,
	0b10001,
	0b10001,
	0b01111,
	0x35,	//  0,3	'5'
	0b11111,
	0b10000,
	0b11110,
	0b00001,
	0b00001,
	0b10001,
	0b01110,
	0x32,	//  1,3	'2'
	0b01110,
	0b10001,
	0b00001,
	0b01110,
	0b10000,
	0b10000,
	0b11111,
	0x31,	//  2,3	'1'
	0b00100,
	0b01100,
	0b00100,
	0b00100,
	0b00100,
	0b00100,
	0b01110,
	0x33,	//  4,3	'3'
	0b01110,
	0b10001,
	0b00001,
	0b00110,
	0b00001,
	0b10001,
	0b01110,
	0x34,	//  5,3	'4'
	0b00010,
	0b00110,
	0b01010,
	0b10010,
	0b11111,
	0b00010,
	0b00010,
	0x37,	//  6,3	'7'
	0b11111,
	0b00001,
	0b00010,
	0b00100,
	0b01000,
	0b01000,
	0b01000,
	0x57,	//  1,4	'W'
	0b10001,
	0b10001,
	0b10001,
	0b10001,
	0b10101,
	0b11011,
	0b10001,
	0x5a,	//  2,4	'Z'
	0b11111,
	0b00001,
	0b00010,
	0b00100,
	0b01000,
	0b10000,
	0b11111,
	0x59,	//  3,4	'Y'
	0b10001,
	0b10001,
	0b01010,
	0b00100,
	0b00100,
	0b00100,
	0b00100,
	0x2c,	//  4,4	','
	0b00000,
	0b00000,
	0b00000,
	0b01100,
	0b01100,
	0b00100,
	0b01000,
	0x28,	//  5,4	'('
	0b00010,
	0b00100,
	0b01000,
	0b01000,
	0b01000,
	0b00100,
	0b00010,
	0x30,	//  1,5	'0'
	0b01110,
	0b10001,
	0b10001,
	0b10001,
	0b10001,
	0b10001,
	0b01110,
	0x52,	//  2,5	'R'
	0b11110,
	0b10001,
	0b10001,
	0b11110,
	0b10100,
	0b10010,
	0b10001,
	0x51,	//  3,5	'Q'
	0b01110,
	0b10001,
	0b10001,
	0b10001,
	0b10101,
	0b10010,
	0b01101,
	0x24,	//  4,5	'$'
	0b00100,
	0b01111,
	0b10000,
	0b01110,
	0b00001,
	0b11110,
	0b00100,
	0x2a,	//  5,5	'*'
	0b10101,
	0b01110,
	0b11111,
	0b01110,
	0b10101,
	0b00000,
	0b00000,
	0x46,	//  1,6	'F'
	0b11111,
	0b10000,
	0b10000,
	0b11100,
	0b10000,
	0b10000,
	0b10000,
	0x49,	//  2,6	'I'
	0b00100,
	0b00100,
	0b00100,
	0b00100,
	0b00100,
	0b00100,
	0b00100,
	0x48,	//  3,6	'H'
	0b10001,
	0b10001,
	0b10001,
	0b11111,
	0b10001,
	0b10001,
	0b10001,
	0x2e,	//  4,6	'.'
	0b00000,
	0b00000,
	0b00000,
	0b00000,
	0b00000,
	0b01100,
	0b01100,
	0x29,	//  5,6	')'
	0b01000,
	0b00100,
	0b00010,
	0b00010,
	0b00010,
	0b00100,
	0b01000,
	0x36,	//  1,7	'6'
	0b00110,
	0b01000,
	0b10000,
	0b11110,
	0b10001,
	0b10001,
	0b01110,
	0x39,	//  2,7	'9'
	0b01110,
	0b10001,
	0b10001,
	0b01111,
	0b00001,
	0b00010,
	0b01100,
	0x38,	//  3,7	'8'
	0b01110,
	0b10001,
	0b10001,
	0b01110,
	0b10001,
	0b10001,
	0b01110,
	0x3d,	//  4,7	'='
	0b00000,
	0b00000,
	0b11111,
	0b00000,
	0b11111,
	0b00000,
	0b00000,
	0x27,	//  5,7	'''
	0b00000,
	0b01100,
	0b01100,
	0b01100,
	0b00000,
	0b00000,
	0b00000,

	0x00	// terminate list
};
