public class I_MAT implements Instruction {
	// Move characters to word mark And Translate
	public void execute(HW2000 sys) {
		int c = 0;
		if (sys.op_xtra.length == 2) {
			// TODO: how to tell between 2-char C-address and V1,V2
			c = (((sys.op_xtra[0] & 077) << 6) | (sys.op_xtra[1] & 077)) << 6;
			if (am_na == 4) {
				c |= (sys.AAR & 0x40000);
			} else {
				c = (c & 0x7fff) | (sys.AAR & 0x78000);
			}
		} else if (sys.op_xtra.length == sys.am_na) {
			for (int x = 0; x < sys.am_na; ++x) {
				c = (c << 6) | (sys.op_xtra[x] & 077);
			}
			// TODO: does C-address allow indirection/indexing?
			c = (sys.AAR & ~sys.am_mask) | (c & sys.am_mask);
			c &= ~077; // needed?
		} else {
			throw new RuntimeException("MAT malformed");
		}
		byte a;
		byte b;
		do {
			a = sys.readMem(sys.AAR);
			sys.incrAAR(-1);
			b = sys.readMem(c | (a & 077));
			sys.writeMem(sys.BAR, (b & 0277) | (sys.readMem(sys.BAR) & 0100));
			sys.incrBAR(-1);
		} while ((a & 0100) == 0 && (b & 0100) == 0);
	}
}
