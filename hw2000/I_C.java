public class I_C implements Instruction {
	// Compare
	public void execute(HW2000 sys) {
		byte a = sys.readMem(sys.AAR);
		sys.incrAAR(-1);
		byte b = sys.readMem(sys.BAR);
		sys.incrBAR(-1);
		byte aw = (a & 0100);
		byte bw = (b & 0100);
		byte c = 0;
		a &= 077;
		b &= 077;
		boolean lt = false;
		byte z = 0;
		boolean aDone = false;
		while (true) {
			c = (a ^ b);
			z |= c;	// will be 0 at end if B=A
			if (c != 0) {
				// The last one prevails...
				lt = (a < b);
			}
			if (bw != 0) {
				break;
			}
			aDone = (aDone || aw != 0);
			if (aDone) {
				a = 0;
			} else {
				a = sys.readMem(sys.AAR);
				sys.incrAAR(-1);
				aw = (a & 0100);
				a &= 077;
			}
			b = sys.readMem(sys.BAR);
			sys.incrBAR(-1);
			bw = (b & 0100);
			b &= 077;
		}
		sys.CTL.setCompare(lt, (z == 0));
	}
}
