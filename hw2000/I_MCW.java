// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_MCW implements Instruction {
	// Move Characters to Word mark
	public void execute(HW2000 sys) {
		byte a;
		byte b;
		do {
			a = sys.readMem(sys.AAR);
			sys.incrAAR(-1);
			b = sys.writeMemMask(sys.BAR, a, (byte)0100);
			sys.incrBAR(-1);
		} while ((a & 0100) == 0 && (b & 0100) == 0);
	}
}
