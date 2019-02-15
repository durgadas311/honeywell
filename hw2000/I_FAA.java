// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_FAA implements Instruction {
	// Floating-point Accumulator-Accumulator ops

	public void execute(HW2000 sys) {
		if (sys.numXtra() != 2) {
			throw new FaultException("FAA malformed");
		}
		byte x = (byte)((sys.getXtra(0) & 070) >> 3);
		byte y = (byte)(sys.getXtra(0) & 007);
		byte op = (byte)(sys.getXtra(1) & 077);

		double a, b;
		switch(op) {
		case 000:	// Store Acc
			sys.AC[y] = sys.AC[x];
			sys.denorm[y] = sys.denorm[x]; // right?
			sys.addTics(3);
			break;
		case 002:	// Load Acc
			sys.AC[x] = sys.AC[y];
			sys.denorm[x] = sys.denorm[y]; // right?
			sys.addTics(3);
			break;
		case 001:	// Load Low-Order Result
			sys.AC[HW2000.LOR] = sys.AC[y];
			sys.denorm[HW2000.LOR] = sys.denorm[y]; // right?
			sys.addTics(2);
			break;
		case 007:	// Store Low-Order Result
			sys.AC[y] = sys.AC[HW2000.LOR];
			sys.denorm[y] = sys.denorm[HW2000.LOR]; // right?
			sys.addTics(2);
			break;
		case 010:	// Add
			a = I_FMA.binToNative(sys.AC[y], sys.denorm[y]) +
				I_FMA.binToNative(sys.AC[x], sys.denorm[x]);
			sys.AC[y] = I_FMA.nativeToBin(a);
			sys.AC[HW2000.LOR] = 0L; // what is this
			sys.denorm[y] = false;
			sys.denorm[HW2000.LOR] = false;
			if (Double.isInfinite(a)) {
				sys.CTL.setEXO(true);
			}
			sys.addTics(6); // + Nn/6
			break;
		case 011:	// Subtract
			a = I_FMA.binToNative(sys.AC[y], sys.denorm[y]) -
				I_FMA.binToNative(sys.AC[x], sys.denorm[x]);
			sys.AC[y] = I_FMA.nativeToBin(a);
			sys.AC[HW2000.LOR] = 0L; // what is this
			sys.denorm[y] = false;
			sys.denorm[HW2000.LOR] = false;
			if (Double.isInfinite(a)) {
				sys.CTL.setEXO(true);
			}
			sys.addTics(6); // + Nn/6
			break;
		case 013:	// Multiply
			a = I_FMA.binToNative(sys.AC[y], sys.denorm[y]) *
				I_FMA.binToNative(sys.AC[x], sys.denorm[x]);
			sys.AC[y] = I_FMA.nativeToBin(a);
			sys.AC[HW2000.LOR] = 0L; // what is this
			sys.denorm[y] = false;
			sys.denorm[HW2000.LOR] = false;
			if (Double.isInfinite(a)) {
				sys.CTL.setEXO(true);
			}
			sys.addTics(6); // + N1/6 + Nn/6
			break;
		case 012:	// Divide
			if (sys.AC[x] == 0L) {
				sys.CTL.setDVC(true);
			}
			a = I_FMA.binToNative(sys.AC[y], sys.denorm[y]);
			b = I_FMA.binToNative(sys.AC[x], sys.denorm[x]);
			sys.AC[HW2000.LOR] = I_FMA.nativeToBin(a % b); // remainder
			a /= b;
			sys.AC[y] = I_FMA.nativeToBin(a);
			sys.denorm[y] = false;
			sys.denorm[HW2000.LOR] = false;
			if (Double.isInfinite(a)) {
				sys.CTL.setEXO(true);
			}
			sys.addTics(12); // + Nn/6
			break;
		}
		// If the operation overwrote AC[7], restore 0.0
		sys.AC[7] = 0L;
		sys.denorm[7] = false;
	}
}
