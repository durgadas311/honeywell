public class I_FAA implements Instruction {
	// Floating-point Accumulator-Accumulator ops

	public void execute(HW2000 sys) {
		if (sys.numXtra() != 2) {
			throw new FaultException("FAA malformed");
		}
		byte x = (byte)(sys.getXtra(0) & 070);
		byte y = (byte)(sys.getXtra(0) & 007);
		byte op = (byte)(sys.getXtra(1) & 077);

		switch(op) {
		case 000:	// Store Acc
			sys.AC[y] = sys.AC[x];
			break;
		case 002:	// Load Acc
			sys.AC[x] = sys.AC[y];
			break;
		case 001:	// Load Low-Order Result
			sys.AC[HW2000.LOR] = sys.AC[y];
			break;
		case 007:	// Store Low-Order Result
			sys.AC[y] = sys.AC[HW2000.LOR];
			break;
		case 010:	// Add
			sys.AC[y] += sys.AC[x];
			sys.AC[HW2000.LOR] = 0.0; // what is this
			if (Double.isInfinite(sys.AC[y])) {
				sys.CTL.setEXO(true);
			}
			break;
		case 011:	// Subtract
			sys.AC[y] = sys.AC[x] - sys.AC[y];
			sys.AC[HW2000.LOR] = 0.0; // what is this
			if (Double.isInfinite(sys.AC[y])) {
				sys.CTL.setEXO(true);
			}
			break;
		case 013:	// Multiply
			sys.AC[y] *= sys.AC[x];
			sys.AC[HW2000.LOR] = 0.0; // what is this
			if (Double.isInfinite(sys.AC[y])) {
				sys.CTL.setEXO(true);
			}
			break;
		case 012:	// Divide
			if (sys.AC[x] == 0.0) {
				sys.CTL.setDVC(true);
			}
			sys.AC[HW2000.LOR] = sys.AC[y] % sys.AC[x]; // remainder
			sys.AC[y] /= sys.AC[x];
			if (Double.isInfinite(sys.AC[y])) {
				sys.CTL.setEXO(true);
			}
			break;
		// TODO:
		//	Binary Mantissa Shift
		//	Others?
		}
	}
}
