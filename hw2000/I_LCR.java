public class I_LCR implements Instruction {
	// Load Control Register
	public void execute(HW2000 sys) {
		if (sys.hadA() && sys.numXtra() > 0) {
			sys.CTL.setV(sys.getXtra(0));
		}
		byte v = sys.CTL.getV();
		boolean allowed = (v == 064 || v == 067 || v == 070 || v == 077);
		if (sys.CTL.inStdMode() && sys.CTL.isPROTECT() &&
				!sys.isProceed()) {
			if (!sys.CTL.allowLCR() || !allowed) {
				throw new IIException("LCR violation", HW2000CCR.IIR_OPVIO);
			}
		}
		boolean restore = true;
		int reg = sys.loadFromAAR();
		switch(v) {
		case 054:
			sys.ATR = reg;
			break;
		case 064:
			sys.CSR = reg;
			break;
		case 066:
			sys.EIR = reg;
			break;
		case 067:
			restore = false;
			sys.AAR = reg;
			break;
		case 070:
			sys.BAR = reg;
			break;
		case 076:
			sys.IIR = reg;
			break;
		case 077:
			sys.SR = reg;
			break;
		default:
			sys.cr[v] = reg;
			break;
		}
		if (restore) {
			sys.restoreAAR();
		}
	}
}
