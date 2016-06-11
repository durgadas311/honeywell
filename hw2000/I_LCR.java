public class I_LCR implements Instruction {
	// Load Control Register
	public void execute(HW2000 sys) {
		if (sys.hadA() && sys.op_xtra.length > 0) {
			sys.CTL.setV(sys.op_xtra[0]);
		}
		byte v = sys.CTL.getV();
		boolean allowed = (v == 064 || v == 067 || v == 070 || v == 077);
		if (sys.CTL.inStdMode() && sys.CTL.isPROTECT() &&
				!sys.CTL.isPROCEED()) {
			if (!sys.CTL.allowLCR() || !allowed) {
				throw new RuntimeException("LCR violation");
			}
		}
		int reg = sys.loadFromAAR();
		if (v < 040) {
			// CLC or SLC
		} else switch(v) {
			// TODO: check privilege...
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
				// throw?
				break;
		}
	}
}
