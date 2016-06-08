public class I_SCR implements Instruction {
	// Store Control Register
	public void execute(HW2000 sys) {
		if (sys.hadA() && sys.op_xtra.length > 0) {
			sys.CTL.setV(sys.op_xtra[0]);
		}
		byte v = sys.CTL.getV();
		int reg = 0;
		if (v < 040) {
			// CLC or SLC
		} else switch(v) {
			// TODO: check privilege...
			case 054:
				reg = sys.ATR;
				break;
			case 064:
				reg = sys.CSR;
				break;
			case 066:
				reg = sys.EIR;
				break;
			case 067:
				reg = sys.iaar;
				break;
			case 070:
				reg = sys.BAR;
				break;
			case 076:
				reg = sys.IIR;
				break;
			case 077:
				reg = sys.SR;
				break;
			default:
				// throw?
				break;
			}
		}
		sys.storeToAAR(reg);
		sys.restoreAAR();
	}
}
