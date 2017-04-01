// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_SCR implements Instruction {
	// Store Control Register
	public void execute(HW2000 sys) {
		if (sys.hadA() && sys.numXtra() > 0) {
			sys.CTL.setV(sys.getXtra(0));
		}
		byte v = sys.CTL.getV();
		int reg = 0;
		if (v < 040) {
			// CLC or SLC
			if (sys.CTL.inStdMode() && sys.CTL.isPROTECT() &&
					!sys.isProceed()) {
				throw new IIException("SCR violation", HW2000CCR.IIR_OPVIO);
			}
		}
		switch(v) {
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
			reg = sys.cr[v];
			break;
		}
		sys.storeToAAR(reg);
		sys.restoreAAR();
		sys.addTics(4 - sys.am_na);
	}
}
