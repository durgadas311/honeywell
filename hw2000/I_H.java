public class I_H implements Instruction {
	// Halt
	public void execute(HW2000 sys) {
		if (sys.hadA() && !sys.hadB()) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
		} else if (sys.hadB() && sys.op_xtra.length > 0) {
			sys.CTL.setV(sys.op_xtra[0]);
		}
		sys.halt = true;
	}
}
