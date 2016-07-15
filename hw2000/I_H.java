public class I_H implements Instruction {
	// Halt
	public void execute(HW2000 sys) {
		if (sys.hadA() && !sys.hadB()) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
		} else if (sys.hadB() && sys.numXtra() > 0) {
			sys.CTL.setV(sys.getXtra(0));
		}
		if (sys.fp != null) {
			sys.fp.setInterrupt(-1);
		}
		sys.halt = true;
		sys.addTics(1);
	}
}
