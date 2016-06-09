public class I_CSM implements Instruction {
	// Change Sequencing Mode
	public void execute(HW2000 sys) {
		if (sys.hadB() && sys.op_xtra.length > 0) {
			sys.CTL.setV(sys.op_xtra[0]);
		}
		int t = sys.SR;
		sys.SR = sys.CSR;
		sys.CSR = t;
	}
}
