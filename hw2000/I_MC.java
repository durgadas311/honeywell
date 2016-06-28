public class I_MC implements Instruction {
	// Monitor Call
	public void execute(HW2000 sys) {
		sys.CTL.setEI(HW2000CCR.EIR_MC);
		sys.addTics(1);
	}
}
