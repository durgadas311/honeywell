public class I_RNM implements Instruction {
	// Return to Normal Mode
	public void execute(HW2000 sys) {
		sys.clearIntr();
	}
}
