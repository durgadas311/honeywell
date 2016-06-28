public class I_NOP implements Instruction {
	// No OPeration
	public void execute(HW2000 sys) {
		// Actually do nothing...
		sys.addTics(1);
	}
}
