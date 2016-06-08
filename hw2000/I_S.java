public class I_S implements Instruction {
	// Subtract (decimal)
	public void execute(HW2000 sys) {
		I_A.add_sub(sys, true);
	}
}
