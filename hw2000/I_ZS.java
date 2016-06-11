public class I_ZS implements Instruction {
	// Zero and Subtract (decimal)
	public void execute(HW2000 sys) {
		I_ZA.zero_add(sys, true);
	}
}
