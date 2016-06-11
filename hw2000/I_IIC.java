public class I_IIC implements Instruction {
	// Internal Interrupt Call (undocumented/not well documented)
	public void execute(HW2000 sys) {
		sys.CTL.setII((byte)0);
	}
}
