public class I_SI implements Instruction {
	// Set Item mark
	public void execute(HW2000 sys) {
		sys.setItem(sys.AAR);
		sys.incrAAR(-1);
		if (sys.hadB()) {
			sys.setItem(sys.BAR);
		}
		sys.incrBAR(-1);
		sys.addTics(2);
	}
}
