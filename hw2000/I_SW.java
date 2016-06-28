public class I_SW implements Instruction {
	// Set Word mark
	public void execute(HW2000 sys) {
		sys.setWord(sys.AAR);
		sys.incrAAR(-1);
		// don't duplicate effort (duplicates A if no B)
		if (sys.hadB()) {
			sys.setWord(sys.BAR);
		}
		sys.incrBAR(-1);
		sys.addTics(2);
	}
}
