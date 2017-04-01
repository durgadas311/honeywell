// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_IIC implements Instruction {
	// Internal Interrupt Call (undocumented/not well documented)
	public void execute(HW2000 sys) {
		sys.CTL.setII((byte)0);
	}
}
