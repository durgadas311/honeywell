// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_S implements Instruction {
	// Subtract (decimal)
	public void execute(HW2000 sys) {
		I_A.add_sub(sys, true);
	}
}
