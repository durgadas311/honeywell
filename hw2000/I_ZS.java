// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_ZS implements Instruction {
	public String mnem() { return "ZS"; }
	// Zero and Subtract (decimal)
	public void execute(HW2000 sys) {
		I_ZA.zero_add(sys, true);
	}
}
