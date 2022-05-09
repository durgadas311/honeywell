// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public interface Instruction {
	String mnem();
	void execute(HW2000 sys);
}
