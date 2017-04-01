// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.io.*;

public interface Peripheral {
	public void io(RWChannel rwc);
	public void run(RWChannel rwc);
	public boolean ctl(RWChannel rwc);	// true if branch
	public void setInterrupt(HW2000 sys);
	public boolean busy(byte c2);
	public void reset();
	public void visible(boolean on);
}
