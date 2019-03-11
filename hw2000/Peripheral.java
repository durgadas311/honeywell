// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.awt.*;
import java.io.*;

public interface Peripheral {
	public static final Color btnWhiteOff = new Color(190, 190, 180);
	public static final Color btnWhiteOn = new Color(255, 255, 200);
	public static final Color btnRedOff = new Color(100, 0, 0);
	public static final Color btnRedOn = new Color(255, 0, 0);
	public static final Color btnGreenOff = new Color(0, 100, 0);
	public static final Color btnGreenOn = new Color(0, 255, 160);
	public static final Color indDark = new Color(50, 50, 50);
	public static final Color indLit = new Color(180, 180, 80);

	public void io(RWChannel rwc);
	public void run(RWChannel rwc);
	public boolean ctl(RWChannel rwc);	// true if branch
	public void setInterrupt();
	public boolean busy(byte c2);
	public void reset();
	public void visible(boolean on);
}
