// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class System2000 {
	public static void main(String[] args) {
		System.setProperty("awt.useSystemAAFontSettings","on");
		System.setProperty("swing.aatext", "true");
		// TODO: process options
		HW2000 hw = new HW2000();
		HW2000FrontPanel fp = new HW2000FrontPanel(hw);
		hw.setFrontPanel(fp);
	}
}
