// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class System2000 {
	public static void main(String[] args) {
		boolean c220_3 = false;
		System.setProperty("awt.useSystemAAFontSettings","on");
		System.setProperty("swing.aatext", "true");
		for (String arg : args) {
			if (arg.equals("220-3")) {
				c220_3 = true;
			}
		}
		// TODO: process options
		HW2000 hw = new HW2000();
		HW2000FrontPanel fp = new HW2000FrontPanel(hw, c220_3);
		hw.setFrontPanel(fp);
	}
}
