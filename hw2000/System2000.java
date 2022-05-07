// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Properties;

public class System2000 {
	public static void main(String[] args) {
		boolean c220_3 = false;
		// TODO: allow properties file...
		Properties props = new Properties();
		System.setProperty("awt.useSystemAAFontSettings","on");
		System.setProperty("swing.aatext", "true");
		String[] ss;
		for (String arg : args) {
			if (arg.equals("220-3")) {
				props.setProperty("console", "220-3");
			} else if (arg.matches(".*=.*")) {
				ss = arg.split("=", 2);
				props.setProperty(ss[0], ss[1]);
			}
		}
		// TODO: process options
		HW2000 hw = new HW2000(props);
		HW2000FrontPanel fp = new HW2000FrontPanel(props, hw);
		hw.setFrontPanel(fp);
	}
}
