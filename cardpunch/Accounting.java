// Copyright (c) 2011,2014 Douglas Miller

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.io.*;

public class Accounting
{
	public static void main(String[] args) {
		CardAccounting acct;
		int x;
		for (x = 0; x < args.length; ++x) {
			if (!args[x].startsWith("-")) {
				break;
			}
			if (args[x].equals("-i")) {
			} else if (args[x].equals("-r")) {
			} else if (args[x].equals("-F") || args[x].equals("-H")) {
			} else if (args[x].equals("-d")) {
			}
		}
		JFrame frame = new JFrame("IBM 402 Accounting Machine");
		acct = new CardAccounting(frame, null, null);

		JMenuBar mb = new JMenuBar();
		JMenu[] ms = acct.getMenu();
		for (x = 0; x < ms.length; ++x) {
			mb.add(ms[x]);
		}

		frame.setJMenuBar(mb);
		frame.getContentPane().setBackground(Color.gray);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.pack();
		frame.setVisible(true);
	}
}
