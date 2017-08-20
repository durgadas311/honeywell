// Copyright (c) 2011,2014 Douglas Miller

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.io.*;

public class Sorter
{
	public static void main(String[] args) {
		CardSorter sort;
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
		String s;
		s = "IBM 082";

		JFrame frame = new JFrame(s + " Sorter");
		sort = new CardSorter(frame, null);

		JMenuBar mb = new JMenuBar();
		JMenu[] ms = sort.getMenu();
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
