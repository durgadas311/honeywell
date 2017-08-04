// Copyright (c) 2011,2014 Douglas Miller

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.io.*;

public class Viewer
{
	public static void main(String[] args) {
		CardViewer view;
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
		JFrame frame = new JFrame("Punch Card Viewer");
		view = new CardViewer(frame);

		JMenuBar mb = new JMenuBar();
		JMenu[] ms = view.getMenu();
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
