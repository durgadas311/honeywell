// Copyright (c) 2011,2014 Douglas Miller

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.io.*;

public class CardPunch
{
	public static void main(String[] args) {


		PunchCardDeck card;
		CardPunchOptions opts = new CardPunchOptions();
		int x;
		for (x = 0; x < args.length; ++x) {
			if (!args[x].startsWith("-")) {
				break;
			}
			if (args[x].equals("-i")) {
				opts.images = true;
			} else if (args[x].equals("-r")) {
				opts.ibm026 = true;
			} else if (args[x].equals("-F") || args[x].equals("-H")) {
				opts.fortran = true;
			} else if (args[x].equals("-d")) {
				opts.dots = true;
			}
		}
		if (!opts.images && x < args.length) {
			opts.output = args[x];
		}
		String s;
		if (opts.ibm026) {
			s = "IBM 026";
			if (opts.fortran) {
				s += "-H";
			}
		} else {
			s = "IBM 029";
		}

		JFrame frame = new JFrame(s + " Card Punch");
		card = new PunchCardDeck(frame, null, opts);

		JMenuBar mb = new JMenuBar();
		JMenu[] ms = card.getMenu();
		for (x = 0; x < ms.length; ++x) {
			mb.add(ms[x]);
		}

		frame.setJMenuBar(mb);
		frame.getContentPane().setBackground(card.getBg());
		frame.setFocusTraversalKeysEnabled(false);  // allows TAB key to work...
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.pack();
		frame.setVisible(true);
		card.start();
	}
}
