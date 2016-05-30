// Copyright (c) 2011,2014 Douglas Miller

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.io.*;

public class CardPunch
{
	public static void main(String[] args) {

		JFrame frame = new JFrame("Honeywell Card Punch");

		PunchCardDeck card;
		boolean images = (args.length > 0 && args[0].equals("-i"));
		if (!images && args.length > 0) {
			card = new PunchCardDeck(frame, args[0]);
		} else {
			card = new PunchCardDeck(frame, null);
		}
		if (images) {
			card.setSaveImages();
		}

		JMenuBar mb = new JMenuBar();
		JMenu[] ms = card.getMenu();
		for (int x = 0; x < ms.length; ++x) {
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
