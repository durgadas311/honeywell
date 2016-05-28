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
		frame.setLayout(new FlowLayout());

		PunchCardDeck card;
		if (args.length > 0) {
			card = new PunchCardDeck(args[0]);
		} else {
			card = new PunchCardDeck(null);
		}
		frame.addKeyListener(card);
		frame.add(card);

		JMenuBar mb = new JMenuBar();
		mb.add(card.getMenu());

		frame.setJMenuBar(mb);
		frame.getContentPane().setBackground(Color.black);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.pack();
		frame.setVisible(true);
	}
}
