// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class EasyCoder extends JFrame implements ActionListener {
	static EasyCoder thus;

	JButton pick;
	JButton asmb;
	JCheckBox listing;
	JCheckBox swi;
	ButtonGroup outBg;
	JRadioButton brt;
	JRadioButton brtCard;
	JRadioButton boot;
	JRadioButton raw;
	JTextField inFile;

	public EasyCoder(String[] args) {
		super("EasyCoder Assembler");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));

		pick = new JButton("Pick");
		listing = new JCheckBox("Listing");
		swi = new JCheckBox("SW/SI");
		outBg = new ButtonGroup();
		brt = new JRadioButton("BRT (Tape)");
		brt.addActionListener(this);
		brtCard = new JRadioButton("BRT (Card)");
		brtCard.addActionListener(this);
		boot = new JRadioButton("Bootstrap");
		boot.addActionListener(this);
		raw = new JRadioButton("Raw Loader");
		raw.addActionListener(this);
		outBg.add(brt);
		outBg.add(brtCard);
		outBg.add(boot);
		outBg.add(raw);
		brt.setSelected(true);
		swi.setEnabled(false);

		inFile = new JTextField();
		inFile.setPreferredSize(new Dimension(200, 20));
		asmb = new JButton("Assemble");
		asmb.addActionListener(this);

		JPanel pn = new JPanel();
		pn.add(pick);
		pn.add(inFile);
		add(pn);
		JPanel pn2 = new JPanel();
		pn2.setLayout(new BoxLayout(pn2, BoxLayout.X_AXIS));
		pn2.add(listing);
		pn = new JPanel();
		pn.setLayout(new BoxLayout(pn, BoxLayout.Y_AXIS));
		pn.add(new JLabel("Output Format"));
		pn.add(brt);
		pn.add(brtCard);
		pn.add(boot);
		pn.add(raw);
		pn2.add(pn);
		pn2.add(swi);
		add(pn2);
		add(asmb);

		pack();
		setVisible(true);
	}

	private void assemble() {
		boolean cards = brtCard.isSelected();
		boolean bin = raw.isSelected();
		boolean bs = boot.isSelected();
		boolean list = listing.isSelected();
		boolean rawSW = swi.isSelected();
		File in = new File(inFile.getText());
		if (!in.exists()) {
			System.err.format("No file: %s\n", inFile.getText());
			return;
		}
		String s = inFile.getText().replaceFirst("\\.ezc$", "");
		File out = new File(s + ".out");
		File lst = new File(s + ".lst");

		FileOutputStream fo = null;
		FileOutputStream lo = null;
		try {
			fo = new FileOutputStream(out);
			if (list) {
				lo = new FileOutputStream(lst);
			}
		} catch (Exception ee) {
			ee.printStackTrace();
			return;
		}
		Assembler asm = new Assembler(in);
		Loader ldr;
		if (cards) {
			ldr = new CardLoader(fo, asm.charCvt());
		} else if (bs || bin) {
			ldr = new RawLoader(fo, rawSW ? asm : null, bin ? 250 : -1);
		} else {
			ldr = new TapeLoader(fo, asm.charCvt());
		}
		int e = asm.passOne();
		if (e >= 0) {
			e = asm.passTwo(ldr, lo);
		}
		if (e < 0) {
			System.err.println(asm.getErrors());
		}
		try { fo.close(); } catch (Exception ee) {}
		if (lo != null) {
			try { fo.close(); } catch (Exception ee) {}
		}
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() instanceof JButton) {
			JButton btn = (JButton)e.getSource();
			if (btn == asmb) {
				assemble();
			}
			return;
		}
		if (e.getSource() instanceof JRadioButton) {
			JRadioButton btn = (JRadioButton)e.getSource();
			if (btn == boot || btn == raw) {
				swi.setEnabled(true);
			} else {
				swi.setEnabled(false);
				swi.setSelected(false);
			}
			return;
		}
	}

	public static void main(String[] args) {
		thus = new EasyCoder(args);
	}
}
