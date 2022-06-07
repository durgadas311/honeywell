// Copyright (c) 2022 Douglas Miller <durgadas311@gmail.com>

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;

class MagTapeDrive extends JPanel {

	private static ImageIcon tapeIn = null;
	private static ImageIcon tapeOut = null;
	private static Color bsh = new Color(30, 30, 30);
	private static Color bhi = new Color(70, 70, 70);
	private static Color hw1 = new Color(123,174,255);
	private static Color hw2 = new Color(143,194,255);
	private static Font smallFont = new Font("Sans-Serif", Font.PLAIN, 8);

	private JButton reels;
	private JButton rew;
	private JButton permit;
	private JLabel pos;
	private JLabel lab;

	MagTapeDrive(int ix) {
		super();
		if (tapeIn == null) {
			tapeIn = new ImageIcon(getClass().getResource("icons/mti_full.png"));
			tapeOut = new ImageIcon(getClass().getResource("icons/mti_empty.png"));
		}

		lab = new JLabel(String.format("%d", ix));
		lab.setFont(smallFont);
		lab.setForeground(Color.white);
		lab.setPreferredSize(new Dimension(10, 14));
		pos = new JLabel("");
		pos.setBackground(hw2);
		pos.setOpaque(false);
		pos.setPreferredSize(new Dimension(80, 20));
		rew = new JButton("\u25c4\u25c4");
		rew.setFont(smallFont);
		rew.setMargin(new Insets(1, 1, 1, 1));
		rew.setFocusable(false);
		rew.setActionCommand(String.format("R%d", ix));
		rew.setPreferredSize(new Dimension(30, 14));
		permit = new JButton("PER");
		permit.setFont(smallFont);
		permit.setBackground(Peripheral.btnWhiteOff);
		permit.setMargin(new Insets(1, 1, 1, 1));
		permit.setFocusable(false);
		permit.setActionCommand(String.format("P%d", ix));
		permit.setPreferredSize(new Dimension(30, 14));
		reels = new JButton();
		reels.setBorderPainted(false);
		reels.setFocusPainted(false);
		reels.setFocusable(false);
		reels.setBackground(Color.black);
		reels.setPreferredSize(new Dimension(100, 53));
		reels.setActionCommand(String.format("M%d", ix));
		reels.setOpaque(true);
		setTape(null);

		JPanel pn;
		GridBagConstraints gc = new GridBagConstraints();
		gc.fill = GridBagConstraints.NONE;
		gc.gridx = 0;
		gc.gridy = 0;
		gc.weightx = 0;
		gc.weighty = 0;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gc.insets.bottom = 0;
		gc.insets.top = 0;
		gc.insets.left = 0;
		gc.insets.right = 0;
		gc.anchor = GridBagConstraints.CENTER;
		GridBagLayout gb = new GridBagLayout();
		setLayout(gb);

		// Operator Controls
		pn = opCtl();
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridy;

		// Tape area
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(100+6, 53+6));
		pn.setBackground(Color.black);
		pn.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED,
			bhi, bsh));
		// all this crap to get the button centered...
		pn.setLayout(new GridBagLayout());
		pn.add(reels, new GridBagConstraints());
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridy;

		// Cabinet (mech. access)
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(100+6, 160));
		pn.setBackground(hw1);
		pn.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
		pn.add(pos);
		gb.setConstraints(pn, gc);
		add(pn);
	}

	private JPanel opCtl() {
		JPanel pn = new JPanel();
		pn.setPreferredSize(new Dimension(100+6, 20));
		pn.setBackground(Color.black);
		pn.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED,
			bhi, bsh));
		pn.setLayout(new GridBagLayout());
		GridBagConstraints gc = new GridBagConstraints();
		gc.insets.left = gc.insets.right = 2;
		gc.gridx = gc.gridy = 0;
		pn.add(lab, gc);
		gc.insets.left = gc.insets.right = 4;
		++gc.gridx;
		pn.add(rew, gc);
		++gc.gridx;
		pn.add(permit, gc);
		return pn;
	}

	public void setTape(String img) {
		if (img != null) {
			reels.setIcon(tapeIn);
			reels.setToolTipText(img);
			pos.setText("");
			pos.setOpaque(true);
		} else {
			reels.setIcon(tapeOut);
			reels.setToolTipText("No Tape");
			pos.setText("");
			pos.setOpaque(false);
		}
	}

	public void permitListener(ActionListener lstn) {
		permit.addActionListener(lstn);
	}

	public void rewListener(ActionListener lstn) {
		rew.addActionListener(lstn);
	}

	public void reelListener(ActionListener lstn) {
		reels.addActionListener(lstn);
	}

	public void setPos(long p) {
		pos.setText(String.format("%d", p));
	}

	public void setPos(String s) {
		pos.setText(s);
	}

	public void setPermit(boolean on) {
		permit.setBackground(on ? Peripheral.btnWhiteOn : Peripheral.btnWhiteOff);
	}
}
