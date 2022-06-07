// Copyright (c) 2022 Douglas Miller <durgadas311@gmail.com>

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;

class DiskDrive extends JPanel {

	private static ImageIcon diskIn = null;
	private static ImageIcon diskOut = null;
	private static ImageIcon diskOut1 = null;
	private static ImageIcon diskOut2 = null;
	private static Color hw1 = new Color(123,174,255);
	private static Color hw2 = new Color(143,194,255);
	private static Font smallFont = new Font("Sans-Serif", Font.PLAIN, 8);

	private JButton pack;
	private JButton a_bt;
	private JButton b_bt;
	private JButton fmt;
	private JButton dat;
	private JLabel pos;
	private JLabel lab;
private int idx;

	DiskDrive(int ix) {
		super();
		idx = ix;
		if (diskIn == null) {
			diskIn = new ImageIcon(getClass().getResource("icons/dpi_full.png"));
			diskOut = new ImageIcon(getClass().getResource("icons/dpi_empty.png"));
		}

		lab = new JLabel(String.format("%d", ix));
		lab.setFont(smallFont);
		lab.setPreferredSize(new Dimension(10, 20));

		pos = new JLabel("");
		pos.setBackground(hw2);
		pos.setOpaque(false);
		pos.setPreferredSize(new Dimension(50, 20));

		a_bt = new JButton("A");
		a_bt.setBackground(Peripheral.btnWhiteOff);
		a_bt.setMargin(new Insets(0, 0, 0, 0));
		a_bt.setFocusable(false);
		a_bt.setActionCommand(String.format("A%d", ix));
		a_bt.setPreferredSize(new Dimension(25, 25));

		b_bt = new JButton("B");
		b_bt.setBackground(Peripheral.btnWhiteOff);
		b_bt.setMargin(new Insets(0, 0, 0, 0));
		b_bt.setFocusable(false);
		b_bt.setActionCommand(String.format("B%d", ix));
		b_bt.setPreferredSize(new Dimension(25, 25));

		fmt = new JButton("FMT");
		fmt.setFont(smallFont);
		fmt.setBackground(Peripheral.btnWhiteOff);
		fmt.setMargin(new Insets(0, 0, 0, 0));
		fmt.setFocusable(false);
		fmt.setActionCommand(String.format("F%d", ix));
		fmt.setPreferredSize(new Dimension(25, 25));

		dat = new JButton("DAT");
		dat.setFont(smallFont);
		dat.setBackground(Peripheral.btnWhiteOff);
		dat.setMargin(new Insets(0, 0, 0, 0));
		dat.setFocusable(false);
		dat.setActionCommand(String.format("D%d", ix));
		dat.setPreferredSize(new Dimension(25, 25));

		pack = new JButton();
		pack.setBorderPainted(false);
		pack.setFocusPainted(false);
		pack.setFocusable(false);
		pack.setBackground(hw1);
		pack.setPreferredSize(new Dimension(100, 100));
		pack.setActionCommand(String.format("M%d", ix));
		pack.setOpaque(true);
		setDisk(null);

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

		// Disk Pack area
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(120+6, 100+6));
		pn.setBackground(hw1);
		pn.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
		// all this crap to get the button centered...
		pn.setLayout(new GridBagLayout());
		pn.add(pack, new GridBagConstraints());
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridy;

		// Cabinet (mech. access)
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(120+6, 30));
		pn.setBackground(hw1);
		pn.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
		pn.add(pos);
		gb.setConstraints(pn, gc);
		add(pn);
	}

	private JPanel opCtl() {
		JPanel pn = new JPanel();
		pn.setPreferredSize(new Dimension(120+6, 30));
		pn.setBackground(hw1);
		pn.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
		pn.setLayout(new GridBagLayout());
		GridBagConstraints gc = new GridBagConstraints();
		gc.insets.left = gc.insets.right = 1;
		gc.gridx = gc.gridy = 0;
		pn.add(lab, gc);
		++gc.gridx;
		pn.add(a_bt, gc);
		++gc.gridx;
		pn.add(b_bt, gc);
		++gc.gridx;
		pn.add(fmt, gc);
		++gc.gridx;
		pn.add(dat, gc);
		return pn;
	}

	public void setDisk(String img) {
		if (img != null) {
			pack.setIcon(diskIn);
			pack.setToolTipText(img);
			pos.setText("");
			pos.setOpaque(true);
		} else {
			pack.setIcon(diskOut);
			pack.setToolTipText("No Disk Pack");
			pos.setText("");
			pos.setOpaque(false);
		}
	}

	public void a_Listener(ActionListener lstn) {
		a_bt.addActionListener(lstn);
	}

	public void b_Listener(ActionListener lstn) {
		b_bt.addActionListener(lstn);
	}

	public void fmtListener(ActionListener lstn) {
		fmt.addActionListener(lstn);
	}

	public void datListener(ActionListener lstn) {
		dat.addActionListener(lstn);
	}

	public void packListener(ActionListener lstn) {
		pack.addActionListener(lstn);
	}

	public void setPos(int c, int h) {
		pos.setText(String.format("%03d-%02d", c, h));
	}

	public void setPos(String s) {
		pos.setText(s);
	}

	public void setA(boolean on) {
		a_bt.setBackground(on ? Peripheral.btnWhiteOn : Peripheral.btnWhiteOff);
	}

	public void setB(boolean on) {
		b_bt.setBackground(on ? Peripheral.btnWhiteOn : Peripheral.btnWhiteOff);
	}

	public void setFMT(boolean on) {
		fmt.setBackground(on ? Peripheral.btnWhiteOn : Peripheral.btnWhiteOff);
	}

	public void setDAT(boolean on) {
		dat.setBackground(on ? Peripheral.btnWhiteOn : Peripheral.btnWhiteOff);
	}
}
