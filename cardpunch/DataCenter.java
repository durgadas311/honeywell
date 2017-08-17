// Copyright (c) 2011,2014 Douglas Miller

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.io.*;

public class DataCenter extends JFrame implements ActionListener, WindowListener
{
	private static DataCenter dc;

	public static void main(String[] args) {
		int x;
		for (x = 0; x < args.length; ++x) {
			if (!args[x].startsWith("-")) {
				break;
			}
		}

		dc = new DataCenter();
	}

	// IOndexes into 'machs[]'
	static final int KEYPUNCH = 0;
	static final int SORTER = 1;
	static final int COLLATOR = 2;
	static final int ACCOUNTING = 3;
	static final int VIEWER = 4;
	static final int NUMACH = 5;

	private Machine[] machs;
	Dimension bd = new Dimension(210, 210);

	public DataCenter() {
		super("Punch-card Data Processing Center");
		machs = new Machine[NUMACH];

		JMenuBar mb = new JMenuBar();
		JMenu mu;
		JMenuItem mi;
		mu = new JMenu("File");
		mi = new JMenuItem("Viewer", KeyEvent.VK_V);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Quit", KeyEvent.VK_Q);
		mi.addActionListener(this);
		mu.add(mi);
		mb.add(mu);
		mu = new JMenu("Machines");
		mb.add(mu);
		mi = new JMenuItem("029 Keypunch", KeyEvent.VK_1);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("026 Keypunch", KeyEvent.VK_2);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("026-H Keypunch", KeyEvent.VK_3);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("082 Sorter", KeyEvent.VK_4);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("085 Collator", KeyEvent.VK_5);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("402 Accounting Mach", KeyEvent.VK_6);
		mi.addActionListener(this);
		mu.add(mi);
		mb.add(mu);
		setJMenuBar(mb);

		GridBagLayout gb = new GridBagLayout();
		setLayout(gb);
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
		gc.anchor = GridBagConstraints.NORTH;

		JButton bt;
		bt = makeButton("029 Keypunch", "keypunch", "docs/ibm029-sm.png");
		gb.setConstraints(bt, gc);
		add(bt);
		++gc.gridx;
		bt = makeButton("082 Sorter", "sorter", "docs/ibm083-sm.png");
		gb.setConstraints(bt, gc);
		add(bt);
		++gc.gridx;
		gc.gridx = 0;
		++gc.gridy;
		bt = makeButton("085 Collator", "collator", "docs/ibm085-sm.png");
		gb.setConstraints(bt, gc);
		add(bt);
		++gc.gridx;
		bt = makeButton("402 Accounting Machine", "accounting", "docs/ibm402-sm.png");
		gb.setConstraints(bt, gc);
		add(bt);
		++gc.gridx;

		getContentPane().setBackground(new Color(180, 180, 180));
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		pack();
		setVisible(true);
	}

	private JButton makeButton(String ttl, String act, String ico) {
		Icon ic = new ImageIcon(getClass().getResource(ico));
		JButton bt = new JButton(ttl, ic);
		bt.setVerticalTextPosition(SwingConstants.BOTTOM);
		bt.setHorizontalTextPosition(SwingConstants.CENTER);
		bt.addActionListener(this);
		bt.setActionCommand(act);
		bt.setToolTipText(ttl);
		bt.setPreferredSize(new Dimension(bd));
		bt.setContentAreaFilled(false);
		return bt;
	}

	private void openMach(int ix, String ttl, CardPunchOptions opts) {
		if (machs[ix] != null) {
			machs[ix].getFrame().setVisible(true);
			// raise also?
			return;
		}
		JFrame frame = new JFrame(ttl);
		Machine mach = null;
		switch (ix) {
		case KEYPUNCH:
			mach = new PunchCardDeck(frame, opts);
			break;
		case SORTER:
			// TODO: model options...
			mach = new CardSorter(frame);
			break;
		case COLLATOR:
			// TODO: model options...
			mach = new CardCollator(frame);
			break;
		case ACCOUNTING:
			// TODO: model options...
			mach = new CardAccounting(frame);
			break;
		case VIEWER:
			mach = new CardViewer(frame, true);
			break;
		}
		if (mach == null) {
			return;
		}
		JMenuBar mb = new JMenuBar();
		JMenu[] ms = mach.getMenu();
		for (int x = 0; x < ms.length; ++x) {
			mb.add(ms[x]);
		}
		frame.setJMenuBar(mb);
		frame.addWindowListener(this);
		frame.getContentPane().setBackground(Color.gray);
		frame.pack();
		frame.setVisible(true);
		mach.setQuitListener(this);
		machs[ix] = mach;
		if (ix == KEYPUNCH) {
			((PunchCardDeck)mach).start();
		}
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() instanceof JButton) {
			JButton bt = (JButton)e.getSource();
			String act = bt.getActionCommand();
			if (act.equals("keypunch")) {
				CardPunchOptions opts = new CardPunchOptions();
				openMach(KEYPUNCH, "IBM 029 Keypunch", opts);
			} else if (act.equals("sorter")) {
				openMach(SORTER, "IBM 082 Sorter", null);
			} else if (act.equals("collator")) {
				openMach(COLLATOR, "IBM 085 Collator", null);
			} else if (act.equals("accounting")) {
				openMach(ACCOUNTING, "IBM 402 Accounting Machine", null);
			}
			return;
		}
		if (e.getSource() instanceof Machine) {
			Machine em = (Machine)e.getSource();
			for (int x = 0; x < machs.length; ++x) {
				if (machs[x] == em) {
					machs[x].getFrame().setVisible(false);
					machs[x] = null;
					break;
				}
			}
		}
		if (!(e.getSource() instanceof JMenuItem)) {
			return;
		}
		JMenuItem m = (JMenuItem)e.getSource();
		if (m.getMnemonic() == KeyEvent.VK_Q) {
			System.exit(0);
		} else if (m.getMnemonic() == KeyEvent.VK_V) {
			openMach(VIEWER, "Punch Card Viewer", null);
		} else if (m.getMnemonic() == KeyEvent.VK_1) {
			CardPunchOptions opts = new CardPunchOptions();
			openMach(KEYPUNCH, "IBM 029 Keypunch", opts);
		} else if (m.getMnemonic() == KeyEvent.VK_2) {
			CardPunchOptions opts = new CardPunchOptions();
			opts.ibm026 = true;
			openMach(KEYPUNCH, "IBM 026 Keypunch", opts);
		} else if (m.getMnemonic() == KeyEvent.VK_3) {
			CardPunchOptions opts = new CardPunchOptions();
			opts.ibm026 = true;
			opts.fortran = true;
			openMach(KEYPUNCH, "IBM 026-H Keypunch", opts);
		} else if (m.getMnemonic() == KeyEvent.VK_4) {
			// TODO: options...
			openMach(SORTER, "IBM 082 Sorter", null);
		} else if (m.getMnemonic() == KeyEvent.VK_5) {
			// TODO: options...
			openMach(COLLATOR, "IBM 085 Collator", null);
		} else if (m.getMnemonic() == KeyEvent.VK_6) {
			// TODO: options...
			openMach(ACCOUNTING, "IBM 402 Accounting Machine", null);
		}
	}

	public void windowActivated(WindowEvent e) { }
	public void windowClosed(WindowEvent e) { }
	public void windowIconified(WindowEvent e) { }
	public void windowOpened(WindowEvent e) { }
	public void windowDeiconified(WindowEvent e) { }
	public void windowDeactivated(WindowEvent e) { }
	public void windowClosing(WindowEvent e) {
		for (Machine mach : machs) {
			if (mach == null) continue;
			if (e.getWindow() == mach.getFrame()) {
				mach.getFrame().setVisible(false);
				return;
			}
		}
	}
}
