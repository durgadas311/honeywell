import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

public class HW2000FrontPanel extends JFrame implements FrontPanel, ActionListener {
	HW2000 sys;

	Font bigFont;
	Font smallFont;

	static final Color btnWhiteOff = new Color(190, 190, 180);
	static final Color btnWhiteOn = new Color(255, 255, 200);
	static final Color btnRedOff = new Color(100, 0, 0);
	static final Color btnRedOn = new Color(255, 0, 0);
	static final Color btnGreenOff = new Color(0, 100, 0);
	static final Color btnGreenOn = new Color(0, 255, 0);
	static final Color indDark = new Color(50, 50, 50);
	static final Color indLit = new Color(180, 180, 80);

	static final int btnContents = 0x1000;
	static final int btnAddress = 0x2000;
	static final int btnControl = 0x3000;

	static final int btnClear = 0x0fff;
	static final int btnEnter = 0x0ff0;
	static final int btnDisplay = 0x0ff1;
	static final int btnDispP1 = 0x0ff2;
	static final int btnDispM1 = 0x0ff3;

	LightedButton run;
	LightedButton stop;
	LightedButton am2;
	LightedButton am3;
	LightedButton am4;
	JLabel intr;

	int contentsReg;
	int addressReg;
	int controlReg;

	LightedButton[] contents;
	LightedButton[] address;
	LightedButton[] control;

	int gbx;

	public HW2000FrontPanel(HW2000 sys) {
		super("Honeywell Series 2000");
		this.sys = sys; // may be null
		getContentPane().setBackground(Color.black);
		bigFont = new Font("Sans-Serif", Font.PLAIN, 40);
		smallFont = new Font("Sans-Serif", Font.PLAIN, 8);

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

		gbx = 29; // full-width of frame, in GridBag cells/units

		contents = new LightedButton[8];
		address = new LightedButton[19];
		control = new LightedButton[6];

		JPanel pn = new JPanel();
		pn.setPreferredSize(new Dimension(40,20));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 0;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		add(pn);

		JLabel lb = new JLabel("CONTENTS");
		lb.setFont(bigFont);
		lb.setOpaque(true);
		lb.setPreferredSize(new Dimension(230,40));
		gc.gridx = 0;
		gc.gridy = 1;
		gc.gridwidth = 1;
		gb.setConstraints(lb, gc);
		add(lb);
		// CLEAR button...
		ImageIcon icn = new ImageIcon(getClass().getResource("icons/fp_clear.png"));
		LightedButton btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, btnContents | btnClear);
		btn.addActionListener(this);
		gc.gridx = 1;
		gb.setConstraints(btn, gc);
		add(btn);
		addButtons(contents, 1, btnContents, gb, gc);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40,10));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 2;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		add(pn);

		lb = new JLabel("ADDRESS");
		lb.setFont(bigFont);
		lb.setOpaque(true);
		lb.setPreferredSize(new Dimension(230,40));
		gc.gridx = 0;
		gc.gridy = 3;
		gc.gridwidth = 1;
		gb.setConstraints(lb, gc);
		add(lb);
		// CLEAR button...
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, btnAddress | btnClear);
		btn.addActionListener(this);
		gc.gridx = 1;
		gb.setConstraints(btn, gc);
		add(btn);
		addButtons(address, 3, btnAddress, gb, gc);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40,10));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 4;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		add(pn);

		lb = new JLabel("CONTROL");
		lb.setFont(bigFont);
		lb.setOpaque(true);
		lb.setPreferredSize(new Dimension(230,40));
		gc.gridx = 0;
		gc.gridy = 5;
		gc.gridwidth = 1;
		gb.setConstraints(lb, gc);
		add(lb);
		// No CLEAR button...
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(30,40));
		gc.gridx = 1;
		gb.setConstraints(pn, gc);
		add(pn);
		addButtons(control, 5, btnControl, gb, gc);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40,20));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 6;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		add(pn);

		addControls(7, gb, gc);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40,20));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 8;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_word.png"));
		contents[6].setIcon(icn);
		icn = new ImageIcon(getClass().getResource("icons/fp_item.png"));
		contents[7].setIcon(icn);

		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		pack();
		setVisible(true);
	}

	public void setContents(int v) {
		contentsReg = v & 0377;
		setBits(contents, v);
		repaint();
	}
	public void setAddress(int v) {
		addressReg = v & 01777777;
		setBits(address, v);
		repaint();
	}
	public void setControl(int v) {
		controlReg = v & 077;
		setBits(control, v);
		repaint();
	}
	public void setRunStop(boolean run) {
		this.run.setOn(run);
		this.stop.setOn(!run);
		repaint();
	}
	public void setAdrMode(int v) {
		am2.setOn(v == 2);
		am3.setOn(v == 3);
		am4.setOn(v == 4);
		repaint();
	}
	public void setInterrupt(boolean intr) {	// Indicator only
		if (intr) {
			this.intr.setForeground(indLit);
		} else {
			this.intr.setForeground(indDark);
		}
		repaint();
	}

	// Actions are:
	//	VK_R	Run
	//	VK_S	Stop
	//	VK_I	Initialize
	//	VK_B	Bootstrap
	//	VK_C	Central Clear
	//	VK_N	Instruct
	//	VK_2,VK_3,VK_4	Address Mode
	// TODO: how many of these are directly sent to core system?
	// (vs. being directly performed on core system object)
	//
	public void setPanelListener(ActionListener lstr) {
		run.addActionListener(lstr);
		stop.addActionListener(lstr);
	}

	// Are these ever queried?
	public int getContents() { return 0; }
	public int getAddress() { return 0; }
	public int getControl() { return 0; }
	public boolean getRunStop() { return false; }
	public int getAdrMode() { return 0; }

	private void setBits(LightedButton[] btns, int val) {
		for (int x = 0; x < btns.length; ++x) {
			btns[x].setOn((val & 1) != 0);
			val >>= 1;
		}
	}

	private void addControls(int row, GridBagLayout gb, GridBagConstraints gc) {
		gc.gridy = row;
		gc.gridwidth = 30;
		JPanel pn;
		JPanel npn;

		npn = new JPanel();
		GridBagLayout gbl = new GridBagLayout();
		npn.setLayout(gbl);
		npn.setOpaque(false);
		gb.setConstraints(npn, gc);
		add(npn);

		gc.gridy = 0;
		gc.gridwidth = gbx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(false);
		gc.gridx = 0;
		gb.setConstraints(pn, gc);
		npn.add(pn);

		// TODO: some of these need to be managed...
		LightedButton btn = new LightedButton(btnGreenOn, btnGreenOff, null, 0);
		btn.setOn(true);
		gc.gridx = 1;
		gb.setConstraints(btn, gc);
		npn.add(btn);
		btn = new LightedButton(btnRedOn, btnRedOff, null, 0);
		gc.gridx = 2;
		gb.setConstraints(btn, gc);
		npn.add(btn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(false);
		gc.gridx = 3;
		gb.setConstraints(pn, gc);
		npn.add(pn);

		btn = new LightedButton(btnGreenOn, btnGreenOff, null, 0);
		btn.setOn(true);
		gc.gridx = 4;
		gb.setConstraints(btn, gc);
		npn.add(btn);
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, 0);
		gc.gridx = 5;
		gb.setConstraints(btn, gc);
		npn.add(btn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40, 40));
		pn.setOpaque(false);
		gc.gridx = 6;
		gb.setConstraints(pn, gc);
		npn.add(pn);

		btn = new LightedButton(btnRedOn, btnRedOff, null, 0);
		btn.setOn(true);
		gc.gridx = 7;
		gb.setConstraints(btn, gc);
		npn.add(btn);
		stop = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(false);
		gc.gridx = 8;
		gb.setConstraints(pn, gc);
		npn.add(pn);

		ImageIcon icn;

		icn = new ImageIcon(getClass().getResource("icons/fp_init.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 9;
		gb.setConstraints(btn, gc);
		npn.add(btn);
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(false);
		gc.gridx = 10;
		gb.setConstraints(pn, gc);
		npn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_boot.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 11;
		gb.setConstraints(btn, gc);
		npn.add(btn);
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(false);
		gc.gridx = 12;
		gb.setConstraints(pn, gc);
		npn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_central.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 13;
		gb.setConstraints(btn, gc);
		npn.add(btn);
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(false);
		gc.gridx = 14;
		gb.setConstraints(pn, gc);
		npn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_instr.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 15;
		gb.setConstraints(btn, gc);
		npn.add(btn);
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(false);
		gc.gridx = 16;
		gb.setConstraints(pn, gc);
		npn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_run.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 17;
		gb.setConstraints(btn, gc);
		npn.add(btn);
		run = btn;

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40, 40));
		pn.setOpaque(false);
		gc.gridx = 18;
		gb.setConstraints(pn, gc);
		npn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_am2.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 19;
		gb.setConstraints(btn, gc);
		npn.add(btn);
		am2 = btn;
		icn = new ImageIcon(getClass().getResource("icons/fp_am3.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		btn.setOn(true);
		gc.gridx = 20;
		gb.setConstraints(btn, gc);
		npn.add(btn);
		am3 = btn;
		icn = new ImageIcon(getClass().getResource("icons/fp_am4.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 21;
		gb.setConstraints(btn, gc);
		npn.add(btn);
		am4 = btn;

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40, 40));
		pn.setOpaque(false);
		gc.gridx = 22;
		gb.setConstraints(pn, gc);
		npn.add(pn);
		intr = new JLabel("INTERRUPT");
		intr.setFont(smallFont);
		intr.setForeground(indDark);
		intr.setOpaque(false);
		intr.setPreferredSize(new Dimension(80, 20));
		gc.gridx = 23;
		gb.setConstraints(intr, gc);
		npn.add(intr);
		JLabel lb = new JLabel("PARITY");
		lb.setFont(smallFont);
		lb.setForeground(indDark);
		lb.setOpaque(false);
		lb.setPreferredSize(new Dimension(50, 20));
		gc.gridx = 24;
		gb.setConstraints(lb, gc);
		npn.add(lb);
		lb = new JLabel("VOLTAGE");
		lb.setFont(smallFont);
		lb.setForeground(indDark);
		lb.setOpaque(false);
		lb.setPreferredSize(new Dimension(60, 20));
		gc.gridx = 25;
		gb.setConstraints(lb, gc);
		npn.add(lb);
		lb = new JLabel("FAN");
		lb.setFont(smallFont);
		lb.setForeground(indDark);
		lb.setOpaque(false);
		lb.setPreferredSize(new Dimension(30, 20));
		gc.gridx = 26;
		gb.setConstraints(lb, gc);
		npn.add(lb);
		lb = new JLabel("CB");
		lb.setFont(smallFont);
		lb.setForeground(indDark);
		lb.setOpaque(false);
		lb.setPreferredSize(new Dimension(20, 20));
		gc.gridx = 27;
		gb.setConstraints(lb, gc);
		npn.add(lb);

		run.setActionCommand("run");
		stop.setActionCommand("stop");
	}

	private void addButtons(LightedButton[] btns, int row, int id, GridBagLayout gb, GridBagConstraints gc) {
		gc.gridy = row;
		gc.gridwidth = 1;
		int goff = 0;
		JPanel pn;

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		gc.gridx = gbx - goff;
		gb.setConstraints(pn, gc);
		add(pn);
		++goff;
		int m = 0;
		for (int x = 0; x < btns.length; ++x) {
			btns[x] = new LightedButton(btnWhiteOn, btnWhiteOff, null, id | x);
			btns[x].addActionListener(this);
			gc.gridx = gbx - x - goff;
			gb.setConstraints(btns[x], gc);
			add(btns[x]);
			if (++m == 3) {
				m = 0;
				++goff;
				pn = new JPanel();
				pn.setPreferredSize(new Dimension(10, 40));
				gc.gridx = gbx - x - goff;
				gb.setConstraints(pn, gc);
				add(pn);
			}
		}
		int d = 19 - btns.length;
		gc.gridwidth = gbx - btns.length + 1 - goff - 2;
		gc.gridx = 2;
		pn = new JPanel();
		pn.setOpaque(true);
		pn.setPreferredSize(new Dimension((d * 30) + ((d + 1) / 3 * 10) + 20, 40)); // will just any width work?
		gb.setConstraints(pn, gc);
		add(pn);
	}

	public void actionPerformed(ActionEvent e) {
		if (!(e.getSource() instanceof LightedButton)) {
			return;
		}
		LightedButton lb = (LightedButton)e.getSource();
		int id = lb.getId();
		if (id != 0) {
			if (run.isOn()) {
				return;
			}
			int cls = id & ~0x0fff;
			int idx = id & 0x0fff;
			if (cls == btnContents) {
				if (idx == btnClear) {
					setContents(0);
				} else if (idx == btnDisplay) {
				} else if (idx == btnEnter) {
				} else {
					setContents(contentsReg | (1 << idx));
				}
			} else if (cls == btnAddress) {
				if (idx == btnClear) {
					setAddress(0);
				} else if (idx == btnDisplay) {
				} else if (idx == btnDispP1) {
				} else if (idx == btnDispM1) {
				} else if (idx == btnEnter) {
				} else {
					setAddress(addressReg | (1 << idx));
				}
			} else if (cls == btnControl) {
				setControl(controlReg ^ (1 << idx));
			}
		}
	}

}
