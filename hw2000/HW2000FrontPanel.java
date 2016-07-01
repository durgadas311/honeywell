import java.awt.*;
import java.io.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

public class HW2000FrontPanel extends JFrame
		implements FrontPanel, ActionListener, Runnable {
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
	static final int btnSense = 0x4000;

	static final int btnClear = 0x0fff;
	static final int btnEnter = 0x0ff0;
	static final int btnDisplay = 0x0ff1;
	static final int btnDispP1 = 0x0ff2;
	static final int btnDispM1 = 0x0ff3;

	LightedButton run;
	LightedButton stop;
	LightedButton instr;
	LightedButton central;
	LightedButton init;
	LightedButton boot;
	LightedButton am2;
	LightedButton am3;
	LightedButton am4;
	JLabel intr;

	int contentsReg;
	int addressReg;
	int controlReg;
	int senseReg;

	LightedButton[] contents;
	LightedButton[] address;
	LightedButton[] control;
	LightedButton[] sense;
	JMenuItem mi_mon;

	int gbx;
	File _last = null;
	boolean listing = false;
	boolean tape = false;
	boolean monitor = false;
	boolean bootstrap = false;
	int currLow = 0;
	int currHi = 0;

	public HW2000FrontPanel(HW2000 sys) {
		super("Honeywell Series 2000");
		this.sys = sys; // may be null
		_last = new File(System.getProperty("user.dir"));

		getContentPane().setBackground(Color.black);
		bigFont = new Font("Sans-Serif", Font.PLAIN, 40);
		smallFont = new Font("Sans-Serif", Font.PLAIN, 8);

		setLayout(new FlowLayout(FlowLayout.CENTER, 0, 0));
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

		gbx = 29; // full-width of left panel, in GridBag cells/units
		JPanel lpn = new JPanel();
		lpn.setOpaque(false);
		GridBagLayout gb = new GridBagLayout();
		lpn.setLayout(gb);

		contents = new LightedButton[8];
		address = new LightedButton[19];
		control = new LightedButton[6];
		sense = new LightedButton[4];

		JPanel pn = new JPanel();
		pn.setPreferredSize(new Dimension(40,20));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 0;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		JLabel lb = new JLabel("CONTENTS");
		lb.setFont(bigFont);
		lb.setOpaque(true);
		lb.setPreferredSize(new Dimension(230,40));
		gc.gridx = 0;
		gc.gridy = 1;
		gc.gridwidth = 1;
		gb.setConstraints(lb, gc);
		lpn.add(lb);
		// CLEAR button...
		ImageIcon icn = new ImageIcon(getClass().getResource("icons/fp_clear.png"));
		LightedButton btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, btnContents | btnClear);
		btn.addActionListener(this);
		btn.setToolTipText("Clear");
		gc.gridx = 1;
		gb.setConstraints(btn, gc);
		lpn.add(btn);
		addButtons(contents, lpn, 1, btnContents, gb, gc);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40,10));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 2;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		lb = new JLabel("ADDRESS");
		lb.setFont(bigFont);
		lb.setOpaque(true);
		lb.setPreferredSize(new Dimension(230,40));
		gc.gridx = 0;
		gc.gridy = 3;
		gc.gridwidth = 1;
		gb.setConstraints(lb, gc);
		lpn.add(lb);
		// CLEAR button...
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, btnAddress | btnClear);
		btn.addActionListener(this);
		btn.setToolTipText("Clear");
		gc.gridx = 1;
		gb.setConstraints(btn, gc);
		lpn.add(btn);
		addButtons(address, lpn, 3, btnAddress, gb, gc);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40,10));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 4;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		lb = new JLabel("CONTROL");
		lb.setFont(bigFont);
		lb.setOpaque(true);
		lb.setPreferredSize(new Dimension(230,40));
		gc.gridx = 0;
		gc.gridy = 5;
		gc.gridwidth = 1;
		gb.setConstraints(lb, gc);
		lpn.add(lb);
		// No CLEAR button...
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(30,40));
		gc.gridx = 1;
		gb.setConstraints(pn, gc);
		lpn.add(pn);
		addButtons(control, lpn, 5, btnControl, gb, gc);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40,20));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 6;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		addControls(lpn, 7, gb, gc);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40,20));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 8;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_word.png"));
		contents[6].setIcon(icn);
		icn = new ImageIcon(getClass().getResource("icons/fp_item.png"));
		contents[7].setIcon(icn);

		add(lpn);

		//---------------------------------------------------------------

		JPanel rpn = new JPanel();
		rpn.setOpaque(false);
		gb = new GridBagLayout();
		rpn.setLayout(gb);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(160,20));
		pn.setOpaque(true);
		gc.gridx = 0;
		gc.gridy = 0;
		gc.gridwidth = 6;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(140,25));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 1;
		gc.gridwidth = 5;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20,140));
		pn.setOpaque(true);
		gc.gridx = 5;
		gc.gridy = 1;
		gc.gridwidth = 1;
		gc.gridheight = 9;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20,15));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 2;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20,10));
		pn.setOpaque(true);
		gc.gridx = 0;
		gc.gridy = 3;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		icn = new ImageIcon(getClass().getResource("icons/fp_disp.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, btnContents | btnDisplay);
		btn.addActionListener(this);
		gc.gridx = 1;
		gc.gridy = 2;
		gc.gridwidth = 1;
		gc.gridheight = 3;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(60,10));
		pn.setOpaque(true);
		gc.gridx = 2;
		gc.gridy = 3;
		gc.gridwidth = 2;
		gc.gridheight = 1;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		icn = new ImageIcon(getClass().getResource("icons/fp_enter.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, btnContents | btnEnter);
		btn.addActionListener(this);
		gc.gridx = 4;
		gc.gridy = 2;
		gc.gridwidth = 1;
		gc.gridheight = 3;
		gb.setConstraints(btn, gc);
		rpn.add(btn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(140,10));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 5;
		gc.gridwidth = 5;
		gc.gridheight = 1;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20,15));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 6;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20,10));
		pn.setOpaque(true);
		gc.gridx = 0;
		gc.gridy = 7;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		icn = new ImageIcon(getClass().getResource("icons/fp_disp.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, btnAddress | btnDisplay);
		btn.addActionListener(this);
		gc.gridx = 1;
		gc.gridy = 6;
		gc.gridwidth = 1;
		gc.gridheight = 3;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		icn = new ImageIcon(getClass().getResource("icons/fp_plus1.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, btnAddress | btnDispP1);
		btn.addActionListener(this);
		gc.gridx = 2;
		gc.gridy = 6;
		gc.gridwidth = 1;
		gc.gridheight = 3;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		icn = new ImageIcon(getClass().getResource("icons/fp_minus1.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, btnAddress | btnDispM1);
		btn.addActionListener(this);
		gc.gridx = 3;
		gc.gridy = 6;
		gc.gridwidth = 1;
		gc.gridheight = 3;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		icn = new ImageIcon(getClass().getResource("icons/fp_enter.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, btnAddress | btnEnter);
		btn.addActionListener(this);
		gc.gridx = 4;
		gc.gridy = 6;
		gc.gridwidth = 1;
		gc.gridheight = 3;
		gb.setConstraints(btn, gc);
		rpn.add(btn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(140,25));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 9;
		gc.gridwidth = 5;
		gc.gridheight = 1;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(160,20));
		pn.setOpaque(true);
		gc.gridx = 0;
		gc.gridy = 10;
		gc.gridwidth = 6;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20,40));
		pn.setOpaque(true);
		gc.gridx = 0;
		gc.gridy = 11;
		gc.gridwidth = 1;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 3);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 4");
		gc.gridx = 1;
		gc.gridy = 11;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[3] = btn;
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 2);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 3");
		gc.gridx = 2;
		gc.gridy = 11;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[2] = btn;
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 1);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 2");
		gc.gridx = 3;
		gc.gridy = 11;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[1] = btn;
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 0);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 1");
		gc.gridx = 4;
		gc.gridy = 11;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[0] = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20,40));
		pn.setOpaque(true);
		gc.gridx = 5;
		gc.gridy = 11;
		gc.gridwidth = 1;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(160,20));
		pn.setOpaque(true);
		gc.gridx = 0;
		gc.gridy = 12;
		gc.gridwidth = 6;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		add(rpn);

		//---------------------------------------------------------
		JMenuBar mb = new JMenuBar();
		JMenu mu = new JMenu("File");
		JMenuItem mi = new JMenuItem("Assemble", KeyEvent.VK_A);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Monitor", KeyEvent.VK_M);
		mi.addActionListener(this);
		mu.add(mi);
		mi_mon = mi;
		mi = new JMenuItem("Quit", KeyEvent.VK_Q);
		mi.addActionListener(this);
		mu.add(mi);
		mb.add(mu);
		mu = new JMenu("I/O");
		mi = new JMenuItem("Console", KeyEvent.VK_C);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("LinePrinter", KeyEvent.VK_P);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("MagTape", KeyEvent.VK_G);
		mi.addActionListener(this);
		mu.add(mi);
		mb.add(mu);
		mu = new JMenu("Debug");
		mi = new JMenuItem("Trace", KeyEvent.VK_T);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("TraceAll", KeyEvent.VK_L);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Dump", KeyEvent.VK_D);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("DumpMon", KeyEvent.VK_N);
		mi.addActionListener(this);
		mu.add(mi);
		mb.add(mu);
		setJMenuBar(mb);

		run.setToolTipText("Run");
		stop.setToolTipText("Stop");
		instr.setToolTipText("Instruct");
		central.setToolTipText("Central Clear");
		init.setToolTipText("Initialize");
		boot.setToolTipText("Bootstrap");

		// Until someone else asks for it...
		run.addActionListener(this);
		stop.addActionListener(this);
		instr.addActionListener(this);
		central.addActionListener(this);
		init.addActionListener(this);
		boot.addActionListener(this);
		am2.addActionListener(this);
		am3.addActionListener(this);
		am4.addActionListener(this);

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
	private void setSense(int v) {
		senseReg = v & 077;
		setBits(sense, v);
		repaint();
	}
	public int getSense() {
		return senseReg;
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
		run.removeActionListener(this);
		stop.removeActionListener(this);
		instr.removeActionListener(this);
		central.removeActionListener(this);
		init.removeActionListener(this);
		boot.removeActionListener(this);

		// TODO: some of these should always remain ours...
		run.addActionListener(lstr);
		stop.addActionListener(lstr);
		instr.addActionListener(lstr);
		central.addActionListener(lstr);
		init.addActionListener(lstr);
		boot.addActionListener(lstr);
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

	private void addControls(Container top, int row, GridBagLayout gb, GridBagConstraints gc) {
		gc.gridy = row;
		gc.gridwidth = 30;
		JPanel pn;
		JPanel npn;

		npn = new JPanel();
		GridBagLayout gbl = new GridBagLayout();
		npn.setLayout(gbl);
		npn.setOpaque(false);
		gb.setConstraints(npn, gc);
		top.add(npn);

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
		init = btn;
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
		boot = btn;
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
		central = btn;
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
		instr = btn;
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
		instr.setActionCommand("instr");
		central.setActionCommand("clear");
		init.setActionCommand("init");
		boot.setActionCommand("boot");
		am2.setActionCommand("am2");
		am3.setActionCommand("am3");
		am4.setActionCommand("am4");
	}

	private void addButtons(LightedButton[] btns, Container top, int row, int id, GridBagLayout gb, GridBagConstraints gc) {
		gc.gridy = row;
		gc.gridwidth = 1;
		int goff = 0;
		JPanel pn;

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		gc.gridx = gbx - goff;
		gb.setConstraints(pn, gc);
		top.add(pn);
		++goff;
		int m = 0;
		for (int x = 0; x < btns.length; ++x) {
			btns[x] = new LightedButton(btnWhiteOn, btnWhiteOff, null, id | x);
			btns[x].addActionListener(this);
			gc.gridx = gbx - x - goff;
			gb.setConstraints(btns[x], gc);
			top.add(btns[x]);
			if (++m == 3) {
				m = 0;
				++goff;
				pn = new JPanel();
				pn.setPreferredSize(new Dimension(10, 40));
				gc.gridx = gbx - x - goff;
				gb.setConstraints(pn, gc);
				top.add(pn);
			}
		}
		int d = 19 - btns.length;
		gc.gridwidth = gbx - btns.length + 1 - goff - 2;
		gc.gridx = 2;
		pn = new JPanel();
		pn.setOpaque(true);
		pn.setPreferredSize(new Dimension((d * 30) + ((d + 1) / 3 * 10) + 20, 40)); // will just any width work?
		gb.setConstraints(pn, gc);
		top.add(pn);
	}

	private void doBootStrap() {
		setRunStop(true);
		bootstrap = true;
		byte c1 = (byte)011;
		byte c2 = (byte)(contentsReg | PeriphDecode.P_IN);
		sys.SR = addressReg;
		sys.AAR = addressReg;
		sys.BAR = addressReg;
		setCtrlReg(c1, addressReg);
		sys.CTL.setV(c1);
		sys.setXtra(new byte[]{c1, c2});
		Instruction op_exec = sys.idc.getExec(InstrDecode.OP_PDT);
		try {
			op_exec.execute(sys);
		} catch (Exception ee) {
			setRunStop(false);
			bootstrap = false;
			warning(this, "Bootstrap", ee.getMessage());
			return;
		}
		Thread thrd = new Thread(this);
		thrd.start();
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() instanceof JMenuItem) {
			performMenu((JMenuItem)e.getSource());
			return;
		}
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
					setContents(sys.rawReadMem(addressReg));
				} else if (idx == btnEnter) {
					sys.rawWriteMem(addressReg, (byte)contentsReg);
				} else {
					setContents(contentsReg | (1 << idx));
				}
			} else if (cls == btnAddress) {
				if (idx == btnClear) {
					setAddress(0);
				} else if (idx == btnDisplay) {
					setAddress(getCtrlReg((byte)controlReg, 0));
					setContents(sys.rawReadMem(addressReg));
				} else if (idx == btnDispP1) {
					setAddress(getCtrlReg((byte)controlReg, 1));
					setContents(sys.rawReadMem(addressReg));
				} else if (idx == btnDispM1) {
					setAddress(getCtrlReg((byte)controlReg, -1));
					setContents(sys.rawReadMem(addressReg));
				} else if (idx == btnEnter) {
					setCtrlReg((byte)controlReg, addressReg);
				} else {
					setAddress(addressReg | (1 << idx));
				}
			} else if (cls == btnControl) {
				setControl(controlReg ^ (1 << idx));
			} else if (cls == btnSense) {
				setSense(senseReg ^ (1 << idx));
			}
		} else {
			String a = lb.getActionCommand();
			if (a == null) {
				return;
			}
			if (a.equals("stop")) {
				bootstrap = false;
				sys.halt = true;
				sys.endWait();
			} else if (sys.halt) {
				if (a.equals("run")) {
					Thread thrd = new Thread(this);
					thrd.start();
				} else if (a.equals("instr")) {
					sys.singleStep = true;
					Thread thrd = new Thread(this);
					thrd.start();
				} else if (a.equals("clear")) {
					// nothing of interest to do?
				} else if (a.equals("init")) {
					// TODO: simulate "lamp test" function?
					monitor = false;
					mi_mon.setEnabled(true);
					sys.reset();
					setAddress(sys.SR);
					setContents(sys.rawReadMem(addressReg));
					setInterrupt(false);
					currLow = 0;
					currHi = 0;
				} else if (a.equals("boot")) {
					doBootStrap();
				} else if (a.equals("am2")) {
					sys.setAM(HW2000CCR.AIR_AM_2C);
				} else if (a.equals("am3")) {
					sys.setAM(HW2000CCR.AIR_AM_3C);
				} else if (a.equals("am4")) {
					sys.setAM(HW2000CCR.AIR_AM_4C);
				}
			}
		}
	}

	private int getCtrlReg(byte reg, int incr) {
		int val = 0;
		int r;
		long d;
		byte s;
		switch(reg & 077) {
		case 041:
		case 045:
		case 051:
		case 055: // exponent of FP ACC
		case 042:
		case 046:
		case 052:
		case 056: // low mantissa of FP ACC
		case 043:
		case 047:
		case 053:
		case 057: // high mantissa of FP ACC
			r = (reg & 014) >> 2;
			d  = Double.doubleToLongBits(sys.AC[r]);
			s = (byte)((d >> 63) & 1);
			if ((d & 0x7fffffffffffffffL) == 0) {
				break;
			}
			switch(reg & 003) {
			case 001: // exponent
				val = (int)((d >> 52) & 0x7ff);
				val -= 1023;
				if ((val & 0x400) != 0) {
					val |= 0x800;
				}
				val &= 0xfff;
				break;
			case 002: // low mantissa
				val = (int)((d >> 18) & 0x3ffff);
				if (s != 0) {
					val = (-val & 0x3ffff);
				}
				val &= 0x3ffff;
				break;
			case 003: // high mantissa
				val = (int)((d >> 36) & 0x0ffff);
				if (!sys.denorm[r]) {
					val |= 0x10000; // implied "1"
				}
				if (s != 0) {
					val = -val;
				}
				val &= 0x3ffff;
				break;
			}
			break;
		case 054:
			val = sys.ATR;
			if (incr != 0) { sys.ATR += incr; }
			break;
		case 064:
			val = sys.CSR;
			if (incr != 0) { sys.CSR += incr; }
			break;
		case 066:
			val = sys.EIR;
			if (incr != 0) { sys.EIR += incr; }
			break;
		case 067:
			val = sys.AAR;
			if (incr != 0) { sys.AAR += incr; }
			break;
		case 070:
			val = sys.BAR;
			if (incr != 0) { sys.BAR += incr; }
			break;
		case 076:
			val = sys.IIR;
			if (incr != 0) { sys.IIR += incr; }
			break;
		case 077:
			val = sys.SR;
			if (incr != 0) { sys.SR += incr; }
			break;
		default:
			val = sys.cr[reg & 077];
			if (incr != 0) { sys.cr[reg & 077] += incr; }
			break;
		}
		return val;
	}

	private void setCtrlReg(byte reg, int val) {
		int r;
		long d;
		byte s;
		switch(reg & 077) {
		case 041:
		case 045:
		case 051:
		case 055: // exponent of FP ACC
		case 042:
		case 046:
		case 052:
		case 056: // low mantissa of FP ACC
		case 043:
		case 047:
		case 053:
		case 057: // high mantissa of FP ACC
			r = (reg & 014) >> 2;
			d  = Double.doubleToLongBits(sys.AC[r]);
			// Can't really detect zero here, need all parts.
			// Same with sign. Setting FP regs is not advised.
			switch(reg & 003) {
			case 001: // exponent
				val &= 0x7ff;
				val += 1023;
				d = (d & ~0x7ff000000003ffffL) | (val << 52);
				break;
			case 002: // low mantissa
				// must know sign to handle properly...
				val &= 0x3ffff;
				d = (d & ~0x0000000fffffffffL) | (val << 18);
				break;
			case 003: // high mantissa
				s = (byte)((val >> 17) & 1);
				val &= 0x1ffff;
				if (s != 0) {
					val = -val;
				}
				sys.denorm[r] = ((val & 0x10000) == 0);
				val &= 0x0ffff;
				d = (d & ~0x800ffff00003ffffL) | (val << 36) | (s << 63);
				break;
			}
			sys.AC[r] = Double.longBitsToDouble(d);
			break;
		case 054:
			sys.ATR = val;
			break;
		case 064:
			sys.CSR = val;
			break;
		case 066:
			sys.EIR = val;
			break;
		case 067:
			sys.AAR = val;
			break;
		case 070:
			sys.BAR = val;
			break;
		case 076:
			sys.IIR = val;
			break;
		case 077:
			sys.SR = val;
			break;
		default:
			sys.cr[reg & 077] = val;
			break;
		}
	}

	private void pgmBoot(OutputStream fo, String boot) {
		try {
			InputStream fi = this.getClass().getResourceAsStream(boot);
			byte[] bt = new byte[fi.available()];
			fi.read(bt);
			fo.write(bt);
			fo.write(0301); // record mark
		} catch (Exception ee) {
			warning(this, "Write Boot", ee.getMessage());
		}
	}
	private void pgmHeader(OutputStream fo, int load, int len, int start, int pre) {
		// Always 3-char address mode - right?
		try {
			fo.write((load >> 12) & 077);
			fo.write((load >> 6) & 077);
			fo.write(load & 077);
			fo.write((len >> 12) & 077);
			fo.write((len >> 6) & 077);
			fo.write(len & 077);
			fo.write((start >> 12) & 077);
			fo.write((start >> 6) & 077);
			fo.write(start & 077);
			fo.write((pre >> 12) & 077);
			fo.write((pre >> 6) & 077);
			fo.write(pre & 077);
			fo.write(0301); // record mark
		} catch (Exception ee) {
			warning(this, "Write Header", ee.getMessage());
		}
	}
	private void pgmCode(OutputStream fo, int low, int hi) {
		try {
			for (int a = low; a < hi; ++a) {
				fo.write(sys.rawReadMem(a) & 077);
			}
			fo.write(0301); // record mark
		} catch (Exception ee) {
			warning(this, "Write Code", ee.getMessage());
		}
	}
	private void pgmPreamble(OutputStream fo, int low, int hi) {
		try {
			for (int a = low; a < hi; ++a) {
				int p = sys.rawReadMem(a) & 0300;
				fo.write((p >> 6) ^ 003);
			}
			fo.write(0301); // record mark
		} catch (Exception ee) {
			warning(this, "Write Punctuation", ee.getMessage());
		}
	}

	private File pickFile(String purpose, String sfx, String typ, File prev) {
		File file = null;
		listing = false;
		tape = false;
		SuffFileChooser ch = new SuffFileChooser(purpose, sfx, typ, prev, 5);
		int rv = ch.showDialog(this);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
			listing = ch.wantListing();
			tape = ch.wantTapeImg();
		}
		return file;
	}

	private boolean asmFile(String op) {
		OutputStream lst = null;
		File src = pickFile(op + " Program",
				"ezc", "EasyCoder", _last);
		if (src == null) {
			return false;
		}
		_last = src;
		Assembler asm = new Assembler(src);
		int e = asm.passOne();
		if (e < 0) {
			warning(this, op, asm.getErrors());
			return false;
		}
		Peripheral p = null;
		if (listing) {
			String l = src.getAbsolutePath();
			if (l.endsWith(".ezc")) {
				l = l.substring(0, l.length() - 4);
			}
			l += ".lst";
			try {
				lst = new FileOutputStream(new File(l));
			} catch (Exception ee) {
				warning(this, op, ee.getMessage());
				return false;
			}
			p = sys.pdc.getPeriph(PeriphDecode.P_LP);
			if (p != null) {
				p.setOutput(lst);
			}
		}
		int low = asm.getMin();
		int hi = asm.getMax();
		int start = asm.getStart();
		// NOTE: tape image is not consistent with monitor
		int reloc = 0;
		int brr = 0;
		int ibr = 0;
		if (monitor) {
			brr = 2; // TODO: manage memory and allocate space
			ibr = ((hi + 07777) >> 12);
			reloc = (brr << 12);
		}
		currLow = 0;
		currHi = 0;
		sys.setTrace(currLow, currHi); // trace off
		e = asm.passTwo(sys, reloc, listing);
		if (e < 0) {
			warning(this, op, asm.getErrors());
			return false;
		}
		if (listing) {
			asm.listSymTab();
		}
		if (monitor) {
			sys.setField(0007, ibr);
			sys.setField(0005, brr);
			sys.setField(0003, start);
			byte[] nm = asm.getHWName();
			int x;
			for (x = 0; x < nm.length; ++x) {
				sys.rawWriteMem(0010 + x, nm[x]);
			}
			sys.rawWriteMem(0010 + x, (byte)((sys.rawReadMem(0010 + x) & 077) | 0300));

			// TODO: add program name to monitor data
			//sys.SR = sys.CSR; // with interactive monitor, do not force SR
		} else if (tape) {
			// TODO: Copy mag tape bootstrap to new file...
			FileOutputStream fo = null;
			String l = src.getAbsolutePath();
			if (l.endsWith(".ezc")) {
				l = l.substring(0, l.length() - 4);
			}
			l += ".mti";
			try {
				fo = new FileOutputStream(new File(l));
			} catch (Exception ee) {
				warning(this, op, ee.getMessage());
				return false;
			}
			pgmBoot(fo, "bringup/bootmt.mti");
			pgmHeader(fo, low, hi - low, start, (hi + 07777) & ~07777);
			pgmCode(fo, low, hi);
			pgmPreamble(fo, low, hi);
			try {
				fo.close();
			} catch (Exception ee) {}
		} else {
			sys.SR = start;
		}
		setAddress(sys.SR);
		setContents(sys.rawReadMem(addressReg));
		currLow = reloc + low;
		currHi = reloc + hi;
		if (p != null) {
			p.setOutput(null);
		}
		if (lst != null) {
			try { lst.close(); } catch (Exception ee) {}
		}
		inform(this, op, String.format("Assembly complete. %07o %07o %07o",
			reloc + low, reloc + hi, reloc + start));
		return true;
	}

	private void performMenu(JMenuItem mi) {
		if (mi.getMnemonic() == KeyEvent.VK_A) {
			asmFile("Assemble");
		} else if (mi.getMnemonic() == KeyEvent.VK_M) {
			// run automatically?
			// set 'true' only after running?
			monitor = asmFile("Monitor");
			mi_mon.setEnabled(!monitor);
		} else if (mi.getMnemonic() == KeyEvent.VK_Q) {
			System.exit(0);
		} else if (mi.getMnemonic() == KeyEvent.VK_C) {
			Peripheral p = sys.pdc.getPeriph(PeriphDecode.P_CO);
			if (p != null) {
				p.visible(true);
			}
		} else if (mi.getMnemonic() == KeyEvent.VK_P) {
			Peripheral p = sys.pdc.getPeriph(PeriphDecode.P_LP);
			if (p != null) {
				p.visible(true);
			}
		} else if (mi.getMnemonic() == KeyEvent.VK_G) {
			Peripheral p = sys.pdc.getPeriph(PeriphDecode.P_MT);
			if (p != null) {
				p.visible(true);
			}
		} else if (mi.getMnemonic() == KeyEvent.VK_T) {
			sys.setTrace(currLow, currHi);
		} else if (mi.getMnemonic() == KeyEvent.VK_L) {
			sys.setTrace(0, 02000000);
		} else if (mi.getMnemonic() == KeyEvent.VK_D) {
			if (currLow < currHi) {
				sys.dumpHW(currLow, currHi - 1);
			}
		} else if (mi.getMnemonic() == KeyEvent.VK_N) {
			sys.dumpHW(0, 03777);
		}

	}

	public static void warning(Component top, String op, String err) {
		JOptionPane.showMessageDialog(top,
			new JLabel(err),
			op + " Warning", JOptionPane.WARNING_MESSAGE);
	}

	public static void inform(Component top, String op, String err) {
		JOptionPane.showMessageDialog(top,
			new JLabel(err),
			op + " Information", JOptionPane.INFORMATION_MESSAGE);
	}

	public static int confirm(String op, String err) {
		int res = JOptionPane.showConfirmDialog(null,
			new JLabel(err),
			op + " Confirmation", JOptionPane.YES_NO_OPTION);
		return res;
	}

	public void run() {
		if (bootstrap) {
			sys.waitIO();
			setRunStop(false);
			bootstrap = false;
		} else {
			sys.run();
		}
	}
}
