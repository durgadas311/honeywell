// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.util.Map;
import java.util.TreeMap;
import java.util.Properties;
import java.awt.*;
import java.io.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.lang.reflect.Constructor;

public class HW2000FrontPanel extends JFrame
		implements FrontPanel, ActionListener, ChangeListener, Runnable {
	HW2000 sys;

	Font bigFont;
	Font medFont;
	Font smallFont;

	public static final Color btnWhiteOff = new Color(190, 190, 180);
	public static final Color btnWhiteOn = new Color(255, 255, 200);
	public static final Color btnRedOff = new Color(100, 0, 0);
	public static final Color btnRedOn = new Color(255, 0, 0);
	public static final Color btnGreenOff = new Color(0, 100, 0);
	public static final Color btnGreenOn = new Color(0, 255, 160);
	public static final Color indDark = new Color(50, 50, 50);
	public static final Color indLit = new Color(180, 180, 80);

	private static final byte[] _1HDR = new byte[]{ 001, 030, 024, 051, 015}; // 1HDR_
	private static final byte[] _1EOF = new byte[]{ 001, 025, 046, 026, 015}; // 1EOF_
	private static final byte[] _1ERI = new byte[]{ 001, 025, 051, 031, 015}; // 1ERI_

	static final int btnContents = 0x1000;
	static final int btnAddress = 0x2000;
	static final int btnControl = 0x3000;
	static final int btnSense = 0x4000;

	static final int btnClear = 0x0fff;
	static final int btnEnter = 0x0ff0;
	static final int btnDisplay = 0x0ff1;
	static final int btnDispP1 = 0x0ff2;
	static final int btnDispM1 = 0x0ff3;

	GenericHelp help;

	LightedButton run;
	LightedButton stop;
	LightedButton instr;
	LightedButton central;
	LightedButton init;
	LightedButton boot;
	LightedButton inter;
	LightedButton am2;
	LightedButton am3;
	LightedButton am4;
	LightedButton type;
	JLabel eintr;
	JLabel iintr;
	JLabel pgm;
	JLabel prot;
	JLabel parity;
	JLabel voltage;
	JLabel fan;
	JLabel cb;

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
	boolean typePressed = false;
	boolean listing = false;
	boolean tape = false;
	boolean monitor = false;
	boolean fortran = false;
	boolean dumpOnHalt = false;
	CompileFortran ftn = null;
	AssembleEZC asm = null;
	int currLow = 0;
	int currHi = 0;

	static final int OPTION_CANCEL = 0;
	static final int OPTION_YES = 1;
	private Object[] dump_btns;
	private JTextArea dump_lo;
	private JTextArea dump_hi;
	private JPanel dump_lo_pn;
	private JPanel dump_hi_pn;
	private JPanel dump_pn;
	int dumpLow;
	int dumpHi;
	private P_Console cons;
	private P_LinePrinter lpt;

	public HW2000FrontPanel(Properties props, HW2000 sys) {
		super("Honeywell Series 2000");
		LightedButton.init(64);
		this.sys = sys; // may be null
		_last = new File(System.getProperty("user.dir"));

		getContentPane().setBackground(Color.black);
		bigFont = new Font("Sans-Serif", Font.PLAIN, 40);
		medFont = new Font("Sans-Serif", Font.PLAIN, 28);
		smallFont = new Font("Sans-Serif", Font.PLAIN, 8);

		setLayout(new FlowLayout(FlowLayout.CENTER, 0, 0));

		// TODO: different for 220-3 console...
		String s = props.getProperty("console");
		if (s != null && s.equals("220-3")) {
			partControlPanel();
			getConsole().setTypeBtn(type);
			getConsole().visible(true);
		} else {
			fullControlPanel();
		}

		//---------------------------------------------------------
		JMenuBar mb = new JMenuBar();
		JMenu mu = new JMenu("File");
		JMenuItem mi;
		fortran = false;
		try {
			Class.forName("Fortran4");
			fortran = true;
		} catch (Exception ee) { }
		if (fortran) {
			mi = new JMenuItem("FORTRAN", KeyEvent.VK_F);
			mi.addActionListener(this);
			mu.add(mi);
		}
		mi = new JMenuItem("Assemble", KeyEvent.VK_A);
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
		mi = new JMenuItem("PunchCard", KeyEvent.VK_H);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Disks", KeyEvent.VK_K);
		mi.addActionListener(this);
		mu.add(mi);
		mb.add(mu);
		mu = new JMenu("Debug");
		mi = new JMenuItem("Trace", KeyEvent.VK_T);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Trace Full", KeyEvent.VK_L);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Trace Off", KeyEvent.VK_O);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Dump", KeyEvent.VK_D);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Dump Full", KeyEvent.VK_N);
		mi.addActionListener(this);
		mu.add(mi);
		mb.add(mu);
		mu = new JMenu("Help");
		mi = new JMenuItem("About", KeyEvent.VK_I);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Show Help", KeyEvent.VK_E);
		mi.addActionListener(this);
		mu.add(mi);
		mb.add(mu);
		setJMenuBar(mb);

		java.net.URL url = this.getClass().getResource("docs/hw2000.html");
		help = new GenericHelp(getTitle() + " Help", url);

		run.setActionCommand("run");
		stop.setActionCommand("stop");
		central.setActionCommand("clear");
		init.setActionCommand("init");
		inter.setActionCommand("inter");
		am2.setActionCommand("am2");
		am3.setActionCommand("am3");
		am4.setActionCommand("am4");

		// These always exist...
		run.setToolTipText("Run");
		stop.setToolTipText("Stop");
		central.setToolTipText("System Clear");
		init.setToolTipText("Initialize");
		inter.setToolTipText("Interrupt");
		// Until someone else asks for it, we are the listener...
		run.addActionListener(this);
		stop.addActionListener(this);
		central.addActionListener(this);
		init.addActionListener(this);
		init.addChangeListener(this);
		inter.addActionListener(this);
		am2.addActionListener(this);
		am3.addActionListener(this);
		am4.addActionListener(this);

		if (boot != null) {
			boot.setActionCommand("boot");
			boot.setToolTipText("Bootstrap");
			boot.addActionListener(this);
		}
		if (instr != null) {
			instr.setActionCommand("instr");
			instr.setToolTipText("Instruct");
			instr.addActionListener(this);
		}
		if (type != null) {
			type.setActionCommand("type");
			type.setToolTipText("Type");
			// TODO: need hold/release not click...
			type.addActionListener(this);
			type.addChangeListener(this);
		}

		// Dialog for Dump Full / Trace Full
		// for some reason, TAB doesn't traverse fields, even if setFocusTraversalKeysEnabled
		dump_pn = new JPanel();
		dump_pn.setLayout(new BoxLayout(dump_pn, BoxLayout.Y_AXIS));
		dump_btns = new Object[2];
		dump_btns[OPTION_YES] = "Dump";
		dump_btns[OPTION_CANCEL] = "Cancel";
		dump_lo = new JTextArea();
		dump_lo.setPreferredSize(new Dimension(200, 20));
		dump_lo_pn = new JPanel();
		dump_lo_pn.add(new JLabel("Low Adr:"));
		dump_lo_pn.add(dump_lo);
		dump_hi = new JTextArea();
		dump_hi.setPreferredSize(new Dimension(200, 20));
		dump_hi_pn = new JPanel();
		dump_hi_pn.add(new JLabel("High Adr:"));
		dump_hi_pn.add(dump_hi);
		dump_pn.add(dump_lo_pn);
		dump_pn.add(dump_hi_pn);

		setContents(0);
		setAddress(0);
		setControl(0);
		setSense(0);

		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		pack();
		setVisible(true);
		Thread thrd = new Thread(this);
		thrd.start();
	}

	private void fullControlPanel() {
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
		sense = new LightedButton[8];

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

		// fudge spacing, instead of more-complex paneling
		lb = new JLabel("HONEYWELL 2000          ");
		lb.setFont(bigFont);
		lb.setOpaque(false);
		lb.setForeground(Color.white);
		lb.setPreferredSize(new Dimension(560,50));
		gc.gridx = 0;
		gc.gridy = 8;
		gc.gridwidth = gbx;
		gb.setConstraints(lb, gc);
		lpn.add(lb);

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
		pn.setPreferredSize(new Dimension(20,190));
		pn.setOpaque(true);
		gc.gridx = 5;
		gc.gridy = 1;
		gc.gridwidth = 1;
		gc.gridheight = 11;
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
		btn.setToolTipText("Contents Display");
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
		btn.setToolTipText("Contents Enter");
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
		btn.setToolTipText("Control Display");
		gc.gridx = 1;
		gc.gridy = 6;
		gc.gridwidth = 1;
		gc.gridheight = 3;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		icn = new ImageIcon(getClass().getResource("icons/fp_plus1.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, btnAddress | btnDispP1);
		btn.addActionListener(this);
		btn.setToolTipText("Control Display +1");
		gc.gridx = 2;
		gc.gridy = 6;
		gc.gridwidth = 1;
		gc.gridheight = 3;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		icn = new ImageIcon(getClass().getResource("icons/fp_minus1.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, btnAddress | btnDispM1);
		btn.addActionListener(this);
		btn.setToolTipText("Control Display -1");
		gc.gridx = 3;
		gc.gridy = 6;
		gc.gridwidth = 1;
		gc.gridheight = 3;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		icn = new ImageIcon(getClass().getResource("icons/fp_enter.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, btnAddress | btnEnter);
		btn.addActionListener(this);
		btn.setToolTipText("Control Enter");
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
		pn.setPreferredSize(new Dimension(140,10));
		pn.setOpaque(true);
		gc.gridx = 0;
		gc.gridy = 10;
		gc.gridwidth = 5;
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
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 7);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 8");
		gc.gridx = 1;
		gc.gridy = 11;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[7] = btn;
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 6);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 7");
		gc.gridx = 2;
		gc.gridy = 11;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[6] = btn;
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 5);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 6");
		gc.gridx = 3;
		gc.gridy = 11;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[5] = btn;
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 4);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 5");
		gc.gridx = 4;
		gc.gridy = 11;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[4] = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20,40));
		pn.setOpaque(true);
		gc.gridx = 5;
		gc.gridy = 11;
		gc.gridwidth = 1;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20,40));
		pn.setOpaque(true);
		gc.gridx = 0;
		gc.gridy = 12;
		gc.gridwidth = 1;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 3);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 4");
		gc.gridx = 1;
		gc.gridy = 12;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[3] = btn;
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 2);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 3");
		gc.gridx = 2;
		gc.gridy = 12;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[2] = btn;
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 1);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 2");
		gc.gridx = 3;
		gc.gridy = 12;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[1] = btn;
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 0);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 1");
		gc.gridx = 4;
		gc.gridy = 12;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[0] = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20,40));
		pn.setOpaque(true);
		gc.gridx = 5;
		gc.gridy = 12;
		gc.gridwidth = 1;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(160,20));
		pn.setOpaque(true);
		gc.gridx = 0;
		gc.gridy = 13;
		gc.gridwidth = 6;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		add(rpn);
	}

	private void partControlPanel() {
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

		gbx = 7; // full-width of left panel, in GridBag cells/units
		JPanel lpn = new JPanel();
		lpn.setOpaque(false);
		GridBagLayout gb = new GridBagLayout();
		lpn.setLayout(gb);

		sense = new LightedButton[8];

		JPanel pn = new JPanel();
		pn.setPreferredSize(new Dimension(150,80));
		pn.setOpaque(true);
		gc.gridx = 0;
		gc.gridy = 0;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		LightedButton btn;
		ImageIcon icn;

		gc.gridwidth = 1;
		gc.gridy = 1;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(true);
		gc.gridx = 0;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_type.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 1;
		gb.setConstraints(btn, gc);
		lpn.add(btn);
		type = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 40));
		pn.setOpaque(true);
		gc.gridx = 2;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_central.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 3;
		gb.setConstraints(btn, gc);
		lpn.add(btn);
		central = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 40));
		pn.setOpaque(true);
		gc.gridx = 4;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_inter.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 5;
		gb.setConstraints(btn, gc);
		lpn.add(btn);
		inter = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(true);
		gc.gridx = 6;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		gc.gridy = 2;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(150, 20));
		pn.setOpaque(true);
		gc.gridx = 0;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		lpn.add(pn);
		gc.gridwidth = 1;

		gc.gridy = 3;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(true);
		gc.gridx = 0;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_am2.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 1;
		gb.setConstraints(btn, gc);
		lpn.add(btn);
		am2 = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 40));
		pn.setOpaque(true);
		gc.gridx = 2;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_am3.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		btn.setOn(true);
		gc.gridx = 3;
		gb.setConstraints(btn, gc);
		lpn.add(btn);
		am3 = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 40));
		pn.setOpaque(true);
		gc.gridx = 4;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_am4.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 5;
		gb.setConstraints(btn, gc);
		lpn.add(btn);
		am4 = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(true);
		gc.gridx = 6;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		gc.gridy = 4;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(150, 20));
		pn.setOpaque(true);
		gc.gridx = 0;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		lpn.add(pn);
		gc.gridwidth = 1;

		gc.gridy = 5;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(true);
		gc.gridx = 0;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		btn = new LightedButton(btnRedOn, btnRedOff, null, 0);
		btn.setOn(true);
		gc.gridx = 1;
		gb.setConstraints(btn, gc);
		lpn.add(btn);
		stop = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 40));
		pn.setOpaque(true);
		gc.gridx = 2;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_init.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 3;
		gb.setConstraints(btn, gc);
		lpn.add(btn);
		init = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 40));
		pn.setOpaque(true);
		gc.gridx = 4;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_run.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 5;
		gb.setConstraints(btn, gc);
		lpn.add(btn);
		run = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(true);
		gc.gridx = 6;
		gb.setConstraints(pn, gc);
		lpn.add(pn);

		gc.gridy = 6;
		gc.gridwidth = gbx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(150, 20));
		pn.setOpaque(true);
		gc.gridx = 0;
		gb.setConstraints(pn, gc);
		lpn.add(pn);
		gc.gridwidth = 1;

		add(lpn);

		//---------------------------------------------------------------

		JPanel rpn = new JPanel();
		rpn.setOpaque(false);
		gb = new GridBagLayout();
		rpn.setLayout(gb);
		gbx = 13;

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(310,20));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 0;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		gc.gridwidth = 1;
		// ---- SENSE 8-5
		gc.gridy = 1;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20,40));
		pn.setOpaque(false);
		gc.gridx = 0;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 7);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 8");
		gc.gridx = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[7] = btn;

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10,40));
		pn.setOpaque(false);
		gc.gridx = 2;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 6);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 7");
		gc.gridx = 3;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[6] = btn;

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10,40));
		pn.setOpaque(false);
		gc.gridx = 4;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 5);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 6");
		gc.gridx = 5;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[5] = btn;

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10,40));
		pn.setOpaque(false);
		gc.gridx = 6;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 4);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 5");
		gc.gridx = 7;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[4] = btn;

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(50,40));
		pn.setOpaque(false);
		gc.gridx = 8;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		btn = new LightedButton(btnGreenOn, btnWhiteOff, null, -1); // A/C ON
		btn.addActionListener(this);
		btn.setOn(true);
		gc.gridx = 9;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10,40));
		pn.setOpaque(false);
		gc.gridx = 10;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		LightedButton btn2 = btn;
		btn = new LightedButton(btnRedOn, btnRedOff, null, -1);	// A/C OFF
		btn.addActionListener(this);
		btn2.setNext(btn);
		btn.setNext(btn2);
		gc.gridx = 11;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20,40));
		pn.setOpaque(false);
		gc.gridx = 12;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(310,20));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 2;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		// ---- SENSE 4-1
		gc.gridwidth = 1;
		gc.gridy = 3;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20,40));
		pn.setOpaque(false);
		gc.gridx = 0;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 3);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 4");
		gc.gridx = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[3] = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10,40));
		pn.setOpaque(false);
		gc.gridx = 2;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 2);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 3");
		gc.gridx = 3;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[2] = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10,40));
		pn.setOpaque(false);
		gc.gridx = 4;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 1);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 2");
		gc.gridx = 5;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[1] = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10,40));
		pn.setOpaque(false);
		gc.gridx = 6;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, btnSense | 0);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 1");
		gc.gridx = 7;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[0] = btn;

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(50,40));
		pn.setOpaque(false);
		gc.gridx = 8;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		btn = new LightedButton(btnGreenOn, btnWhiteOff, null, -1); // DC ON
		btn.setOn(true);
		btn.addActionListener(this);
		gc.gridx = 9;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10,40));
		pn.setOpaque(false);
		gc.gridx = 10;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		btn2 = btn;
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, -1); // DC OFF
		btn.addActionListener(this);
		btn2.setNext(btn);
		btn.setNext(btn2);
		gc.gridx = 11;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20,40));
		pn.setOpaque(false);
		gc.gridx = 12;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(310,40));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 4;
		gc.gridwidth = gbx;
		gc.gridheight = 2;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		gc.gridheight = 1;
		gc.gridy = 6;
		gc.gridwidth = 3;
		gc.gridx = 1;
		eintr = new JLabel("EXTERNAL");
		eintr.setFont(smallFont);
		eintr.setForeground(indDark);
		eintr.setOpaque(false);
		eintr.setPreferredSize(new Dimension(50, 20));
		gc.anchor = GridBagConstraints.NORTH;
		gb.setConstraints(eintr, gc);
		rpn.add(eintr);
		iintr = new JLabel("INTERNAL");
		iintr.setFont(smallFont);
		iintr.setForeground(indDark);
		iintr.setOpaque(false);
		iintr.setPreferredSize(new Dimension(50, 20));
		gc.gridy = 7;
		gc.anchor = GridBagConstraints.SOUTH;
		gb.setConstraints(iintr, gc);
		rpn.add(iintr);

		gc.gridy = 6;
		gc.gridx = 4;
		pgm = new JLabel("PROGRAM");
		pgm.setFont(smallFont);
		pgm.setForeground(indDark);
		pgm.setOpaque(false);
		pgm.setPreferredSize(new Dimension(50, 20));
		gc.anchor = GridBagConstraints.NORTH;
		gb.setConstraints(pgm, gc);
		rpn.add(pgm);
		prot = new JLabel("PROTECT");
		prot.setFont(smallFont);
		prot.setForeground(indDark);
		prot.setOpaque(false);
		prot.setPreferredSize(new Dimension(50, 20));
		gc.gridy = 7;
		gc.anchor = GridBagConstraints.SOUTH;
		gb.setConstraints(prot, gc);
		rpn.add(prot);

		gc.gridy = 6;
		gc.gridx = 7;
		gc.gridwidth = 2;
		voltage = new JLabel("VOLTAGE");
		voltage.setFont(smallFont);
		voltage.setForeground(indDark);
		voltage.setOpaque(false);
		voltage.setPreferredSize(new Dimension(80, 20));
		gc.anchor = GridBagConstraints.NORTH;
		gb.setConstraints(voltage, gc);
		rpn.add(voltage);
		parity = new JLabel("PARITY");
		parity.setFont(smallFont);
		parity.setForeground(indDark);
		parity.setOpaque(false);
		parity.setPreferredSize(new Dimension(80, 20));
		gc.gridy = 7;
		gc.anchor = GridBagConstraints.SOUTH;
		gb.setConstraints(parity, gc);
		rpn.add(parity);

		gc.gridy = 6;
		gc.gridx = 9;
		gc.gridwidth = 2;
		fan = new JLabel("FAN");
		fan.setFont(smallFont);
		fan.setForeground(indDark);
		fan.setOpaque(false);
		fan.setPreferredSize(new Dimension(40, 20));
		gc.anchor = GridBagConstraints.NORTH;
		gb.setConstraints(fan, gc);
		rpn.add(fan);
		cb = new JLabel("CB");
		cb.setFont(smallFont);
		cb.setForeground(indDark);
		cb.setOpaque(false);
		cb.setPreferredSize(new Dimension(40, 20));
		gc.gridy = 7;
		gc.anchor = GridBagConstraints.SOUTH;
		gb.setConstraints(cb, gc);
		rpn.add(cb);

		gc.gridy = 8;
		// fudge spacing, instead of more-complex paneling
		JLabel lb = new JLabel("HONEYWELL 2000");
		lb.setFont(medFont);
		lb.setOpaque(false);
		lb.setForeground(Color.white);
		lb.setPreferredSize(new Dimension(260,60));
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(310, 60));
		pn.setOpaque(false);
		pn.add(lb);
		gc.gridx = 0;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		add(rpn);
	}

	private P_Console getConsole() {
		if (cons == null) {
			cons = (P_Console)sys.pdc.getPeriph(PeriphDecode.P_CO);
		}
		return cons; // what if still null?
	}

	private P_LinePrinter getPrinter() {
		if (lpt == null) {
			lpt = (P_LinePrinter)sys.pdc.getPeriph(PeriphDecode.P_LP);
		}
		return lpt; // what if still null?
	}

	public void setContents(int v) {
		contentsReg = v & 0377;
		setBits(contents, v);
	}
	public void setAddress(int v) {
		addressReg = v & 01777777;
		setBits(address, v);
	}
	public void setControl(int v) {
		controlReg = v & 077;
		setBits(control, v);
	}
	private void setSense(int v) {
		senseReg = v & 0377;
		setBits(sense, v);
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
	public void setInterrupt(int type) {	// Indicator only
		eintr.setForeground(type == HW2000CCR.EIR_EI ? indLit : indDark);
		iintr.setForeground(type == HW2000CCR.EIR_II ? indLit : indDark);
		// TODO: how to detect "program exception"?
		pgm.setForeground(type == HW2000CCR.EIR_PC ? indLit : indDark);
		repaint();
	}
	public void setProtect(int type) {	// Indicator only
		prot.setForeground(type != 0 ? indLit : indDark);
	}

	// Actions are:
	//	"run"	Run
	//	"stop"	Stop
	//	"init"	Initialize
	//	"boot"	Bootstrap
	//	"clear"	System Clear
	//	"instr"	Instruct
	//	"am2","am3","am4"	Address mode
	// TODO: how many of these are directly sent to core system?
	// (vs. being directly performed on core system object)
	//
	public void setPanelListener(ActionListener lstr) {
		run.removeActionListener(this);
		stop.removeActionListener(this);
		central.removeActionListener(this);
		init.removeActionListener(this);

		// TODO: some of these should always remain ours...
		run.addActionListener(lstr);
		stop.addActionListener(lstr);
		central.addActionListener(lstr);
		init.addActionListener(lstr);

		if (boot != null) {
			boot.removeActionListener(this);
			boot.addActionListener(lstr);
		}
		if (instr != null) {
			instr.removeActionListener(this);
			instr.addActionListener(lstr);
		}
	}

	// Are these ever queried?
	public int getContents() { return 0; }
	public int getAddress() { return 0; }
	public int getControl() { return 0; }
	public boolean getRunStop() { return false; }
	public int getAdrMode() { return 0; }

	private void setBits(LightedButton[] btns, int val) {
		if (btns == null) {
			return;
		}
		for (int x = 0; x < btns.length; ++x) {
			btns[x].setOn((val & 1) != 0);
			val >>= 1;
		}
		repaint();
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
		gc.gridwidth = gbx;
		gc.gridheight = 1;
		gb.setConstraints(npn, gc);
		top.add(npn);

		gc.gridy = 0;
		gc.gridwidth = 1;
		gc.gridheight = 2;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(false);
		gc.gridx = 0;
		gbl.setConstraints(pn, gc);
		npn.add(pn);

		// TODO: some of these need to be managed...
		LightedButton btn = new LightedButton(btnGreenOn, btnWhiteOff, null, -1);
		btn.addActionListener(this);
		btn.setOn(true);
		gc.gridx = 1;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		LightedButton btn2 = btn;
		btn = new LightedButton(btnRedOn, btnRedOff, null, -1);
		btn.addActionListener(this);
		btn2.setNext(btn);
		btn.setNext(btn2);
		gc.gridx = 2;
		gbl.setConstraints(btn, gc);
		npn.add(btn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(false);
		gc.gridx = 3;
		gbl.setConstraints(pn, gc);
		npn.add(pn);

		btn = new LightedButton(btnGreenOn, btnWhiteOff, null, -1);
		btn.setOn(true);
		btn.addActionListener(this);
		gc.gridx = 4;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		btn2 = btn;
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, null, -1);
		btn.addActionListener(this);
		btn2.setNext(btn);
		btn.setNext(btn2);
		gc.gridx = 5;
		gbl.setConstraints(btn, gc);
		npn.add(btn);

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40, 40));
		pn.setOpaque(false);
		gc.gridx = 6;
		gbl.setConstraints(pn, gc);
		npn.add(pn);

		btn = new LightedButton(btnRedOn, btnRedOff, null, 0);
		btn.setOn(true);
		gc.gridx = 7;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		stop = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 40));
		pn.setOpaque(false);
		gc.gridx = 8;
		gbl.setConstraints(pn, gc);
		npn.add(pn);

		ImageIcon icn;

		icn = new ImageIcon(getClass().getResource("icons/fp_init.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 9;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		init = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 40));
		pn.setOpaque(false);
		gc.gridx = 10;
		gbl.setConstraints(pn, gc);
		npn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_boot.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 11;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		boot = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 40));
		pn.setOpaque(false);
		gc.gridx = 12;
		gbl.setConstraints(pn, gc);
		npn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_central.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 13;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		central = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 40));
		pn.setOpaque(false);
		gc.gridx = 14;
		gbl.setConstraints(pn, gc);
		npn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_instr.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 15;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		instr = btn;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 40));
		pn.setOpaque(false);
		gc.gridx = 16;
		gbl.setConstraints(pn, gc);
		npn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_run.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 17;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		run = btn;

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40, 40));
		pn.setOpaque(false);
		gc.gridx = 18;
		gbl.setConstraints(pn, gc);
		npn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_inter.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 19;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		inter = btn;

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 40));
		pn.setOpaque(false);
		gc.gridx = 20;
		gbl.setConstraints(pn, gc);
		npn.add(pn);

		icn = new ImageIcon(getClass().getResource("icons/fp_am2.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 21;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		am2 = btn;
		icn = new ImageIcon(getClass().getResource("icons/fp_am3.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		btn.setOn(true);
		gc.gridx = 22;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		am3 = btn;
		icn = new ImageIcon(getClass().getResource("icons/fp_am4.png"));
		btn = new LightedButton(btnWhiteOn, btnWhiteOff, icn, 0);
		gc.gridx = 23;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		am4 = btn;

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(80, 40));
		pn.setOpaque(false);
		gc.gridx = 24;
		gbl.setConstraints(pn, gc);
		npn.add(pn);

		gc.gridheight = 1;
		eintr = new JLabel("EXTERNAL");
		eintr.setFont(smallFont);
		eintr.setForeground(indDark);
		eintr.setOpaque(false);
		eintr.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 0;
		gc.gridx = 25;
		gc.anchor = GridBagConstraints.NORTH;
		gbl.setConstraints(eintr, gc);
		npn.add(eintr);
		iintr = new JLabel("INTERNAL");
		iintr.setFont(smallFont);
		iintr.setForeground(indDark);
		iintr.setOpaque(false);
		iintr.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 1;
		gc.anchor = GridBagConstraints.SOUTH;
		gbl.setConstraints(iintr, gc);
		npn.add(iintr);

		pgm = new JLabel("PROGRAM");
		pgm.setFont(smallFont);
		pgm.setForeground(indDark);
		pgm.setOpaque(false);
		pgm.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 0;
		gc.gridx = 26;
		gc.anchor = GridBagConstraints.NORTH;
		gbl.setConstraints(pgm, gc);
		npn.add(pgm);
		prot = new JLabel("PROTECT");
		prot.setFont(smallFont);
		prot.setForeground(indDark);
		prot.setOpaque(false);
		prot.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 1;
		gc.anchor = GridBagConstraints.SOUTH;
		gbl.setConstraints(prot, gc);
		npn.add(prot);

		parity = new JLabel("PARITY");
		parity.setFont(smallFont);
		parity.setForeground(indDark);
		parity.setOpaque(false);
		parity.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 0;
		gc.gridx = 27;
		gc.anchor = GridBagConstraints.NORTH;
		gbl.setConstraints(parity, gc);
		npn.add(parity);
		voltage = new JLabel("VOLTAGE");
		voltage.setFont(smallFont);
		voltage.setForeground(indDark);
		voltage.setOpaque(false);
		voltage.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 1;
		gc.anchor = GridBagConstraints.SOUTH;
		gbl.setConstraints(voltage, gc);
		npn.add(voltage);

		fan = new JLabel("FAN");
		fan.setFont(smallFont);
		fan.setForeground(indDark);
		fan.setOpaque(false);
		fan.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 0;
		gc.gridx = 28;
		gc.anchor = GridBagConstraints.NORTH;
		gbl.setConstraints(fan, gc);
		npn.add(fan);
		cb = new JLabel("CB");
		cb.setFont(smallFont);
		cb.setForeground(indDark);
		cb.setOpaque(false);
		cb.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 1;
		gc.anchor = GridBagConstraints.SOUTH;
		gbl.setConstraints(cb, gc);
		npn.add(cb);
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
		// will just any width work?
		pn.setPreferredSize(new Dimension((d * 30) + ((d + 1) / 3 * 10) + 20, 40));
		gb.setConstraints(pn, gc);
		top.add(pn);
	}

	private void doBootStrap() {
		byte c1 = (byte)011;
		byte c2 = (byte)contentsReg;
		sys.SR = addressReg;
		sys.AAR = addressReg;
		sys.BAR = addressReg;
		setCtrlReg(c1, addressReg);
		sys.CTL.setV(c1);
		sys.setXtra(new byte[]{c1, c2});
		Instruction op_exec = sys.idc.getExec(InstrDecode.OP_PDT);
		try {
			// TODO: should this execute in thread?
			sys.bootstrap = true;
			setRunStop(true);
			op_exec.execute(sys);
		} catch (Exception ee) {
			setRunStop(false);
			sys.bootstrap = false;
			warning(this, "Bootstrap", ee.toString());
			return;
		}
	}

	private void endCtrlMode() {
		getConsole().poke();
	}

	private void doCtrlMode() {
		if (dumpOnHalt) {
			dumpOnHalt = false;
			sys.dumpHW(currLow, currHi - 1);
		}
		int state = 0;
		int dc = 0;	// digit count (params)
		int v = 0;	// value, accumulator
		P_Console p = getConsole();
		// must be careful to avoid getting stuck here
		while (true) {
			int c;
			if (typePressed) {
				c = 't';
			} else {
				c = p.inChar(sys);
			}
			if (c < 0) {
				if (state != 0) {
					p.output("!\n");
				}
				return;
			}
			String cEcho = "" + (char)c;
			switch (state) {
			case 0:
				switch (c) {
				case 'A':
					state = c;
					dc = 0;
					v = 0;
					break;
				case 'P':
					state = c;
					dc = 0;
					v = 0;
					break;
				case 'B':
					state = c;
					dc = 0;
					v = 0;
					break;
				case 'L':
					getConsole().setOffline(true);
					state = c;
					dc = 0;
					v = 0;
					break;
				case 'R':
					p.output("R\n");
					setAddress(getCtrlReg((byte)controlReg, 0));
					setContents(sys.rawReadMem(addressReg));
					p.output(String.format("%07o %03o\n",
							addressReg, contentsReg));
					return;
				case 'S':
					p.output("S\n");
					sys.singleStep = true;
					sys.halt = false;
					return;
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
					// TODO: only accept 0-3...
					v = (c & 007);
					dc = 1;
					state = 'I';
					break;
				// TODO: implement this (TYPE button depressed)
				// should not have a keyboard equivalent...
				case 't':
					v = sys.rawReadMem(addressReg);
					setAddress(addressReg + 1);
					setCtrlReg((byte)controlReg, addressReg);
					setContents(v);
					p.output(String.format("%03o", v));
					dc += 4;
					if (dc >= 64) {
						p.output("\n");
						dc = 0;
					} else {
						p.output(" ");
					}
					// TODO: best method to slow-down output.
					// Simulate 10 chars/sec...
					try {
						Thread.sleep(250);
					} catch (Exception ee) {}
					return;
				case '\n':
					p.output("\n");
					return;
				default:
					// TODO: silently ignore?
					//p.output("?\n");
					return;
				}
				// Echo char if OK
				p.output(cEcho);
				if (state != 'I') {
					p.output(" ");
				}
				break;
			case 'I':	// ccc ccc ...
				if (c == '\n') {
					// discard partial data
					p.output("\n");
					return;
				}
				// TODO: only accept 0-3 if dc == 0
				if (c < '0' || c > '7') {
					// suppress echo - ignore
					//p.output("?\n");
					break;
				}
				p.output(cEcho);
				++dc;
				v = (v << 3) | (c & 07);
				if ((dc % 4) == 3) {
					setContents(v);
					sys.rawWriteMem(addressReg, (byte)contentsReg);
					setAddress(addressReg + 1);
					setCtrlReg((byte)controlReg, addressReg);
					++dc;
					if (dc >= 64) {
						p.output("\n");
						return;
					}
					p.output(" ");
				}
				break;
			case 'A':	// A rr aaaaaaa
				if (c == '\n') {
					// discard partial data
					p.output("\n");
					return;
				}
				if (c < '0' || c > '7') {
					// suppress echo - ignore
					//p.output("?\n");
					break;
				}
				p.output(cEcho);
				++dc;
				v = (v << 3) | (c & 07);
				if (dc == 2) {
					p.output(" ");
					setControl(v);
					v = 0;
				} else if (dc == 9) {
					setAddress(v);
					setCtrlReg((byte)controlReg, addressReg);
					p.output("\n");
					return;
				}
				break;
			case 'P':	// P rr / aaaaaaa
				if (c == '\n') {
					// discard partial data
					p.output("\n");
					return;
				}
				if (c < '0' || c > '7') {
					// suppress echo - ignore
					// p.output("?\n");
					break;
				}
				p.output(cEcho);
				++dc;
				v = (v << 3) | (c & 07);
				if (dc == 2) {
					setControl(v);
					v = getCtrlReg((byte)controlReg, 0);
					setAddress(v);
					p.output("\n");
					p.output(String.format("%07o\n", v));
					return;
				}
				break;
			case 'B':	// B pp aaaaaaa
				if (c == '\n') {
					// discard partial data
					p.output("\n");
					return;
				}
				if (c < '0' || c > '7') {
					// suppress echo - ignore
					// p.output("?\n");
					break;
				}
				p.output(cEcho);
				++dc;
				v = (v << 3) | (c & 07);
				if (dc == 2) {
					p.output(" ");
					setContents(v);
					v = 0;
				} else if (dc == 9) {
					setAddress(v);
					p.output("\n");
					doBootStrap();
					return;
				}
				break;
			case 'L':
				p.output(cEcho);
				if (c == '\n') {
					getConsole().setOffline(false);
					return;
				}
				break;
			default:
				// Probably never get here
				// p.output("?\n");
				return;
			}
		}
	}

	private void showAbout() {
	       java.net.URL url = this.getClass().getResource("docs/About.html");
	       try {
			JEditorPane about = new JEditorPane(url);
			about.setEditable(false);
			Dimension dim = new Dimension(570, 400);
			about.setPreferredSize(dim);
			JOptionPane.showMessageDialog(this, about,
				"About: " + getTitle(), JOptionPane.PLAIN_MESSAGE);
		} catch (Exception ee) { }
	}

	private void showHelp() {
		help.setVisible(true);
	}

	public void doStop() {
		sys.bootstrap = false;
		sys.halt = true;
		sys.endWait();
	}

	public void doRun() {
		// TODO: need to restore program (interrupt) state?
		sys.halt = false;
		endCtrlMode();
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
		if (id == -1) {
			lb.setOn(true);
		} else if (id != 0) {
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
				doStop();
			} else if (a.equals("inter")) {
				getConsole().setInterrupt(sys);
				sys.endWait();
			} else if (sys.halt) {
				if (a.equals("run")) {
					// Restore program state if needed...
					byte eir = sys.CTL.peekCR(HW2000CCR.EIR);
					eir &= (HW2000CCR.EIR_EI | HW2000CCR.EIR_II);
					if ((eir & HW2000CCR.EIR_EI) != 0) {
						// might be both... EI overrides
						eir = HW2000CCR.EIR_EI;
					}
					setInterrupt(eir);
					doRun();
				} else if (a.equals("instr")) {
					sys.singleStep = true;
					sys.halt = false;
					endCtrlMode();
				} else if (a.equals("clear")) {
					// nothing of interest to do?
				} else if (a.equals("init")) {
					// TODO: simulate "lamp test" function?
					monitor = false;
					mi_mon.setEnabled(true);
					sys.reset();
					setAddress(sys.SR);
					setContents(sys.rawReadMem(addressReg));
					setInterrupt(-1);
					currLow = 0;
					currHi = 0;
				} else if (a.equals("boot")) {
					doBootStrap();
					endCtrlMode();
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

	private void resCopy(String res, SequentialRecordIO p) {
		try {
			InputStream r = getClass().getResourceAsStream(res);
			int n = r.available();
			byte[] buf = new byte[n];
			r.read(buf);
			p.appendBulk(buf, 0, n);
		} catch (Exception ee) {
			// TODO: show error?
		}
	}

	private void posTo(boolean copy, byte[] targ, SequentialRecordIO p) {
		while (true) {
			byte[] b = p.nextRecord();
			if (b == null) {
				return;
			}
			if (b.length >= targ.length) {
				int x = 0;
				for (x = 0; x < targ.length && targ[x] == b[x]; ++x);
				if (x == targ.length) {
					p.backspace();
					return;
				}
			}
			if (copy) {
				p.appendRecord(b, 0, -1);
			}
		}
	}

	private void fortranFile() {
		if (ftn != null) {
			ftn.kill();
			ftn = null;
		}
		String op = "FORTRAN IV";
		File src = pickFile(op + " Program",
				"f4", "FORTRAN", _last);
		if (src == null) {
			return;
		}
		// The rest of this should be executed from a separate thread!
		ftn = new CompileFortran(src);
	}

	class AssembleEZC implements Runnable {
		File src;
		String op;
		Thread t;
		public AssembleEZC(File src, String op) {
			this.src = src;
			this.op = op;
			t = new Thread(this);
			t.start();
		}
		public void run() {
			try {
				boolean ok = doAsm(src, op);
				if (op.equals("Monitor")) {
					// run automatically?
					// set only after running?
					mi_mon.setEnabled(!ok);
				}
			} catch (Exception ee) {
				ee.printStackTrace();
				warning(HW2000FrontPanel.this, op, ee.toString());
			}
			asm = null;
		}
		public void kill() {
			try {
				t.interrupt();
			} catch (Exception ee) {}
		}
	}

	class CompileFortran implements Runnable {
		File src;
		Thread t;
		public CompileFortran(File src) {
			this.src = src;
			t = new Thread(this);
			t.start();
		}
		public void run() {
			try {
				doFortran(src);
			} catch (Exception ee) {
				ee.printStackTrace();
				warning(HW2000FrontPanel.this, "FORTRAN", ee.toString());
			}
			ftn = null;
		}
		public void kill() {
			try {
				t.interrupt();
			} catch (Exception ee) {}
		}
	}

	private void doFortran(File src) {
		String op = "FORTRAN IV";
		File lst = null;
		File ezc = null;
		_last = src;
		String l = src.getAbsolutePath();
		if (l.endsWith(".f4")) {
			l = l.substring(0, l.length() - 3);
		}
		ezc = new File(l + ".ezc");
		if (listing) {
			lst = new File(l + ".lst");
			FileOutputStream lstf;
			try {
				lstf = new FileOutputStream(lst);
			} catch (Exception ee) {
				warning(this, op, ee.toString());
				return;
			}
			getPrinter().setOutput(lstf);
		}
		Compiler cmp;
		try {
			Class<?> clazz = Class.forName("Fortran4");
			Constructor<?> ctor = clazz.getConstructor(File.class);
			cmp = (Compiler)ctor.newInstance(src);
		} catch (Exception ee) {
ee.printStackTrace();
			return;
		}
		int e = cmp.compile(sys, listing);
		if (e >= 0) {
			e = cmp.generate(ezc);
		}
		if (e < 0) {
			warning(this, op, "<HTML><PRE>" + cmp.getErrors() + "</PRE></HTML>");
			return;
		}
		dumpOnHalt = listing && cmp.wantsDump();
		Assembler ret = assemble(ezc, op, listing && cmp.listEasyCoder());
		if (ret != null) {
			if (listing && cmp.listSymbols()) {
				genMemoryMap(cmp.getSymTab(), ret.getSymTab());
			}
			inform(this, op, String.format("Compile complete. %07o %07o %07o",
				currLow, currHi, sys.SR));
			if (cmp.hasData()) {
				P_CardReaderPunch cp = (P_CardReaderPunch)sys.pdc.getPeriph(PeriphDecode.P_PP);
				cp.addInput(new CardInputStream(cmp.getData(), sys.pdc.cvt),
						src.getName(), cmp.lineCount());
				cp.visible(true);
			}
		}
	}

	private void asmFile(String op) {
		if (asm != null) {
			asm.kill();
			asm = null;
		}
		File src = pickFile(op + " Program",
				"ezc", "EasyCoder", _last);
		if (src == null) {
			return;
		}
		// The rest of this should be executed from a separate thread!
		asm = new AssembleEZC(src, op);
	}

	private boolean doAsm(File src, String op) {
		OutputStream lst = null;
		_last = src;
		if (listing) {
			String l = src.getAbsolutePath();
			if (l.endsWith(".ezc")) {
				l = l.substring(0, l.length() - 4);
			}
			l += ".lst";
			try {
				lst = new FileOutputStream(new File(l));
			} catch (Exception ee) {
				warning(this, op, ee.toString());
				return false;
			}
			getPrinter().setOutput(lst);
		}
		Assembler ret = assemble(src, op, listing);
		getPrinter().setOutput(null);
		if (lst != null) {
			try { lst.close(); } catch (Exception ee) {}
		}
		if (ret != null) {
			inform(this, op, String.format("Assembly complete. %07o %07o %07o",
				currLow, currHi, sys.SR));
		}
		return (ret != null);
	}

	private void genMemoryMap(Map<String, String> cmpMap,
				Map<String, Integer> asmMap) {
		Map<Integer, String> out = new TreeMap<Integer, String>();
		for (Map.Entry<String, String> entry : cmpMap.entrySet()) {
			String s1 = entry.getKey();
			String s2 = entry.getValue();
			// TODO: find out where these come from...
			if (s2.matches("\\(.*\\)")) {
				s2 = s2.replaceAll("[\\(\\)]", "");
			}
			int k = 07777777;
			if (asmMap.containsKey(s2)) {
				k = asmMap.get(s2);
			}
			String s3 = String.format("%15s  %07o  %-15s",
					s2, k, s1.equals(s2) ? "" : s1);
			out.put(k, s3);
		}
		listOut("\nMemory Map\n         Symbol  Address  Alternate\n");
		for (Map.Entry<Integer, String> entry : out.entrySet()) {
			listOut(String.format("%s\n", entry.getValue()));
		}
	}

	private Assembler assemble(File src, String op, boolean doList) {
		Assembler asm = new Assembler(src);
		int e = asm.passOne();
		if (e < 0) {
			warning(this, op, "<HTML><PRE>" + asm.getErrors() + "</PRE></HTML>");
			return null;
		}
		Peripheral p = null;
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
		// TODO: card BRT also...
		if (tape) {
			// Mag Tape BRT format... TODO: select unit
			int unit = 0;
			P_MagneticTape tp = (P_MagneticTape)sys.pdc.getPeriph(PeriphDecode.P_MT);
			// TODO: are these ever null?
			boolean copy = !tp.begin(unit); // always 'true' (!copy)?
			if (!tp.rewind()) {
				warning(this, op, "No tape mounted");
				return null;
			}
			// TODO: need option to overwrite tape
			if (tp.empty()) {
				tp.appendRecord(_1HDR, 0, -1);
				resCopy("bringup/bootmt.mti", tp);
			} else {
				// Find "1EOF " (or EOT)
				// TODO: fail-safe "1ERI "?
				posTo(copy, _1EOF, tp);
			}
			// TODO: Allow cards vs. tape
			e = asm.passTwo(new PeriphLoader(tp, asm.charCvt(), 250),
					doList ? (CoreMemory)sys : null);
			tp.appendRecord(_1EOF, 0, -1);
			tp.appendRecord(_1ERI, 0, -1);
			tp.appendRecord(_1ERI, 0, -1);
			tp.end();
		} else {
			// TODO: might try to execute from here...
			// Need hooks back to this class...
			e = asm.passTwo(new CoreLoader(sys, this), reloc, doList);
		}
		if (e < 0) {
			warning(this, op, "<HTML><PRE>" + asm.getErrors() + "</PRE></HTML>");
			return null;
		}
		if (doList) {
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
		} else {
			sys.SR = start;
		}
		setAddress(sys.SR);
		setContents(sys.rawReadMem(addressReg));
		currLow = reloc + low;
		currHi = reloc + hi;
		return asm;
	}

	public void listOut(String str) {
		getPrinter().output(str);
	}

	private int dumpDialog(String name) {
		JOptionPane dump_dia;
		dump_dia = new JOptionPane(dump_pn, JOptionPane.QUESTION_MESSAGE, JOptionPane.YES_NO_OPTION, null, dump_btns);
		dump_lo.setText(Integer.toString(currLow));
		dump_hi.setText(Integer.toString(currHi));
		Dialog dlg = dump_dia.createDialog(this, name);
		dlg.setVisible(true);
		Object res = dump_dia.getValue();
		if (dump_btns[OPTION_CANCEL].equals(res)) return 0;
		if (dump_btns[OPTION_YES].equals(res)) {
			try {
				if (dump_lo.getText().length() > 0) {
					dumpLow = Integer.valueOf(dump_lo.getText());
				} else {
					dumpLow = currLow;
				}
				if (dump_hi.getText().length() > 0) {
					dumpHi = Integer.valueOf(dump_hi.getText());
				} else {
					dumpHi = currHi;
				}
			} catch (Exception ee) {
				return 0;
			}
			return 1;
		}
		return 0;
	}

	private void performMenu(JMenuItem mi) {
		if (mi.getMnemonic() == KeyEvent.VK_A) {
			// This should NOT be run in the event thread!
			asmFile("Assemble");
		} else if (mi.getMnemonic() == KeyEvent.VK_F) {
			// This should NOT be run in the event thread!
			fortranFile();
		} else if (mi.getMnemonic() == KeyEvent.VK_M) {
			// run automatically?
			// set 'true' only after running?
			asmFile("Monitor");
		} else if (mi.getMnemonic() == KeyEvent.VK_Q) {
			System.exit(0);
		} else if (mi.getMnemonic() == KeyEvent.VK_C) {
			getConsole().visible(true);
		} else if (mi.getMnemonic() == KeyEvent.VK_P) {
			getPrinter().visible(true);
		} else if (mi.getMnemonic() == KeyEvent.VK_G) {
			Peripheral p = sys.pdc.getPeriph(PeriphDecode.P_MT);
			if (p != null) {
				p.visible(true);
			}
		} else if (mi.getMnemonic() == KeyEvent.VK_H) {
			Peripheral p = sys.pdc.getPeriph(PeriphDecode.P_PP);
			if (p != null) {
				p.visible(true);
			}
		} else if (mi.getMnemonic() == KeyEvent.VK_K) {
			Peripheral p = sys.pdc.getPeriph(PeriphDecode.P_DK);
			if (p != null) {
				p.visible(true);
			}
		} else if (mi.getMnemonic() == KeyEvent.VK_T) {
			sys.setTrace(currLow, currHi);
		} else if (mi.getMnemonic() == KeyEvent.VK_L) {
			dump_btns[OPTION_YES] = "Trace On";
			if (dumpDialog("Trace Parameters") > 0) {
				sys.setTrace(dumpLow, dumpHi);
			}
		} else if (mi.getMnemonic() == KeyEvent.VK_O) {
			sys.setTrace(0, 0);
		} else if (mi.getMnemonic() == KeyEvent.VK_D) {
			if (currLow < currHi) {
				sys.dumpHW(currLow, currHi - 1);
			}
		} else if (mi.getMnemonic() == KeyEvent.VK_N) {
			dump_btns[OPTION_YES] = "Dump";
			if (dumpDialog("Dump Parameters") > 0) {
				sys.dumpHW(dumpLow, dumpHi);
			}
		} else if (mi.getMnemonic() == KeyEvent.VK_I) {
			showAbout();
		} else if (mi.getMnemonic() == KeyEvent.VK_E) {
			showHelp();
		}

	}

	public void stateChanged(ChangeEvent evt) {
		if (!(evt.getSource() instanceof JButton)) {
			return;
		}
		JButton btn = (JButton)evt.getSource();
		if (btn == init) {
			if (btn.getModel().isPressed()) {
				lampTest(true);
			} else {
				lampTest(false);
			}
		} else if (btn == type) {
			typePressed = btn.getModel().isPressed();
			if (typePressed) {
				endCtrlMode();
			}
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

	boolean inLampTest = false;
	private void lampTest(boolean test) {
		if (test == inLampTest) {
			return;
		}
		inLampTest = test;
		LightedButton.doLampTest(test);
		Color ind = (test ? indLit : indDark);
		eintr.setForeground(ind);
		iintr.setForeground(ind);
		pgm.setForeground(ind);
		prot.setForeground(ind);
		parity.setForeground(ind);
		voltage.setForeground(ind);
		fan.setForeground(ind);
		cb.setForeground(ind);
	}

	public void run() {
		while (true) {
			if (sys.bootstrap) {
				// Pause, for aesthetics
				try {
					Thread.sleep(100);
				} catch (Exception ee) {}
				sys.waitIO();
				setRunStop(false);
				sys.bootstrap = false;
			} else if (sys.halt) {
				doCtrlMode();
			} else {
				sys.run();
			}
		}
	}
}
