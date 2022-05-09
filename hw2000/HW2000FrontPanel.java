// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.util.Map;
import java.util.TreeMap;
import java.util.Properties;
import java.util.Arrays;
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
	JLabel active;
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
	JMenuItem mi_thr;
	JMenuItem mi_trc;

	int gbx;
	File _last = null;
	boolean typePressed = false;
	boolean listing = false;
	boolean tape = false;
	boolean disk = false;
	boolean monitor = false;
	boolean throttled = false;
	boolean trcCon = false;
	boolean fortran = false;
	boolean dumpOnHalt = false;
	CompileFortran ftn = null;
	AssembleEZC asm = null;
	int currLow = 0;
	int currHi = 0;

	static final int OPTION_CANCEL = 0;
	static final int OPTION_YES = 1;
	private Object[] dump_btns;
	private JTextField dump_lo;
	private JTextField dump_hi;
	private JPanel dump_lo_pn;
	private JPanel dump_hi_pn;
	private JCheckBox dump_rx;
	private JPanel dump_pn;
	//
	private UtilInitialize vol_pn;
	private UtilMapVolume map_pn;
	private UtilAllocate all_pn;
	private UtilDeallocate rel_pn;
	private UtilBootstrapGen bsg_pn;
	private UtilExecutable xbl_pn;
	//
	int dumpLow;
	int dumpHi;
	private P_Console cons;
	private P_LinePrinter lpt;
	private JCheckBox lst;
	private JCheckBox mti;
	JCheckBox dpi;
	JTextField dpi_lun;
	JTextField dpi_rev;
	JTextField dpi_vis;
	JCheckBox dpi_bsp;
	ButtonGroup go_bg;
	JRadioButton go_go;
	JRadioButton go_res;
	private JPanel acc; // Accessories for file chooser

	public HW2000FrontPanel(Properties props, HW2000 sys) {
		super("Honeywell Series 2000");
		setIconImage(Toolkit.getDefaultToolkit().
			getImage(getClass().getResource("icons/sys-96.png")));
		LightedButton.init(64);
		this.sys = sys; // may be null
		_last = new File(System.getProperty("user.dir"));

		getContentPane().setBackground(Color.black);
		bigFont = new Font("Sans-Serif", Font.PLAIN, 40);
		medFont = new Font("Sans-Serif", Font.PLAIN, 28);
		smallFont = new Font("Sans-Serif", Font.PLAIN, 8);

		JPanel pn;
		makeFileAccessory();

		setLayout(new FlowLayout(FlowLayout.CENTER, 0, 0));

		String s = props.getProperty("console");
		if (s != null && s.equals("220-3")) {
			partControlPanel();
			getConsole().setTypeBtn(type);
			getConsole().visible(true);
		} else {
			fullControlPanel();
			type = getConsole().getTypeBtn();
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
		mi = new JMenuItem("Load Obj", KeyEvent.VK_B);
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
		mu = new JMenu("Disk Util");
		mi = new JMenuItem("Initialize Volume", KeyEvent.VK_V);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Map Volume", KeyEvent.VK_S);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Allocate File", KeyEvent.VK_0);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Deallocate File", KeyEvent.VK_1);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Bootstrap Generator", KeyEvent.VK_2);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Executable Function", KeyEvent.VK_3);
		mi.addActionListener(this);
		mu.add(mi);
		mb.add(mu);
		mu = new JMenu("Debug");
		mi = new JMenuItem("Trace to CON", KeyEvent.VK_J);
		mi_trc = mi;
		mi.addActionListener(this);
		mu.add(mi);
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
		mi = new JMenuItem("Throttle On", KeyEvent.VK_X);
		mi_thr = mi;
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Clear Memory", KeyEvent.VK_Z);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Rand Memory", KeyEvent.VK_Y);
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

		vol_pn = new UtilInitialize(sys);
		map_pn = new UtilMapVolume(sys);
		all_pn = new UtilAllocate(sys);
		rel_pn = new UtilDeallocate(sys);
		bsg_pn = new UtilBootstrapGen(sys);
		xbl_pn = new UtilExecutable(sys);

		// Dialog for Dump Full / Trace Full
		dump_pn = new JPanel();
		dump_pn.setLayout(new BoxLayout(dump_pn, BoxLayout.Y_AXIS));
		dump_btns = new Object[2];
		dump_btns[OPTION_YES] = "Dump";
		dump_btns[OPTION_CANCEL] = "Cancel";
		dump_rx = new JCheckBox("Octal (decimal)");
		dump_lo = new JTextField();
		dump_lo.setPreferredSize(new Dimension(200, 20));
		dump_lo_pn = new JPanel();
		dump_lo_pn.add(new JLabel("Low Adr:"));
		dump_lo_pn.add(dump_lo);
		dump_hi = new JTextField();
		dump_hi.setPreferredSize(new Dimension(200, 20));
		dump_hi_pn = new JPanel();
		dump_hi_pn.add(new JLabel("High Adr:"));
		dump_hi_pn.add(dump_hi);
		dump_pn.add(dump_rx);
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

	private void makeFileAccessory() {
		// TODO: add Visibility and Revision Number (both Tape and Disk).
		JPanel pn;
		lst = new JCheckBox("Listing");
		mti = new JCheckBox("Tape Image");
		dpi = new JCheckBox("Disk Image");
		dpi_lun = new JTextField("0");
		dpi_lun.setPreferredSize(new Dimension(30, 20));
		dpi_rev = new JTextField("0");
		dpi_rev.setPreferredSize(new Dimension(30, 20));
		dpi_vis = new JTextField("A");
		dpi_vis.setPreferredSize(new Dimension(50, 20));
		go_bg = new ButtonGroup();
		go_go = new JRadioButton("*DRS1GO");
		go_res = new JRadioButton("*DRS1RES");
		go_bg.add(go_go);
		go_bg.add(go_res);
		dpi_bsp = new JCheckBox("Bootstrap");
		dpi_bsp.setEnabled(false); // until we find a use for it

		acc = new JPanel();
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
		gc.anchor = GridBagConstraints.WEST;
		GridBagLayout gb = new GridBagLayout();
		acc.setLayout(gb);

		gc.gridy = 0;
		gb.setConstraints(lst, gc);
		acc.add(lst);
		++gc.gridy;
		JSeparator sp = new JSeparator();
		sp.setPreferredSize(new Dimension(100, 3));
		gb.setConstraints(sp, gc);
		acc.add(sp);
		++gc.gridy;
		pn = new JPanel();
		pn.setLayout(new BoxLayout(pn, BoxLayout.X_AXIS));
		pn.add(new JLabel("Unit: "));
		pn.add(dpi_lun);
		gb.setConstraints(pn, gc);
		acc.add(pn);
		++gc.gridy;
		pn = new JPanel();
		pn.setLayout(new BoxLayout(pn, BoxLayout.X_AXIS));
		pn.add(new JLabel("Rev: "));
		pn.add(dpi_rev);
		gb.setConstraints(pn, gc);
		acc.add(pn);
		++gc.gridy;
		pn = new JPanel();
		pn.setLayout(new BoxLayout(pn, BoxLayout.X_AXIS));
		pn.add(new JLabel("Vis: "));
		pn.add(dpi_vis);
		gb.setConstraints(pn, gc);
		acc.add(pn);
		++gc.gridy;
		sp = new JSeparator();
		sp.setPreferredSize(new Dimension(100, 3));
		gb.setConstraints(sp, gc);
		acc.add(sp);
		++gc.gridy;
		gb.setConstraints(mti, gc);
		acc.add(mti);
		++gc.gridy;
		sp = new JSeparator();
		sp.setPreferredSize(new Dimension(100, 3));
		gb.setConstraints(sp, gc);
		acc.add(sp);
		++gc.gridy;
		gb.setConstraints(dpi, gc);
		acc.add(dpi);
		++gc.gridy;
		gc.anchor = GridBagConstraints.EAST;
		gb.setConstraints(go_go, gc);
		acc.add(go_go);
		++gc.gridy;
		gb.setConstraints(go_res, gc);
		acc.add(go_res);
		++gc.gridy;
		gb.setConstraints(dpi_bsp, gc);
		acc.add(dpi_bsp);
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
		LightedButton btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, btnContents | btnClear);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, btnAddress | btnClear);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, btnContents | btnDisplay);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, btnContents | btnEnter);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, btnAddress | btnDisplay);
		btn.addActionListener(this);
		btn.setToolTipText("Control Display");
		gc.gridx = 1;
		gc.gridy = 6;
		gc.gridwidth = 1;
		gc.gridheight = 3;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		icn = new ImageIcon(getClass().getResource("icons/fp_plus1.png"));
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, btnAddress | btnDispP1);
		btn.addActionListener(this);
		btn.setToolTipText("Control Display +1");
		gc.gridx = 2;
		gc.gridy = 6;
		gc.gridwidth = 1;
		gc.gridheight = 3;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		icn = new ImageIcon(getClass().getResource("icons/fp_minus1.png"));
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, btnAddress | btnDispM1);
		btn.addActionListener(this);
		btn.setToolTipText("Control Display -1");
		gc.gridx = 3;
		gc.gridy = 6;
		gc.gridwidth = 1;
		gc.gridheight = 3;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		icn = new ImageIcon(getClass().getResource("icons/fp_enter.png"));
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, btnAddress | btnEnter);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 7);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 8");
		gc.gridx = 1;
		gc.gridy = 11;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[7] = btn;
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 6);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 7");
		gc.gridx = 2;
		gc.gridy = 11;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[6] = btn;
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 5);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 6");
		gc.gridx = 3;
		gc.gridy = 11;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[5] = btn;
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 4);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 3);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 4");
		gc.gridx = 1;
		gc.gridy = 12;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[3] = btn;
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 2);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 3");
		gc.gridx = 2;
		gc.gridy = 12;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[2] = btn;
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 1);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 2");
		gc.gridx = 3;
		gc.gridy = 12;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[1] = btn;
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 0);
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
		pn.setPreferredSize(new Dimension(150,20));
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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

		btn = new LightedButton(Peripheral.btnRedOn, Peripheral.btnRedOff, null, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 7);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 6);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 5);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 4);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 5");
		gc.gridx = 7;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[4] = btn;

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(50,100));
		pn.setOpaque(false);
		active = new JLabel("<HTML><CENTER>SYSTEM<BR>ACTIVE</CENTER></HTML>");
		active.setFont(smallFont);
		active.setForeground(Peripheral.indDark);
		active.setOpaque(false);
		// { All this effort just to get the JLabel centered V & H...
		GridBagLayout gb2 = new GridBagLayout();
		pn.setLayout(gb2);
		GridBagConstraints gc2 = new GridBagConstraints();
		gc2.fill = GridBagConstraints.NONE;
		gc2.gridx = 0;
		gc2.gridy = 0;
		gc2.weightx = 0;
		gc2.weighty = 0;
		gc2.gridwidth = 1;
		gc2.gridheight = 1;
		gc2.insets.bottom = 0;
		gc2.insets.top = 0;
		gc2.insets.left = 0;
		gc2.insets.right = 0;
		gc2.anchor = GridBagConstraints.CENTER;
		gb2.setConstraints(active, gc2);
		pn.add(active);
		// } ...
		gc.gridx = 8;
		gc.gridheight = 3;
		gb.setConstraints(pn, gc);
		rpn.add(pn);
		gc.gridheight = 1;

		btn = new LightedButton(Peripheral.btnGreenOn, Peripheral.btnWhiteOff, null, -1); // A/C ON
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
		btn = new LightedButton(Peripheral.btnRedOn, Peripheral.btnRedOff, null, -1);	// A/C OFF
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
		pn.setPreferredSize(new Dimension(120,20));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 2;
		gc.gridwidth = 8;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		// spacer panel done on gridy 1

		pn = new JPanel();
		pn.setPreferredSize(new Dimension(90,20));
		pn.setOpaque(false);
		gc.gridx = 9;
		gc.gridwidth = 4;
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 3);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 2);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 1);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, btnSense | 0);
		btn.addActionListener(this);
		btn.setToolTipText("SENSE 1");
		gc.gridx = 7;
		gb.setConstraints(btn, gc);
		rpn.add(btn);
		sense[0] = btn;

		// spacer panel done on gridy 1

		btn = new LightedButton(Peripheral.btnGreenOn, Peripheral.btnWhiteOff, null, -1); // DC ON
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, -1); // DC OFF
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
		pn.setPreferredSize(new Dimension(310,10));
		pn.setOpaque(false);
		gc.gridx = 0;
		gc.gridy = 4;
		gc.gridwidth = gbx;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		pn = new JPanel();
		gb2 = new GridBagLayout();
		pn.setLayout(gb2);
		pn.setOpaque(false);
		gc2.gridx = 0;
		gc2.gridy = 0;
		gc2.gridwidth = 1;
		gc2.gridheight = 1;
		eintr = new JLabel("EXTERNAL");
		eintr.setFont(smallFont);
		eintr.setForeground(Peripheral.indDark);
		eintr.setOpaque(false);
		eintr.setPreferredSize(new Dimension(50, 15));
		gc2.anchor = GridBagConstraints.NORTH;
		gb2.setConstraints(eintr, gc2);
		pn.add(eintr);
		iintr = new JLabel("INTERNAL");
		iintr.setFont(smallFont);
		iintr.setForeground(Peripheral.indDark);
		iintr.setOpaque(false);
		iintr.setPreferredSize(new Dimension(50, 15));
		gc2.gridy = 1;
		gc2.anchor = GridBagConstraints.SOUTH;
		gb2.setConstraints(iintr, gc2);
		pn.add(iintr);

		gc2.gridy = 0;
		gc2.gridx = 1;
		pgm = new JLabel("PROGRAM");
		pgm.setFont(smallFont);
		pgm.setForeground(Peripheral.indDark);
		pgm.setOpaque(false);
		pgm.setPreferredSize(new Dimension(50, 15));
		gc2.anchor = GridBagConstraints.NORTH;
		gb2.setConstraints(pgm, gc2);
		pn.add(pgm);
		prot = new JLabel("PROTECT");
		prot.setFont(smallFont);
		prot.setForeground(Peripheral.indDark);
		prot.setOpaque(false);
		prot.setPreferredSize(new Dimension(50, 15));
		gc2.gridy = 1;
		gc2.anchor = GridBagConstraints.SOUTH;
		gb2.setConstraints(prot, gc2);
		pn.add(prot);

		gc2.gridy = 0;
		gc2.gridx = 2;
		voltage = new JLabel("VOLTAGE");
		voltage.setFont(smallFont);
		voltage.setForeground(Peripheral.indDark);
		voltage.setOpaque(false);
		voltage.setPreferredSize(new Dimension(50, 15));
		gc2.anchor = GridBagConstraints.NORTH;
		gb2.setConstraints(voltage, gc2);
		pn.add(voltage);
		parity = new JLabel("PARITY");
		parity.setFont(smallFont);
		parity.setForeground(Peripheral.indDark);
		parity.setOpaque(false);
		parity.setPreferredSize(new Dimension(50, 15));
		gc2.gridy = 1;
		gc2.anchor = GridBagConstraints.SOUTH;
		gb2.setConstraints(parity, gc2);
		pn.add(parity);

		gc2.gridy = 0;
		gc2.gridx = 3;
		fan = new JLabel("FAN");
		fan.setFont(smallFont);
		fan.setForeground(Peripheral.indDark);
		fan.setOpaque(false);
		fan.setPreferredSize(new Dimension(50, 15));
		gc2.anchor = GridBagConstraints.NORTH;
		gb2.setConstraints(fan, gc2);
		pn.add(fan);
		cb = new JLabel("CB");
		cb.setFont(smallFont);
		cb.setForeground(Peripheral.indDark);
		cb.setOpaque(false);
		cb.setPreferredSize(new Dimension(50, 15));
		gc2.gridy = 1;
		gc2.anchor = GridBagConstraints.SOUTH;
		gb2.setConstraints(cb, gc2);
		pn.add(cb);

		gc.gridheight = 3;
		gc.gridwidth = gbx;
		gc.gridy = 5;
		gc.gridx = 0;
		gb.setConstraints(pn, gc);
		rpn.add(pn);

		gc.gridy = 8;
		// fudge spacing, instead of more-complex paneling
		JLabel lb = new JLabel("Honeywell 2000");
		lb.setFont(medFont);
		lb.setOpaque(false);
		lb.setForeground(Color.white);
		lb.setPreferredSize(new Dimension(260,32));
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(310, 40));
		pn.setOpaque(false);
		pn.add(lb);
		gc.anchor = GridBagConstraints.NORTH;
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
		eintr.setForeground(type == HW2000CCR.EIR_EI ? Peripheral.indLit : Peripheral.indDark);
		iintr.setForeground(type == HW2000CCR.EIR_II ? Peripheral.indLit : Peripheral.indDark);
		repaint();
	}
	public void setProtect(int type) {	// Indicator only
		prot.setForeground(type != 0 ? Peripheral.indLit : Peripheral.indDark);
	}
	public void setActive(boolean on) {	// Indicator only
		if (active != null) {
			active.setForeground(on ? Peripheral.indLit : Peripheral.indDark);
		}
	}
	public void setProgram(boolean on) {	// Indicator only
		// TODO: how to detect "program exception"?
		pgm.setForeground(on ? Peripheral.indLit : Peripheral.indDark);
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
		LightedButton btn = new LightedButton(Peripheral.btnGreenOn, Peripheral.btnWhiteOff, null, -1);
		btn.addActionListener(this);
		btn.setOn(true);
		gc.gridx = 1;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		LightedButton btn2 = btn;
		btn = new LightedButton(Peripheral.btnRedOn, Peripheral.btnRedOff, null, -1);
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

		btn = new LightedButton(Peripheral.btnGreenOn, Peripheral.btnWhiteOff, null, -1);
		btn.setOn(true);
		btn.addActionListener(this);
		gc.gridx = 4;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		btn2 = btn;
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, -1);
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

		btn = new LightedButton(Peripheral.btnRedOn, Peripheral.btnRedOff, null, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
		gc.gridx = 21;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		am2 = btn;
		icn = new ImageIcon(getClass().getResource("icons/fp_am3.png"));
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
		btn.setOn(true);
		gc.gridx = 22;
		gbl.setConstraints(btn, gc);
		npn.add(btn);
		am3 = btn;
		icn = new ImageIcon(getClass().getResource("icons/fp_am4.png"));
		btn = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, icn, 0);
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
		eintr.setForeground(Peripheral.indDark);
		eintr.setOpaque(false);
		eintr.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 0;
		gc.gridx = 25;
		gc.anchor = GridBagConstraints.NORTH;
		gbl.setConstraints(eintr, gc);
		npn.add(eintr);
		iintr = new JLabel("INTERNAL");
		iintr.setFont(smallFont);
		iintr.setForeground(Peripheral.indDark);
		iintr.setOpaque(false);
		iintr.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 1;
		gc.anchor = GridBagConstraints.SOUTH;
		gbl.setConstraints(iintr, gc);
		npn.add(iintr);

		pgm = new JLabel("PROGRAM");
		pgm.setFont(smallFont);
		pgm.setForeground(Peripheral.indDark);
		pgm.setOpaque(false);
		pgm.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 0;
		gc.gridx = 26;
		gc.anchor = GridBagConstraints.NORTH;
		gbl.setConstraints(pgm, gc);
		npn.add(pgm);
		prot = new JLabel("PROTECT");
		prot.setFont(smallFont);
		prot.setForeground(Peripheral.indDark);
		prot.setOpaque(false);
		prot.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 1;
		gc.anchor = GridBagConstraints.SOUTH;
		gbl.setConstraints(prot, gc);
		npn.add(prot);

		parity = new JLabel("PARITY");
		parity.setFont(smallFont);
		parity.setForeground(Peripheral.indDark);
		parity.setOpaque(false);
		parity.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 0;
		gc.gridx = 27;
		gc.anchor = GridBagConstraints.NORTH;
		gbl.setConstraints(parity, gc);
		npn.add(parity);
		voltage = new JLabel("VOLTAGE");
		voltage.setFont(smallFont);
		voltage.setForeground(Peripheral.indDark);
		voltage.setOpaque(false);
		voltage.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 1;
		gc.anchor = GridBagConstraints.SOUTH;
		gbl.setConstraints(voltage, gc);
		npn.add(voltage);

		fan = new JLabel("FAN");
		fan.setFont(smallFont);
		fan.setForeground(Peripheral.indDark);
		fan.setOpaque(false);
		fan.setPreferredSize(new Dimension(50, 15));
		gc.gridy = 0;
		gc.gridx = 28;
		gc.anchor = GridBagConstraints.NORTH;
		gbl.setConstraints(fan, gc);
		npn.add(fan);
		cb = new JLabel("CB");
		cb.setFont(smallFont);
		cb.setForeground(Peripheral.indDark);
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
			btns[x] = new LightedButton(Peripheral.btnWhiteOn, Peripheral.btnWhiteOff, null, id | x);
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
		sys.setCtrlReg(c1, addressReg);
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
			PopupFactory.warning(this, "Bootstrap", ee.toString());
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
				c = p.inChar();
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
					v = sys.rawReadMem(addressReg) & 0xff;
					setAddress(addressReg + 1);
					sys.setCtrlReg((byte)controlReg, addressReg);
					setContents(v);
					p.output(String.format("%03o ", v));
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
					sys.setCtrlReg((byte)controlReg, addressReg);
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
					sys.setCtrlReg((byte)controlReg, addressReg);
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
			int cls = id & ~0x0fff;
			int idx = id & 0x0fff;
			if (cls == btnSense) {
				setSense(senseReg ^ (1 << idx));
				return;
			}
			if (run.isOn()) {
				return;
			}
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
					sys.setCtrlReg((byte)controlReg, addressReg);
				} else {
					setAddress(addressReg | (1 << idx));
				}
			} else if (cls == btnControl) {
				setControl(controlReg ^ (1 << idx));
			}
		} else {
			String a = lb.getActionCommand();
			if (a == null) {
				return;
			}
			if (a.equals("stop")) {
				doStop();
			} else if (a.equals("inter")) {
				getConsole().setInterrupt();
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
					setProgram(false); // yes?
				} else if (a.equals("init")) {
					monitor = false;
					mi_mon.setEnabled(true);
					sys.reset();
					setAddress(sys.SR);
					setContents(sys.rawReadMem(addressReg));
					setInterrupt(-1);
					setProtect(0);
					setProgram(false);
					setActive(false);
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
		int val = sys.getCtrlReg(reg);
		boolean fpreg = ((reg & 060) == 040 && (reg & 003) != 0);
		if (!fpreg && incr != 0) {
			sys.setCtrlReg(reg, val + incr); // masked internally
		}
		return val;
	}

	private File pickFile(String purpose, String sfx, String typ, File prev) {
		File file = null;
		listing = false;
		tape = false;
		disk = false;
		// Default to GO file, every time.
		go_go.setSelected(true);
		SuffFileChooser ch = new SuffFileChooser(purpose,
			new String[]{sfx}, new String[]{typ}, prev, acc);
		int rv = ch.showDialog(this);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
			listing = lst.isSelected();
			tape = mti.isSelected();
			disk = dpi.isSelected();
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

	private int ldInt(byte[] b, int s) {
		s *= 4;
		int v = b[s + 3] & 0xff;
		v <<= 8;
		v |= b[s + 2] & 0xff;
		v <<= 8;
		v |= b[s + 1] & 0xff;
		v <<= 8;
		v |= b[s + 0] & 0xff;
		return v;
	}

	// This is a bit of a cheat, as the H200/H2000 cannot
	// load punctuation from I/O.
	private void loadObjFile() {
		String op = "AS200 OBJ";
		RandomAccessFile obj;
		byte[] hdr = new byte[8*4];
		long vis = 0400000000000L;
		int rev = 0;
		File src = pickFile(op + " File",
				"ohw", "AS200 OBJ", _last);
		if (src == null) {
			return;
		}
		if (!dpi_rev.getText().isEmpty()) try {
			rev = Integer.valueOf(dpi_rev.getText());
		} catch (Exception ee) {
			PopupFactory.warning(this, op, "Revision number invalid");
			return;
		}
		if (!dpi_vis.getText().isEmpty()) try {
			vis = UtilExecutable.visibility(dpi_vis.getText());
		} catch (Exception ee) {
			PopupFactory.warning(this, op, "Visibility invalid");
			return;
		}
		try {
			obj = new RandomAccessFile(src, "r");
			obj.read(hdr);
		} catch (Exception ee) {
			PopupFactory.warning(this, op, ee.getMessage());
			return;
		}
		// TODO: other A_xMAGIC values
		if (ldInt(hdr, 0) != 020007) {
			PopupFactory.warning(this, op, src.getName() + ": not object file format: " + hdr[0]);
			try { obj.close(); } catch (Exception ee) {}
			return;
		}
		int adr = ldInt(hdr, 5);	// a_entry
		int txt = ldInt(hdr, 1);	// a_text
		int dat = ldInt(hdr, 2);	// a_data
		int bss = ldInt(hdr, 3);	// a_bss
		int heap = ldInt(hdr, 6);	// a_heap
		// TODO: leave bss as-is, or pad with stack space?
		// TODO: what about heap?
		bss += 256; // user can add more, via -t
		try {
			obj.read(sys.mem, adr, txt + dat);
		} catch (Exception ee) {
			PopupFactory.warning(this, op, ee.getMessage());
		}
		// Equiv of CLEAR directive...
		Arrays.fill(sys.mem, adr + txt + dat, adr + txt + dat + bss, (byte)0);
		try { obj.close(); } catch (Exception ee) {}
		currLow = adr;
		currHi = adr + txt + dat + bss;
		// Put top of .bss in X1... stack pointer
		sys.putAddr(4, currHi, sys.M_WM);
		// Equiv of RANGE directive
		if (heap != 0) {
			currHi += heap;
		} else {
			currHi += 64*1024; // TODO: should there be any default?
		}
		// Since running stand-alone, no need for IBR/BRR or memory
		// allocation, etc.
		// Put data in MOD1 locations...
		sys.putStr(65, String.format("%03d", rev), 3);
		sys.putStr(68, src.getName().split("\\.")[0], 8);
		sys.putRaw(118, vis, 6);
		sys.putAddr(189, currHi, sys.M_WM); // EOM, give program some "heap"
		sys.setWord(65);	// REV
		sys.setWord(68);	// PGM
		sys.setWord(74);	// SEG
		sys.setWord(113);	// VIS
		sys.SR = adr;
		_last = src;
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
					monitor = ok;
				}
			} catch (Exception ee) {
				ee.printStackTrace();
				PopupFactory.warning(HW2000FrontPanel.this, op, ee.toString());
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
				PopupFactory.warning(HW2000FrontPanel.this, "FORTRAN", ee.toString());
			}
			ftn = null;
		}
		public void kill() {
			try {
				t.interrupt();
			} catch (Exception ee) {}
		}
	}

	private int lineCount(File src) {
		BufferedReader in = null;
		try {
			in = new BufferedReader(new FileReader(src));
			int l = 0;
			while (in.readLine() != null) {
				++l;
			}
			return l;
		} catch (Exception ee) {
			return 0;
		} finally {
			if (in != null) try {
				in.close();
			} catch (Exception ee) {}
		}
	}

	private void doFortran(File src) {
		String op = "FORTRAN IV";
		File lst = null;
		File ezc = null;
		_last = src;
		int lc = lineCount(src); // TODO: error if "0"?
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
				PopupFactory.warning(this, op, ee.toString());
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
			PopupFactory.warning(this, op, "<HTML><PRE>" + cmp.getErrors() + "</PRE></HTML>");
			return;
		}
		dumpOnHalt = listing && cmp.wantsDump();
		Assembler ret = assemble(ezc, op, listing && cmp.listEasyCoder());
		if (ret != null) {
			if (listing && cmp.listSymbols()) {
				genMemoryMap(cmp.getSymTab(), ret.getSymTab());
			}
			PopupFactory.inform(this, op, String.format("Compile complete. %07o %07o %07o",
				currLow, currHi, sys.SR));
			if (cmp.hasData()) {
				P_CardReaderPunch cp = (P_CardReaderPunch)sys.pdc.getPeriph(PeriphDecode.P_PP);
				cp.addInput(new CardInputStream(cmp.getData(), sys.pdc.cvt),
						src.getName(), lc - cmp.lineCount());
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
		Assembler ret = null;
		try {
			if (listing) {
				String l = src.getAbsolutePath();
				if (l.endsWith(".ezc")) {
					l = l.substring(0, l.length() - 4);
				}
				l += ".lst";
				try {
					lst = new FileOutputStream(new File(l));
				} catch (Exception ee) {
					PopupFactory.warning(this, op, ee.toString());
					return false;
				}
				getPrinter().setOutput(lst);
			}
			ret = assemble(src, op, listing);
		} finally {
			getPrinter().setOutput(null);
			if (lst != null) {
				try { lst.close(); } catch (Exception ee) {}
			}
		}
		if (ret != null) {
			PopupFactory.inform(this, op, String.format("Assembly complete. %07o %07o %07o",
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
			PopupFactory.warning(this, op, "<HTML><PRE>" + asm.getErrors() + "</PRE></HTML>");
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
		int unit = 0;
		long vis = 0400000000000L;
		int rev = 0;
		if (tape || disk) {
			if (!dpi_lun.getText().isEmpty()) try {
				unit = Integer.valueOf(dpi_lun.getText());
			} catch (Exception ee) {
				PopupFactory.warning(this, op, "Unit number invalid");
				return null;
			}
			if (!dpi_rev.getText().isEmpty()) try {
				rev = Integer.valueOf(dpi_rev.getText());
			} catch (Exception ee) {
				PopupFactory.warning(this, op, "Revision number invalid");
				return null;
			}
			if (!dpi_vis.getText().isEmpty()) try {
				vis = UtilExecutable.visibility(dpi_vis.getText());
			} catch (Exception ee) {
				PopupFactory.warning(this, op, "Visibility invalid");
				return null;
			}
		}
		if (tape) {
			// Mag Tape BRT format...
			P_MagneticTape tp = (P_MagneticTape)sys.pdc.getPeriph(PeriphDecode.P_MT);
			// TODO: are these ever null?
			boolean copy = !tp.begin(unit); // always 'true' (!copy)?
			if (!tp.rewind()) {
				PopupFactory.warning(this, op, "No tape mounted");
				return null;
			}
			// TODO: need option to overwrite tape
			if (tp.empty()) {
				tp.appendRecord(SequentialRecordIO._1HDR, 0, -1);
				resCopy("bringup/bootmt.mti", tp);
			} else {
				// Find "1EOF " (or EOT)
				// TODO: fail-safe "1ERI "?
				// TODO: detect dup and delete/update? ++rev?
				posTo(copy, SequentialRecordIO._1EOF, tp);
			}
			// TODO: Allow cards vs. tape
			e = asm.passTwo(new PeriphLoader(tp, asm.charCvt(), vis, rev, 250),
					doList ? (CoreMemory)sys : null);
			tp.appendRecord(SequentialRecordIO._1EOF, 0, -1);
			tp.appendRecord(SequentialRecordIO._1ERI, 0, -1);
			tp.appendRecord(SequentialRecordIO._1ERI, 0, -1);
			tp.end();
		} else if (disk) {
			// MOD1 BRF format...
			byte[] file;
			if (go_go.isSelected()) {
				file = asm.charCvt().hwString("*DRS1GO", 10);
			} else {
				file = asm.charCvt().hwString("*DRS1RES", 10);
			}
			// boolean boot = dpi_bsp.isSelected();
			P_Disk dk = (P_Disk)sys.pdc.getPeriph(PeriphDecode.P_DK);
			DiskVolume vol = new DiskVolume(dk, unit, asm.charCvt());
			if (!vol.mount()) {
				PopupFactory.warning(this, op, "Failed to mount volume");
				return null;
			}
			DiskFile fi = vol.openFile(file, DiskFile.UPDATE);
			if (fi == null) {
				PopupFactory.warning(this, op, Errors.getError(vol.getError()));
				vol.unmount();
				return null;
			}
			e = asm.passTwo(new BRFLoader(fi, asm.charCvt(), vis, rev),
					doList ? (CoreMemory)sys : null);
			fi.close();
			vol.unmount();
		} else {
			// TODO: might try to execute from here...
			// Need hooks back to this class...
			e = asm.passTwo(new CoreLoader(sys, this), reloc, doList);
		}
		if (e < 0) {
			PopupFactory.warning(this, op, "<HTML><PRE>" + asm.getErrors() + "</PRE></HTML>");
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

	public void traceOut(String str) {
		if (trcCon) {
			getConsole().output(str);
		} else {
			getPrinter().output(str);
		}
	}

	public void listOut(String str) {
		getPrinter().output(str);
	}

	private void volInit() {
		String title = "Initialize Volume";
		int res = JOptionPane.showOptionDialog(this, vol_pn, title,
			JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE,
			null, null, null);
		if (res != JOptionPane.OK_OPTION) return;
		setActive(true);
		boolean ok = vol_pn.perform();
		setActive(false);
		if (!ok) {
			PopupFactory.warning(this, title, "Failed: " + vol_pn.getError());
			return;
		}
	}

	private void volMakeBoot() {
		String title = "Volume Bootstrap Generator";
		int res = JOptionPane.showOptionDialog(this, bsg_pn, title,
			JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE,
			null, null, null);
		if (res != JOptionPane.OK_OPTION) return;
		setActive(true);
		boolean ok = bsg_pn.perform();
		setActive(false);
		if (!ok) {
			PopupFactory.warning(this, title, "Failed: " + bsg_pn.getError());
			return;
		}
	}

	private void volExecutable() {
		String title = "Executable Function";
		int res = JOptionPane.showOptionDialog(this, xbl_pn, title,
			JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE,
			null, null, null);
		if (res != JOptionPane.OK_OPTION) return;
		setActive(true);
		boolean ok = xbl_pn.perform();
		setActive(false);
		if (!ok) {
			PopupFactory.warning(this, title, "Failed: " + xbl_pn.getError());
			return;
		}
	}

	private void volMap() {
		String title = "Map Volume";
		int res = JOptionPane.showOptionDialog(this, map_pn, title,
			JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE,
			null, null, null);
		if (res != JOptionPane.OK_OPTION) return;
		setActive(true);
		boolean ok = map_pn.perform();
		setActive(false);
		if (!ok) {
			PopupFactory.warning(this, title, "Failed: " + map_pn.getError());
			return;
		}
	}

	private void fileAlloc() {
		String title = "Allocate File";
		int res = JOptionPane.showOptionDialog(this, all_pn, title,
			JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE,
			null, null, null);
		if (res != JOptionPane.OK_OPTION) return;
		setActive(true);
		boolean ok = all_pn.perform();
		setActive(false);
		if (!ok) {
			PopupFactory.warning(this, title, "Failed: " + all_pn.getError());
			return;
		}
	}

	private void fileDealloc() {
		String title = "Deallocate File";
		int res = JOptionPane.showOptionDialog(this, rel_pn, title,
			JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE,
			null, null, null);
		if (res != JOptionPane.OK_OPTION) return;
		setActive(true);
		boolean ok = rel_pn.perform();
		setActive(false);
		if (!ok) {
			PopupFactory.warning(this, title, "Failed: " + rel_pn.getError());
			return;
		}
	}

	private int dumpDialog(String name) {
		int res = JOptionPane.showOptionDialog(this, dump_pn, name,
			JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE,
			null, dump_btns, dump_btns[OPTION_YES]);
		if (res == OPTION_CANCEL) return 0;
		if (res == OPTION_YES) {
			int radix = dump_rx.isSelected() ? 8 : 10;
			try {
				if (dump_lo.getText().length() > 0) {
					dumpLow = Integer.valueOf(dump_lo.getText(), radix);
				} else {
					dumpLow = currLow;
				}
				if (dump_hi.getText().length() > 0) {
					dumpHi = Integer.valueOf(dump_hi.getText(), radix);
				} else {
					dumpHi = currHi;
				}
				if (dumpLow >= sys.size() || dumpHi >= sys.size()) {
					PopupFactory.warning(this, "Dump",
						"Dump out of range");
					return 0;
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
		} else if (mi.getMnemonic() == KeyEvent.VK_B) {
			loadObjFile();
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
		} else if (mi.getMnemonic() == KeyEvent.VK_V) {
			volInit();
		} else if (mi.getMnemonic() == KeyEvent.VK_S) {
			volMap();
		} else if (mi.getMnemonic() == KeyEvent.VK_0) {
			fileAlloc();
		} else if (mi.getMnemonic() == KeyEvent.VK_1) {
			fileDealloc();
		} else if (mi.getMnemonic() == KeyEvent.VK_2) {
			volMakeBoot();
		} else if (mi.getMnemonic() == KeyEvent.VK_3) {
			volExecutable();
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
		} else if (mi.getMnemonic() == KeyEvent.VK_Z) {
			sys.clearMem();
		} else if (mi.getMnemonic() == KeyEvent.VK_Y) {
			sys.randMem();
		} else if (mi.getMnemonic() == KeyEvent.VK_X) {
			throttled = !throttled;
			mi_thr.setText("Throttle " + (throttled ? "Off" : "On"));
			sys.throttle(throttled);
		} else if (mi.getMnemonic() == KeyEvent.VK_J) {
			trcCon = !trcCon;
			mi_trc.setText("Trace to " + (trcCon ? "LP" : "CON"));
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

	boolean inLampTest = false;
	private void lampTest(boolean test) {
		if (test == inLampTest) {
			return;
		}
		inLampTest = test;
		LightedButton.doLampTest(test);
		Color ind = (test ? Peripheral.indLit : Peripheral.indDark);
		eintr.setForeground(ind);
		iintr.setForeground(ind);
		pgm.setForeground(ind);
		prot.setForeground(ind);
		parity.setForeground(ind);
		voltage.setForeground(ind);
		fan.setForeground(ind);
		cb.setForeground(ind);
		if (active != null) {
			active.setForeground(ind);
		}
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
