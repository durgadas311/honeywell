// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class EasyCoder extends JFrame implements ActionListener {
	static EasyCoder thus;

	static final Color done = new Color(200,255,200);
	File dir;
	JButton pick;
	JButton asmb;
	JCheckBox listing;
	JCheckBox swi;
	JCheckBox self;
	JCheckBox ovrw;
	ButtonGroup outBg;
	JRadioButton brt;
	JRadioButton brtCard;
	JRadioButton boot;
	JRadioButton raw;
	JTextField inFile;
	JTextArea min;
	JTextArea max;
	JTextArea start;

	// TODO: These are supposed to be 80-char (both Tape and Card)
	private static final byte[] _1HDR = new byte[]{ 001, 030, 024, 051, 015}; // 1HDR_
	private static final byte[] _1EOF = new byte[]{ 001, 025, 046, 026, 015}; // 1EOF_
	private static final byte[] _1ERI = new byte[]{ 001, 025, 051, 031, 015}; // 1ERI_

	public EasyCoder(String[] args) {
		super("EasyCoder Assembler");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));

		dir = new File(System.getProperty("user.dir"));

		pick = new JButton("Pick");
		pick.addActionListener(this);
		listing = new JCheckBox("Listing");
		listing.addActionListener(this);
		self = new JCheckBox("Self Loading");
		self.addActionListener(this);
		ovrw = new JCheckBox("Overwrite");
		ovrw.addActionListener(this);
		swi = new JCheckBox("SW/SI");
		swi.addActionListener(this);
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
		min = new JTextArea();
		min.setPreferredSize(new Dimension(50, 20));
		min.setEditable(false);
		max = new JTextArea();
		max.setPreferredSize(new Dimension(50, 20));
		max.setEditable(false);
		start = new JTextArea();
		start.setPreferredSize(new Dimension(50, 20));
		start.setEditable(false);

		brt.setSelected(true);
		swi.setEnabled(false);
		ovrw.setEnabled(false);

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
		pn = new JPanel();
		pn.setLayout(new BoxLayout(pn, BoxLayout.Y_AXIS));
		pn.add(self);
		pn.add(ovrw);
		pn.add(swi);
		pn2.add(pn);
		add(pn2);
		pn2 = new JPanel();
		pn2.setLayout(new BoxLayout(pn2, BoxLayout.X_AXIS));
		pn = new JPanel();
		pn.setLayout(new BoxLayout(pn, BoxLayout.X_AXIS));
		pn.add(new JLabel(" Min:"));
		pn.add(min);
		pn2.add(pn);
		pn = new JPanel();
		pn.setLayout(new BoxLayout(pn, BoxLayout.X_AXIS));
		pn.add(new JLabel(" Max:"));
		pn.add(max);
		pn2.add(pn);
		pn = new JPanel();
		pn.setLayout(new BoxLayout(pn, BoxLayout.X_AXIS));
		pn.add(new JLabel(" Start:"));
		pn.add(start);
		pn2.add(pn);
		add(pn2);
		pn = new JPanel();
		pn.add(asmb);
		add(pn);

		pack();
		setVisible(true);
	}

	private void resCopy(String res, RandomAccessFile p) throws Exception {
		InputStream r = getClass().getResourceAsStream(res);
		if (r == null) {
			// TODO: throw error
			System.err.format("No resource: %s\n", res);
			return;
		}
		int n = r.available();
		byte[] buf = new byte[n];
		r.read(buf);
		p.write(buf);
	}

	// Assumes at first char of record...
	private int skipRec(RandomAccessFile brt) throws Exception {
		int c1 = brt.read();
		if ((c1 & 0300) == 0300) {
			return -1;
		}
		while (true) {
			int b = brt.read();
			if (b < 0) {
				break; // or fail?
			}
			if ((b & 0300) == 0300) {
				break;
			}
		}
		return c1;
	}

	private void setupSelfLdr(RandomAccessFile brt, boolean tape) throws Exception {
		if (brt.length() == 0) {
			brt.write(_1HDR);
			brt.write(0300);
			resCopy("bootmt.mti", brt);
		} else {
			brt.seek(0);
			byte[] hdr = new byte[_1HDR.length];
			int n = brt.read(hdr);
			int e = brt.read();
			if (n != hdr.length || !hdr.equals(_1HDR) ||
					(e & 0300) != 0300) {
				// throw error...
				brt.close();
				return;
			}
			// Skip Bootstrap record
			if (skipRec(brt) != 022) {
				// throw error...
				brt.close();
				return;
			}
			int b;
			// Skip Loader records
			while ((b = skipRec(brt)) == 042) { }
			// Now skip 050/054/041/044 records...
			// i.e. skip to "1EOF " record...
			while (b == 050 || b == 054 || b == 041 || b == 044) {
				b = skipRec(brt);
			}
			if (b < 0) {
				// throw error?
				brt.close();
				return;
			}
			if (b == 001) {
				// possibly EOF/ERI...
				brt.seek(brt.getFilePointer() - 1);
			}
			// Should be at "1EOF " record...
			n = brt.read(hdr);
			e = brt.read();
			if (n != hdr.length || !hdr.equals(_1EOF) ||
					(e & 0300) != 0300) {
				// throw error...
				brt.close();
				return;
			}
			// Backup to overwrite "1EOF "
			brt.seek(brt.getFilePointer() - hdr.length - 1);
		}
	}

	private void finishSelfLdr(RandomAccessFile brt, boolean tape) throws Exception {
		brt.write(_1EOF);
		brt.write(0300);
		brt.write(_1ERI);
		brt.write(0300);
		brt.write(_1ERI);
		brt.write(0300);
		brt.write(0300);
	}

	private void assemble() {
		boolean errs = false;
		boolean cards = brtCard.isSelected();
		boolean bin = raw.isSelected();
		boolean slf = self.isSelected();
		boolean ovr = ovrw.isSelected();
		boolean bs = boot.isSelected();
		boolean list = listing.isSelected();
		boolean rawSW = swi.isSelected();
		File in = new File(dir, inFile.getText());
		if (!in.exists()) {
			System.err.format("No file: %s\n", inFile.getText());
			return;
		}
		String s = inFile.getText().replaceFirst("\\.ezc$", "");
		File lst = new File(s + ".lst");

		Assembler asm = new Assembler(in);
		int e = asm.passOne();
		if (e < 0) {
			setError(asm);
			return;
		}
		Loader ldr;
		Closeable fo = null;
		FileOutputStream lo = null;
		RandomAccessFile brt = null;
		try {
			if (slf) {
				File out = new File(s + ".mti");
				brt = new RandomAccessFile(out, "rw");
				fo = brt;
				if (ovr) {
					brt.setLength(0);
				}
				setupSelfLdr(brt, !cards);
				if (cards) {
					ldr = new CardLoader(brt, asm.charCvt());
				} else {
					ldr = new TapeLoader(brt, asm.charCvt());
				}
			} else {
				File out = new File(s + ".out");
				FileOutputStream f = new FileOutputStream(out);
				fo = f;
				if (cards) {
					ldr = new CardLoader(f, asm.charCvt());
				} else if (bs || bin) {
					PrintStream swi =null;
					if (rawSW) {
						swi = new PrintStream(s + ".swi");
					}
					ldr = new RawLoader(f, swi,
						rawSW ? asm : null, bin ? 250 : -1);
				} else {
					ldr = new TapeLoader(f, asm.charCvt());
				}
			}
			if (list) {
				lo = new FileOutputStream(lst);
			}
		} catch (Exception ee) {
			ee.printStackTrace();
			return;
		}
		e = asm.passTwo(ldr, lo);
		errs = (e < 0);
		try {
			if (slf) {
				finishSelfLdr(brt, !cards);
			}
			fo.close();
		} catch (Exception ee) {}
		if (lo != null) {
			try { lo.close(); } catch (Exception ee) {}
		}
		if (errs) {
			setError(asm);
		} else {
			setSuccess(asm);
		}
	}

	private void setError(Assembler asm) {
		inFile.setBackground(Color.pink);
		warning(inFile.getText(),
			"<HTML><PRE>" + asm.getErrors() + "</PRE></HTML>");
	}

	private void setSuccess(Assembler asm) {
		inFile.setBackground(done);
		min.setText(String.format("%07o", asm.getMin()));
		max.setText(String.format("%07o", asm.getMax()));
		start.setText(String.format("%07o", asm.getStart()));
	}

	private void pickFile() {
		SuffFileChooser ch = new SuffFileChooser("EasyCoder Source",
				new String[]{"ezc"}, new String[]{"EasyCoder"},
				dir, null);
		int rv = ch.showDialog(this);
		if (rv == JFileChooser.APPROVE_OPTION) {
			File f = ch.getSelectedFile();
			inFile.setText(f.getName());
			dir = f.getParentFile();
		}
	}

	public void actionPerformed(ActionEvent e) {
		inFile.setBackground(Color.white);
		min.setText("");
		max.setText("");
		start.setText("");
		if (e.getSource() instanceof JCheckBox) {
			JCheckBox btn = (JCheckBox)e.getSource();
			if (btn == self) {
				if (self.isSelected()) {
					ovrw.setEnabled(true);
				} else {
					ovrw.setSelected(false);
					ovrw.setEnabled(false);
				}
			}
			return;
		}
		if (e.getSource() instanceof JButton) {
			JButton btn = (JButton)e.getSource();
			if (btn == asmb) {
				assemble();
			} else if (btn == pick) {
				pickFile();
			}
			return;
		}
		if (e.getSource() instanceof JRadioButton) {
			JRadioButton btn = (JRadioButton)e.getSource();
			if (btn == boot || btn == raw) {
				self.setSelected(false);
				ovrw.setSelected(false);
				self.setEnabled(false);
				ovrw.setEnabled(false);
				swi.setEnabled(true);
			} else {
				swi.setSelected(false);
				swi.setEnabled(false);
				self.setEnabled(true);
			}
			return;
		}
	}

	private void warning(String op, String err) {
		JOptionPane.showMessageDialog(this,
			new JLabel(err),
			op, JOptionPane.WARNING_MESSAGE);
	}

	public static void main(String[] args) {
		thus = new EasyCoder(args);
	}
}
