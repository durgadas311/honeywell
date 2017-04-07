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
	JCheckBox self;
	JCheckBox ovrw;
	ButtonGroup outBg;
	JRadioButton brt;
	JRadioButton brtCard;
	JRadioButton boot;
	JRadioButton raw;
	JTextField inFile;

	private static final byte[] _1HDR = new byte[]{ 001, 030, 024, 051, 015}; // 1HDR_
	private static final byte[] _1EOF = new byte[]{ 001, 025, 046, 026, 015}; // 1EOF_
	private static final byte[] _1ERI = new byte[]{ 001, 025, 051, 031, 015}; // 1ERI_

	public EasyCoder(String[] args) {
		super("EasyCoder Assembler");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));

		pick = new JButton("Pick");
		listing = new JCheckBox("Listing");
		self = new JCheckBox("Self Loading");
		self.addActionListener(this);
		ovrw = new JCheckBox("Overwrite");
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
		pn = new JPanel();
		pn.add(asmb);
		add(pn);

		pack();
		setVisible(true);
	}

	private void resCopy(String res, RandomAccessFile p) throws Exception {
		InputStream r = getClass().getResourceAsStream(res);
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

	private void setupSelfLdr(RandomAccessFile brt, boolean tape) {
try {
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
} catch (Exception ee) {
	// TODO: handle
}
	}

	private void assemble() {
		boolean cards = brtCard.isSelected();
		boolean bin = raw.isSelected();
		boolean slf = self.isSelected();
		boolean ovr = ovrw.isSelected();
		boolean bs = boot.isSelected();
		boolean list = listing.isSelected();
		boolean rawSW = swi.isSelected();
		File in = new File(inFile.getText());
		if (!in.exists()) {
			System.err.format("No file: %s\n", inFile.getText());
			return;
		}
		String s = inFile.getText().replaceFirst("\\.ezc$", "");
		File lst = new File(s + ".lst");

		Assembler asm = new Assembler(in);
		int e = asm.passOne();
		if (e < 0) {
			// TODO: pop-up
			System.err.println(asm.getErrors());
			return;
		}
		Loader ldr;
		Closeable fo = null;
		FileOutputStream lo = null;
		try {
			if (slf) {
				File out = new File(s + ".mti");
				RandomAccessFile f = new RandomAccessFile(out, "rw");
				fo = f;
				if (ovr) {
					f.setLength(0);
				}
				setupSelfLdr(f, !cards);
				if (cards) {
					ldr = new CardLoader(f, asm.charCvt());
				} else {
					ldr = new TapeLoader(f, asm.charCvt());
				}
			} else {
				File out = new File(s + ".out");
				FileOutputStream f = new FileOutputStream(out);
				fo = f;
				if (cards) {
					ldr = new CardLoader(f, asm.charCvt());
				} else if (bs || bin) {
					ldr = new RawLoader(f, rawSW ? asm : null, bin ? 250 : -1);
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
		if (e < 0) {
			// TODO: pop-up
			System.err.println(asm.getErrors());
		}
		try { fo.close(); } catch (Exception ee) {}
		if (lo != null) {
			try { lo.close(); } catch (Exception ee) {}
		}
	}

	public void actionPerformed(ActionEvent e) {
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

	public static void main(String[] args) {
		thus = new EasyCoder(args);
	}
}
