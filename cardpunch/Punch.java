// Copyright (c) 2016 Douglas Miller

import java.awt.*;
import javax.swing.*;
import java.awt.geom.AffineTransform;
import java.io.*;
import java.awt.event.*;
import java.util.Arrays;

import java.awt.print.*;
import javax.print.attribute.*;
import javax.print.attribute.standard.*;

class Punch
{
	FileOutputStream _outDeck = null;
	CharConverter _cvt;
	byte[] bb;
	int _cursor;
	byte[] _code;

	static public void main(String[] args) {
		if (args.length < 1) {
			System.err.println("Usage: Punch outfile [< infile]");
			System.exit(1);
		}
		Punch pun = new Punch(args[0]);
		pun.run();
	}

	public Punch(String out) {
		_cvt = new CharConverter();
		bb = new byte[1];
		_code = new byte[2*80];
		setupFile(new File(out));
		newCard();
	}

	public void run() {
		boolean trunc = false;
		int c = 0;
		while (true) {
			try {
				c = System.in.read();
			} catch (Exception ee) {
				break;
			}
			if (c < 0) {
				break;
			}
			c &= 0x7f;
			int p = 0;
			if (c == '\n') {
				finishCard(false);
				trunc = false;
				continue;
			}
			if (trunc) {
				continue;
			}
			c = Character.toUpperCase(c);
			p = _cvt.asciiToPun((int)c);
			if (p < 0) {
				continue;
			}
			punch(p, false);
			if (_cursor > 80) {
				trunc = true;
			}
		}
		if (_outDeck != null) {
			try {
				_outDeck.close();
			} catch (Exception ee) {}
			_outDeck = null;
		}
	}

	private void nextCol() {
		++_cursor;
	}

	private void newCard() {
		Arrays.fill(_code, (byte)0);
		_cursor = 1;
	}

	private void finishCard(boolean auto) {
		if (_outDeck != null) {
			try {
				_outDeck.write(_code);
			} catch (Exception ee) {
				ee.printStackTrace();
			}
		}
		newCard();
	}

	private void punch(int p, boolean multi) {
		if (_cursor > 80) {
			return;
		}
		if (p != 0) {
			// this corrupts 'p'...
			p |= 0x1000;
			int cx = (_cursor - 1) * 2;
			_code[cx] |= (byte)(p & 0x0ff);
			_code[cx + 1] |= (byte)((p >> 8) & 0x0ff);
		}
		nextCol();
	}

	private void setupFile(File file) {
		if (_outDeck != null) {
			try {
				_outDeck.close();
			} catch (Exception ee) {}
			_outDeck = null;
		}
		try {
			_outDeck = new FileOutputStream(file);
		} catch (Exception ee) {
			ee.printStackTrace();
		}
	}

}
