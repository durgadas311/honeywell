// Copyright (c) 2016 Douglas Miller

import java.util.Arrays;
import java.io.*;
import java.awt.*;
import java.awt.print.*;
import javax.print.*;
import javax.print.attribute.*;
import javax.print.attribute.standard.*;

class PunchPS extends PunchCard implements Printable {

	int lastPage;
	int status;
	static FileInputStream _fis;
	static FileOutputStream _fos;
	byte[] _code;

	class DocPaperSize extends MediaSize implements DocAttribute {
		public DocPaperSize(float w, float h, int units) {
			super(w, h, units);
		}
	}

	class ReqPaperSize extends MediaSize implements PrintRequestAttribute {
		public ReqPaperSize(float w, float h, int units) {
			super(w, h, units);
		}
	}

	public static void main(String[] args) {
		if (args.length < 2) {
			System.err.println("Usage: PunchPS infile outfile");
			System.exit(1);
		}
		try {
			_fis = new FileInputStream(args[0]);
			_fos = new FileOutputStream(args[1]);
		} catch (Exception ee) {
			ee.printStackTrace();
			System.exit(1);
		}
		PunchPS pps = new PunchPS();
	}

	public PunchPS() {
		super(new CardPunchOptions());
		_code = new byte[2*80];
		_curr = _code;
		_cursor = 0;

		lastPage = -1;
		/* Use the pre-defined flavor for a Printable from an InputStream */
		DocFlavor flavor = DocFlavor.SERVICE_FORMATTED.PRINTABLE;
		StreamPrintServiceFactory[] factories;

		factories = new StreamPrintServiceFactory[1];
		factories[0] = new sun.print.PSxStreamPrinterFactory();

		try {
			/* Create a Stream printer for Postscript */
			StreamPrintService sps;
			sps = factories[0].getPrintService(_fos);

			DocAttributeSet dset = new HashDocAttributeSet();
			dset.add(OrientationRequested.LANDSCAPE);
			dset.add(new DocPaperSize(3.25f, 7.375f, Size2DSyntax.INCH));
			dset.add(new MediaPrintableArea(0.0f, 0.0f, 3.25f, 7.375f, Size2DSyntax.INCH));
			PrintRequestAttributeSet pset = new HashPrintRequestAttributeSet();
			pset.add(OrientationRequested.LANDSCAPE);
			pset.add(new ReqPaperSize(3.25f, 7.375f, Size2DSyntax.INCH));
			pset.add(new MediaPrintableArea(0.0f, 0.0f, 3.25f, 7.375f, Size2DSyntax.INCH));

			/* Create and call a Print Job */
			DocPrintJob pj = sps.createPrintJob();
			Doc doc = new SimpleDoc(this, flavor, dset);
			status = 0;
			pj.print(doc, pset);
		} catch (PrintException pe) { 
			pe.printStackTrace();
			//System.err.println(pe);
		}
	}

	public int print(Graphics g, PageFormat pf, int pageIndex) {
		if (status == 0) {
			++status;
		}
		if (lastPage != pageIndex) {
			lastPage = pageIndex;
			int b;
			Arrays.fill(_code, (byte)0);
			try {
				b = _fis.read(_code);
			} catch (Exception ee) {
				b = -1;
			}
			if (b > 0) {
				return Printable.PAGE_EXISTS;
			} else {
				return Printable.NO_SUCH_PAGE;
			}
		}
		Graphics2D g2d = (Graphics2D)g;
		double w0 = pf.getImageableWidth();
		w0 = w0 / _image.getIconWidth();
		g2d.scale(w0, w0);
		paint(g2d);
		return Printable.PAGE_EXISTS;
	}
}
