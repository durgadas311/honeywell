// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class CoreLoader implements Loader {
	private HW2000 sys;
	private FrontPanel fp;
	private String program;
	private String segment;
	private String revision;
	private int visibility;
	private LoaderMonitorC mon;

	public CoreLoader(CoreMemory sys, FrontPanel fp) {
		// TODO: fix this cast
		this.sys = (HW2000)sys;
		this.fp = fp;
	}

	public void begin(int adr, String prg, String seg, String rev, int vis) {
		program = prg;
		segment = seg;
		revision = rev;
		visibility = vis;
	}

	public void setCode(int adr, byte[] code) {
		for (int y = 0; y < code.length; ++y) {
			sys.rawWriteMem(adr + y, code[y]);
		}
	}

	public void clear(int start, int end, byte fill) {
		for (int y = start; y <= end; ++y) {
			sys.rawWriteMem(y, fill);
		}
	}

	public void range(int start, int end) {
		// Nothing for us. Assembler already tracks.
	}

	// TODO: run? or just pause?
	public void exec(int start) {
		mon = new LoaderMonitorC(sys, program, segment, revision, visibility);
		sys.addTrap(mon);
		sys.SR = start;
		fp.doRun();
		mon.waitReturn(0); // TODO: choose a timeout
		fp.doStop(); // 'mon' should have already done this
		sys.removeTrap(mon);
	}

	public void segment(String prg, String seg, String rev, int vis) {
		program = prg;
		segment = seg;
		revision = rev;
		visibility = vis;
	}

	public void end(int start) {
	}

	// A public service...
	public void listOut(String str) {
		fp.listOut(str);
	}
}
