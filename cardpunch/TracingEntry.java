// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class TracingEntry extends ProgStart {
	String name;
	public TracingEntry(String name, boolean f) {
		super(f);
		this.name = name;
	}
	@Override
	public void set(boolean b) {
		System.err.format("%s: set %s -> %s\n", name, bool, b);
		super.set(b);
	}
	@Override
	public boolean is() {
		System.err.format("%s: is %s\n", name, bool);
		return super.is();
	}
	@Override
	public void reset() {
		System.err.format("%s: reset\n", name);
		super.reset();
	}
	@Override
	public void putCol(int p, char c) {
		System.err.format("%s: putCol %03x '%c'\n", name, p, c);
		super.putCol(p, c);
	}
	@Override
	public void addWatcher(ProgStart ps) {
		System.err.format("%s: add watcher %s\n", name, ps.getClass().getName());
		super.addWatcher(ps);
	}
	@Override
	void trigger(boolean b) {
		for (ProgStart ps : watchers) {
			System.err.format("%s: trigger %s (%s)\n", name, b,
				ps.getClass().getName());
			ps.set(b);
		}
	}
	@Override
	void trigger(int p, char c) {
		for (ProgStart ps : watchers) {
			System.err.format("%s: trigger %03x '%c' (%s)\n", name,
				p, c, ps.getClass().getName());
			ps.putCol(p, c);
		}
	}
}
