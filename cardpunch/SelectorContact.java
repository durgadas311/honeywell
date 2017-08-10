// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

// Calling set() on C will result in set() on either N or T.
// Requesting is() on C will return is() on either N or T.
// Calling set() on N or T will conditionally call set() on C.
// Requesting is() on N or T will conditionally return is() on C.
public class SelectorContact {
	ProgStart selector;
	C comm;
	T tran;
	N norm;

	class C extends ProgStart {
		public C() {
			super(false);
		}
		@Override
		public void set(boolean b) {
			// TODO: remember state here, also?
			if (selector.is()) {
				tran._set(b);
			} else {
				norm._set(b);
			}
		}
		@Override
		public boolean is() {
			if (selector.is()) {
				return tran._is();
			} else {
				return norm._is();
			}
		}
		// These are needed to avoid looping
		public void _set(boolean b) { super.set(b); }
		public boolean _is() { return super.is(); }
	}

	class N extends ProgStart {
		public N() {
			super(false);
		}
		@Override
		public void set(boolean b) {
			// TODO: remember state here, also?
			if (!selector.is()) { comm._set(b); }
		}
		@Override
		public boolean is() {
			if (!selector.is()) { return comm._is(); }
			return false;
		}
		// These are needed to avoid looping
		public void _set(boolean b) { super.set(b); }
		public boolean _is() { return super.is(); }
	}

	class T extends ProgStart {
		public T() {
			super(false);
		}
		@Override
		public void set(boolean b) {
			// TODO: remember state here, also?
			if (selector.is()) { comm._set(b); }
		}
		@Override
		public boolean is() {
			if (selector.is()) { return comm._is(); }
			return false;
		}
		// These are needed to avoid looping
		public void _set(boolean b) { super.set(b); }
		public boolean _is() { return super.is(); }
	}

	public SelectorContact(ProgStart sel) {
		selector = sel;
		comm = new C();
		norm = new N();
		tran = new T();
	}

	public ProgStart C() { return comm; }
	public ProgStart N() { return norm; }
	public ProgStart T() { return tran; }

	public void change(boolean st) {
		// TODO: how to ripple change through...
	}
}
