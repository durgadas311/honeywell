// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

// TODO! Contacts can carry ProgStart or ProgEntry!

// Calling set() on C will result in set() on either N or T.
// Requesting is() on C will return is() on either N or T.
// Calling set() on N or T will conditionally call set() on C.
// Requesting is() on N or T will conditionally return is() on C.
public class SelectorContact {
	boolean selector;	// current state of the pickup
	C comm;	// "common" contact terminal (C)
	T tran;	// "transferred" contact terminal (NO)
	N norm;	// "normal" contact terminal (NC)

	// "set()" should only be called on one end, either
	// "C" or "N"/"T". The state is *only* stored on the
	// end where "set()" is called. It is assumed that
	// any call to "is()" is looking for the state on
	// the "remote" (opposite) end.
	// This allows the contact to be bi-direction, but
	// there are effectively two paths through the contact
	// and they have separate states. The contact should
	// only be used in one direction, however choice of
	// that direction is entirely up to the user.
	//
	class C extends ProgStart {
		public C() {
			super(false);
		}
		@Override
		public void set(boolean b) {
			super.set(b);
			if (selector) {
				tran.trigger(b);
			} else {
				norm.trigger(b);
			}
		}
		@Override
		public boolean is() {
			if (selector) {
				return tran.get();
			} else {
				return norm.get();
			}
		}
		// selector is about to change state...
		public void pre() {
			if (!get()) return;
			if (selector) {
				tran.trigger(false);
			} else {
				norm.trigger(false);
			}
		}
		// selector just changed state...
		public void post() {
			if (!get()) return;
			if (selector) {
				tran.trigger(get());
			} else {
				norm.trigger(get());
			}
		}
	}

	class N extends ProgStart {
		public N() {
			super(false);
		}
		@Override
		public void set(boolean b) {
			super.set(b);
			if (!selector) {
				comm.trigger(b);
			}
		}
		@Override
		public boolean is() {
			if (!selector) { return comm.get(); }
			return false;
		}
		// selector is about to change state...
		public void pre() {
			if (!get()) return;
			if (!selector) {
				comm.trigger(false);
			}
		}
		// selector just changed state...
		public void post() {
			if (!get()) return;
			if (!selector) {
				comm.trigger(get());
			}
		}
	}

	class T extends ProgStart {
		public T() {
			super(false);
		}
		@Override
		public void set(boolean b) {
			super.set(b);
			if (selector) {
				comm.trigger(b);
			}
		}
		@Override
		public boolean is() {
			if (selector) { return comm.get(); }
			return false;
		}
		// selector is about to change state...
		public void pre() {
			if (!get()) return;
			if (selector) {
				comm.trigger(false);
			}
		}
		// selector just changed state...
		public void post() {
			if (!get()) return;
			if (selector) {
				comm.trigger(get());
			}
		}
	}

	public SelectorContact(boolean st) {
		selector = st;
		comm = new C();
		norm = new N();
		tran = new T();
	}

	public ProgStart C() { return comm; }
	public ProgStart N() { return norm; }
	public ProgStart T() { return tran; }


	public void change(boolean st) {
		comm.pre();
		norm.pre();
		tran.pre();
		selector = st;
		tran.post();
		norm.post();
		comm.post();
	}
}
