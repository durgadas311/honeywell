// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Vector;

// An array of this class represents all available selectors,
// but each selector has an infinit number of contacts.
public class Selector extends ProgStart {
	Vector<SelectorContact> ctcs;

	public Selector() {
		super(false);
		ctcs = new Vector<SelectorContact>();
	}

	private SelectorContact addContact() {
		SelectorContact ctc = new SelectorContact(this);
		ctcs.add(ctc);
		return ctc;
	}

	@Override
	public void set(boolean b) {
		super.set(b);
		// TODO: only if 'true'?
		for (SelectorContact sc : ctcs) {
			sc.change(b);
		}
	}
}
