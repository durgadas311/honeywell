// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Vector;

// set() -----> Selector(ProgStart) -----> is()
//                 |
//                 |
//        change() +----> SelectorContact
//                         [C(ProgStart)]
//                         [N(ProgStart)]
//                         [T(ProgStart)]

// An array of this class represents all available selectors,
// but each selector has an infinit number of contacts.
public class Selector extends ProgStart {
	Vector<SelectorContact> ctcs;

	public Selector() {
		super(false);
		ctcs = new Vector<SelectorContact>();
	}

	public SelectorContact addContact() {
		SelectorContact ctc = new SelectorContact(is());
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
