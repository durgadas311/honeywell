// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.awt.event.*;

public class CardHandlerEvent extends ActionEvent {
	boolean consumed = false;
	public CardHandlerEvent(Object source, int id, String command) {
		super(source, id, command);
	}
	public void consume() {
		consumed = true;
	}
	public boolean isConsumed() {
		return consumed;
	}
}
