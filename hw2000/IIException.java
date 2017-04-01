// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class IIException extends RuntimeException {
	public byte type;
	public IIException(String msg, byte type) {
		super(msg);
		this.type = type;
	}
}
