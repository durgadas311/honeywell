// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class EIException extends RuntimeException {
	public byte type;
	public EIException(String msg, byte type) {
		super(msg);
		this.type = type;
	}
}
