public class IIException extends RuntimeException {
	public byte type;
	public IIException(String msg, byte type) {
		super(msg);
		this.type = type;
	}
}
