public class EIException extends RuntimeException {
	public byte type;
	public EIException(String msg, byte type) {
		super(msg);
		this.type = type;
	}
}
