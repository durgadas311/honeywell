import java.io.*;

public interface Peripheral {
	public void setOutput(OutputStream dev);
	public void setInput(InputStream dev);
	public void io(HW2000 sys);
	public void ctl(HW2000 sys);
}
