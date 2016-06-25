import java.io.*;

public interface Peripheral {
	public void setOutput(OutputStream dev);
	public void setInput(InputStream dev);
	public OutputStream getOutput();
	public InputStream getInput();
	public void io(HW2000 sys);
	public void ctl(HW2000 sys);
	public void run(HW2000 sys);
	public boolean busy();
}
