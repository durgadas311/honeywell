import java.io.*;

public interface Peripheral {
	public void setOutput(OutputStream dev);
	public void setInput(InputStream dev);
	public OutputStream getOutput();
	public InputStream getInput();
	public void io(HW2000 sys);
	public void ctl(HW2000 sys);
	public void run(HW2000 sys);
	public void setInterrupt(HW2000 sys);
	public boolean busy(byte c2);
	public void output(String s);
	public void reset();
	public void visible(boolean on);
}
