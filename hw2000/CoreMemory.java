public interface CoreMemory {
	public byte readMem(int adr);
	public byte readChar(int adr);
	public byte rawReadMem(int adr);
	public void rawWriteMem(int adr, byte val);
	public void writeMem(int adr, byte val);
	public void writeChar(int adr, byte val);
	public void setWord(int adr);
	public void setItem(int adr);
	public void clrWord(int adr);
	public void clrItem(int adr);
	public void listOut(String str);
}
