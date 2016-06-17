public interface CoreMemory {
	public byte readMem(int adr);
	public byte readChar(int adr);
	public void writeMem(int adr, byte val);
	public void writeChar(int adr, byte val);
	public void setWord(int adr);
	public void setItem(int adr);
}
