public class I_PDT implements Instruction {
	// Format: PDT/A,C1,C2,C3,...
	// Format: PDT/A,C1,CE,C2,C3,...
	//
	// CE vs. C2 can be determined by bits 011000. For CE, those bits will be 01.
	// Otherwise, it is C2.
}
