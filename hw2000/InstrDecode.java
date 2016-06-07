public class InstrDecode {
	public static final int OP_HAS_A = 0x0001;
	public static final int OP_HAS_B = 0x0002;
	public static final int OP_DUP_A = 0x0004;
	public static final int OP_HAS_V = 0x0008;
	public static final int OP_HAS_C = 0x0010; // any number of extra operands
	public static final int OP_REQ_A = 0x0020;
	public static final int OP_REQ_B = 0x0040;
	public static final int OP_REQ_V = 0x0080;
	public static final int OP_PRIV = 0x4000;
	public static final int OP_INVAL = 0x8000;

	public static final byte OP_A = 036;	// Decimal Add
	public static final byte OP_S = 037;	// Decimal Subtract
	public static final byte OP_BA = 034;	// Binary Add
	public static final byte OP_BS = 035;	// Binary Subtract
	public static final byte OP_ZA = 016;	// Zero and Add
	public static final byte OP_ZS = 017;	// Zero and Subtract
	public static final byte OP_M = 026;	// Multiply
	public static final byte OP_D = 027;	// Divide

	public static final byte OP_EXT = 031;	// Extract (logical AND)
	public static final byte OP_HA = 030;	// Half-add (XOR)
	public static final byte OP_SST = 032;	// Substitute
	public static final byte OP_C = 033;	// Compare

	public static final byte OP_B = 065;	// Branch [on Condition Test]
	public static final byte OP_BCC = 054;	// Branch on Character Condition
	public static final byte OP_BCE = 055;	// Branch if Character Equal
	public static final byte OP_BBE = 056;	// Branch on Bit Equal

	public static final byte OP_SW = 022;	// Set Work mark
	public static final byte OP_SI = 020;	// Set Item mark
	public static final byte OP_CW = 023;	// Clear Work mark
	public static final byte OP_CI = 021;	// Clear Item mark
	public static final byte OP_H = 045;	// Halt
	public static final byte OP_NOP = 040;	// No Op
	public static final byte OP_MCW = 014;	// Move Chars to Word mark
	public static final byte OP_LCA = 015;	// Load Chars to A-field word mark
	public static final byte OP_SCR = 024;	// Store Control Registers
	public static final byte OP_LCR = 025;	// Load Control Registers
	public static final byte OP_CAM = 042;	// Change Address Mode
	public static final byte OP_CSM = 043;	// Change Sequencing Mode
	public static final byte OP_EXM = 010;	// EXtended Move
	public static final byte OP_MAT = 060;	// Move And Translate
	public static final byte OP_MIT = 062;	// Move Item and Translate
	public static final byte OP_LIB = 077;	// Load Index/Barricade register
	public static final byte OP_SIB = 076;	// Store Index/Barricade register
	public static final byte OP_TLU = 057;	// Table LookUp
	public static final byte OP_MOS = 013;	// Move Or Scan

	public static final byte OP_SVI = 046;	// Store Variant and Indicators
	public static final byte OP_RVI = 067;	// Restore Variant and Indicators
	public static final byte OP_MC = 044;	// Monitor Call
	public static final byte OP_RNM = 041;	// Resume Normal Mode

	public static final byte OP_MCE = 074;	// Move Characters and Edit

	public static final byte OP_PDT = 066;	// Peripheral Data Transfer
	public static final byte OP_PCB = 066;	// Peripheral Control and Branch

	public static final byte OP_IIC = 000;	// Internal Interrupt Call... TBD

	// For FP instructions, variant specifies operation.
	public static final byte OP_FMA = 007;	// FP Memory Acc
	public static final byte OP_FAA = 006;	// FP Acc Acc

	private int[] i_flags;
	private Instruction[] i_exec;

	public InstrDecode() {
		i_flags = new int[64];
		i_exec = new Instruction[64];
		Arrays.fill(i_flags, OP_INVAL);
		i_flags[OP_A] = OP_HAS_A | OP_HAS_B | OP_DUP_A;
		i_flags[OP_S] = OP_HAS_A | OP_HAS_B | OP_DUP_A;
		i_flags[OP_BA] = OP_HAS_A | OP_HAS_B | OP_DUP_A;
		i_flags[OP_BS] = OP_HAS_A | OP_HAS_B | OP_DUP_A;
		i_flags[OP_ZA] = OP_HAS_A | OP_HAS_B | OP_DUP_A;
		i_flags[OP_ZS] = OP_HAS_A | OP_HAS_B | OP_DUP_A;
		i_flags[OP_M] = OP_HAS_A | OP_HAS_B;
		i_flags[OP_D] = OP_HAS_A | OP_HAS_B;

		i_flags[OP_EXT] = OP_HAS_A | OP_HAS_B;
		i_flags[OP_HA] = OP_HAS_A | OP_HAS_B;
		i_flags[OP_SST] = OP_HAS_A | OP_HAS_B | OP_HAS_V;
		i_flags[OP_C] = OP_HAS_A | OP_HAS_B;

		i_flags[OP_B] = OP_HAS_A | OP_HAS_V; // B and BCT
		i_flags[OP_BCC] = OP_HAS_A | OP_HAS_B | OP_HAS_V;
		i_flags[OP_BCE] = OP_HAS_A | OP_HAS_B | OP_HAS_V;
		i_flags[OP_BBE] = OP_HAS_A | OP_HAS_B | OP_HAS_V;

		i_flags[OP_SW] = OP_HAS_A | OP_HAS_B | OP_DUP_A;
		i_flags[OP_SI] = OP_HAS_A | OP_HAS_B | OP_DUP_A;
		i_flags[OP_CW] = OP_HAS_A | OP_HAS_B | OP_DUP_A;
		i_flags[OP_CI] = OP_HAS_A | OP_HAS_B | OP_DUP_A;
		i_flags[OP_H] = OP_HAS_A | OP_HAS_B | OP_HAS_V | OP_PRIV;
		i_flags[OP_NOP] = 0;
		i_flags[OP_MCW] = OP_HAS_A | OP_HAS_B;
		i_flags[OP_LCA] = OP_HAS_A | OP_HAS_B;
		i_flags[OP_SCR] = OP_HAS_A | OP_HAS_V;
		i_flags[OP_LCR] = OP_HAS_A | OP_HAS_V | OP_PRIV;
		i_flags[OP_CAM] = OP_HAS_V;
		i_flags[OP_CSM] = OP_HAS_A | OP_HAS_B | OP_HAS_V;
		i_flags[OP_EXM] = OP_HAS_A | OP_HAS_B | OP_HAS_V;
		i_flags[OP_MAT] = OP_REQ_A | OP_REQ_B | OP_REQ_V;
		i_flags[OP_MIT] = OP_REQ_A | OP_REQ_B | OP_REQ_V;
		i_flags[OP_LIB] = OP_REQ_A | OP_HAS_B | OP_PRIV;
		i_flags[OP_SIB] = OP_REQ_A | OP_HAS_B;
		i_flags[OP_TLU] = OP_HAS_A | OP_HAS_B | OP_HAS_V;

		i_flags[OP_SVI] = OP_REQ_V | OP_PRIV;
		i_flags[OP_RVI] = OP_REQ_A | OP_REQ_V | OP_PRIV;
		i_flags[OP_MC] = 0;
		i_flags[OP_RNM] = OP_HAS_A | OP_HAS_B | OP_PRIV;

		i_flags[OP_MCE] = OP_HAS_A | OP_HAS_B;

		i_flags[OP_PDT] = OP_REQ_A | OP_REQ_V | OP_PRIV;
		i_flags[OP_PCB] = OP_REQ_A | OP_REQ_V | OP_PRIV;

		i_flags[OP_IIC] = OP_PRIV;
		// FPU
		i_flags[OP_FMA] = OP_HAS_A | OP_HAS_V;
		i_flags[OP_FAA] = OP_HAS_V;
	}

	public int getFlags(byte op) {
		return i_flags[op & 077];
	}

	public Instruction getExec(byte op) {
		return i_exec[op & 077];
	}
}
