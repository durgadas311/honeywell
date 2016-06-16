import java.util.Arrays;
import java.util.Map;
import java.util.HashMap;

public class InstrDecode {
	public static final byte OP_ILL = 070;	// An (otherwise) illegal op-code
	public static final byte OP_UNUSED_A = 001;
	public static final byte OP_UNUSED_B = 002;
	public static final byte OP_UNUSED_C = 003;
	public static final byte OP_UNUSED_D = 004;
	public static final byte OP_UNUSED_E = 005;
	public static final byte OP_UNUSED_F = 011;
	public static final byte OP_UNUSED_G = 012;
	public static final byte OP_UNUSED_H = 047;
	public static final byte OP_UNUSED_I = 050;
	public static final byte OP_UNUSED_J = 051;
	public static final byte OP_UNUSED_K = 052;
	public static final byte OP_UNUSED_L = 053;
	public static final byte OP_UNUSED_M = 061;
	public static final byte OP_UNUSED_N = 063;
	public static final byte OP_UNUSED_O = 071;	// used for Easycoder "B" vs "BCT"
	public static final byte OP_UNUSED_P = 072;	// used for Easycoder "B" vs "BCT"
	public static final byte OP_UNUSED_Q = 073;
	public static final byte OP_UNUSED_R = 075;

	public static final int OP_HAS_A = 0x0001;
	public static final int OP_HAS_B = 0x0002;
	public static final int OP_DUP_A = 0x0004;
	public static final int OP_HAS_V = 0x0008;
	public static final int OP_HAS_C = 0x0010; // any number of extra operands
	public static final int OP_REQ_A = 0x0020;
	public static final int OP_REQ_B = 0x0040;
	public static final int OP_REQ_V = 0x0080;
	public static final int OP_SPC = 0x2000;
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
	public static final byte OP_B_B = OP_UNUSED_O;		// Branch (Easycoder)
	public static final byte OP_B_BCT = OP_UNUSED_P;	// Branch on Condition Test (Easycoder)
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
	public static final byte OP_PCB = 064;	// Peripheral Control and Branch

	public static final byte OP_IIC = 000;	// Internal Interrupt Call... TBD

	// For FP instructions, variant specifies operation.
	public static final byte OP_FMA = 007;	// FP Memory Acc
	public static final byte OP_FAA = 006;	// FP Acc Acc

	private int[] i_flags;
	private Instruction[] i_exec;
	private Map<String,Byte> i_asm;

	public InstrDecode(boolean asm) {
		i_flags = new int[64];
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

		if (asm) {
			// Not true opcodes, but converted later. Need separate flags.
			i_flags[OP_B_B] = OP_HAS_A | OP_REQ_A | OP_SPC;
			i_flags[OP_B_BCT] = OP_HAS_A | OP_HAS_V | OP_SPC;
		} else {
			i_flags[OP_B] = OP_HAS_A | OP_HAS_V; // B, and BCT (conditionally priv)
		}
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
		i_flags[OP_LCR] = OP_HAS_A | OP_HAS_V; // conditionally privileged
		i_flags[OP_CAM] = OP_HAS_V;
		i_flags[OP_CSM] = OP_HAS_A | OP_HAS_B | OP_HAS_V;
		i_flags[OP_EXM] = OP_HAS_A | OP_HAS_B | OP_HAS_V;
		i_flags[OP_MAT] = OP_HAS_A | OP_HAS_B | OP_HAS_V | OP_REQ_A | OP_REQ_B | OP_REQ_V;
		i_flags[OP_MIT] = OP_HAS_A | OP_HAS_B | OP_HAS_V | OP_REQ_A | OP_REQ_B | OP_REQ_V;
		i_flags[OP_LIB] = OP_HAS_A | OP_REQ_A | OP_HAS_B | OP_PRIV;
		i_flags[OP_SIB] = OP_HAS_A | OP_REQ_A | OP_HAS_B;
		i_flags[OP_TLU] = OP_HAS_A | OP_HAS_B | OP_HAS_V;

		i_flags[OP_SVI] = OP_HAS_V | OP_REQ_V | OP_PRIV;
		i_flags[OP_RVI] = OP_HAS_A | OP_HAS_V | OP_REQ_A | OP_REQ_V | OP_PRIV;
		i_flags[OP_MC] = 0;
		i_flags[OP_RNM] = OP_HAS_A | OP_HAS_B | OP_PRIV;

		i_flags[OP_MCE] = OP_HAS_A | OP_HAS_B;

		i_flags[OP_PDT] = OP_HAS_A | OP_HAS_V | OP_REQ_A | OP_REQ_V | OP_PRIV;
		i_flags[OP_PCB] = OP_HAS_A | OP_HAS_V | OP_REQ_A | OP_REQ_V | OP_PRIV;

		i_flags[OP_IIC] = OP_PRIV;
		// FPU
		i_flags[OP_FMA] = OP_HAS_A | OP_HAS_V;
		i_flags[OP_FAA] = OP_HAS_V;

		// ---------------------------------------------------
		if (!asm) {
			i_exec = new Instruction[64];
			i_exec[OP_A] = new I_A();
			i_exec[OP_S] = new I_S();
			i_exec[OP_BA] = new I_BA();
			i_exec[OP_BS] = new I_BS();
			i_exec[OP_ZA] = new I_ZA();
			i_exec[OP_ZS] = new I_ZS();
			i_exec[OP_M] = new I_M();
			i_exec[OP_D] = new I_D();

			i_exec[OP_EXT] = new I_EXT();
			i_exec[OP_HA] = new I_HA();
			i_exec[OP_SST] = new I_SST();
			i_exec[OP_C] = new I_C();

			i_exec[OP_B] = new I_B_BCT(); // B and BCT
			i_exec[OP_BCC] = new I_BCC();
			i_exec[OP_BCE] = new I_BCE();
			i_exec[OP_BBE] = new I_BBE();

			i_exec[OP_SW] = new I_SW();
			i_exec[OP_SI] = new I_SI();
			i_exec[OP_CW] = new I_CW();
			i_exec[OP_CI] = new I_CI();
			i_exec[OP_H] = new I_H();
			i_exec[OP_NOP] = new I_NOP();
			i_exec[OP_MCW] = new I_MCW();
			i_exec[OP_LCA] = new I_LCA();
			i_exec[OP_SCR] = new I_SCR();
			i_exec[OP_LCR] = new I_LCR();
			i_exec[OP_CAM] = new I_CAM();
			i_exec[OP_CSM] = new I_CSM();
			i_exec[OP_EXM] = new I_EXM();
			i_exec[OP_MAT] = new I_MAT();
			i_exec[OP_MIT] = new I_MIT();
			i_exec[OP_LIB] = new I_LIB();
			i_exec[OP_SIB] = new I_SIB();
			i_exec[OP_TLU] = new I_TLU();
			i_exec[OP_MOS] = new I_MOS();

			i_exec[OP_SVI] = new I_SVI();
			i_exec[OP_RVI] = new I_RVI();
			i_exec[OP_MC] = new I_MC();
			i_exec[OP_RNM] = new I_RNM();

			i_exec[OP_MCE] = new I_MCE();

			i_exec[OP_PDT] = new I_PDT();
			i_exec[OP_PCB] = new I_PCB();

			i_exec[OP_IIC] = new I_IIC();
			// FPU
			i_exec[OP_FMA] = new I_FMA();
			i_exec[OP_FAA] = new I_FAA();
		} else {
			i_asm = new HashMap<String,Byte>();
			i_asm.put("A", OP_A);
			i_asm.put("S", OP_S);
			i_asm.put("BA", OP_BA);
			i_asm.put("BS", OP_BS);
			i_asm.put("ZA", OP_ZA);
			i_asm.put("ZS", OP_ZS);
			i_asm.put("M", OP_M);
			i_asm.put("D", OP_D);

			i_asm.put("EXT", OP_EXT);
			i_asm.put("HA", OP_HA);
			i_asm.put("SST", OP_SST);
			i_asm.put("C", OP_C);

			i_asm.put("B", OP_B_B);
			i_asm.put("BCT", OP_B_BCT);
			i_asm.put("BCC", OP_BCC);
			i_asm.put("BCE", OP_BCE);
			i_asm.put("BBE", OP_BBE);

			i_asm.put("SW", OP_SW);
			i_asm.put("SI", OP_SI);
			i_asm.put("CW", OP_CW);
			i_asm.put("CI", OP_CI);
			i_asm.put("H", OP_H);
			i_asm.put("NOP", OP_NOP);
			i_asm.put("MCW", OP_MCW);
			i_asm.put("LCA", OP_LCA);
			i_asm.put("SCR", OP_SCR);
			i_asm.put("LCR", OP_LCR);
			i_asm.put("CAM", OP_CAM);
			i_asm.put("CSM", OP_CSM);
			i_asm.put("EXM", OP_EXM);
			i_asm.put("MAT", OP_MAT);
			i_asm.put("MIT", OP_MIT);
			i_asm.put("LIB", OP_LIB);
			i_asm.put("SIB", OP_SIB);
			i_asm.put("TLU", OP_TLU);
			i_asm.put("MOS", OP_MOS);

			i_asm.put("SVI", OP_SVI);
			i_asm.put("RVI", OP_RVI);
			i_asm.put("MC", OP_MC);
			i_asm.put("RNM", OP_RNM);

			i_asm.put("MCE", OP_MCE);

			i_asm.put("PDT", OP_PDT);
			i_asm.put("PCB", OP_PCB);

			i_asm.put("IIC", OP_IIC);

			i_asm.put("FMA", OP_FMA);
			i_asm.put("FAA", OP_FAA);
		}
	}

	public int getFlags(byte op) {
		return i_flags[op & 077];
	}

	public Instruction getExec(byte op) {
		return i_exec[op & 077];
	}

	public byte getOp(String nm) {
		Byte b = i_asm.get(nm);
		if (b == null) {
			return OP_ILL;
		}
		return b;
	}
}
