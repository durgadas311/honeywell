#include "c1.h"
#define cr102 &optab[0]

static char L1[]=	"b\tA\n";
static char L2[]=	"GBb\t#(I)\n";
#define cr100 &optab[3]

static char L3[]=	"bl\tA\n";
static char L4[]=	"GBbl\t#(I)\n";
static char L5[]=	"GAbl\t(I)\n";
#define cr106 &optab[7]

static char L6[]=	"bs\tI\n";
static char L7[]=	"clrf\tI\n";
static char L8[]=	"li\tI,A\n";
static char L9[]=	"movC\tA,I\n"
			">1\tI,8\n";
static char L10[]=	"movof\tA,I\n";
static char L11[]=	"GBmovC\t#(I),I\n"
			">1\tI,8\n";
static char L12[]=	"GBmovof\t#(I),I\n";
static char L13[]=	"li\tI+,A+\n"
			"li\tI,A\n";
static char L14[]=	"mov\tA+,I+\n"
			"mov\tA,I\n";
static char L15[]=	"GBmov\t#+2(I),I+\n"
			"mov\t#(I),I\n";
static char L16[]=	"GA";
#define cr32 &optab[28]

static char L17[]=	"mov\tA',I\n"
			"M'\tA''\n";
static char L18[]=	"mov\tA',I\n"
			"M't\tA''\n";
static char L19[]=	"mov\tA',I\n"
			"li\tr0,B\n"
			"M\tr0,A''\n";
static char L20[]=	"movb\tA',I\n"	// mov byte from arg to reg
			">1\tI,8\n"	// move hi byte to lo
			"li\tr0,256*B\n"
			"Mb\tr0,A''\n";
static char L21[]=	"GJmov\t#(J),I\n"
			"M'\t#(J)\n";
static char L22[]=	"GBQmov\t#(I),(sp)\n"
			"M'\t#(I)\n"
			"mov\t(sp)+,I\n";
static char L23[]=	"GJmov\t#(J),I\n"
			"li\tr0,B\n"
			"M\tr0,#(J)\n";
static char L24[]=	"GJmovb\t#(J),I\n"
			">1\tI,8\n"
			"li\tr0,256*B\n"
			"Mb\tr0,#(J)\n";
static char L25[]=	"GBQmov\t#(I),(sp)\n"
			"M\tB,#(I)\n"
			"mov\t(sp)+,I\n";
static char L26[]=	"GBQmovb\t(I),(sp)\n"
			"li\tr0,256*B\n"
			"Mb\tr0,(I)\n"
			"movb\t(sp)+,I\n"
			">1\tI,8\n";
static char L27[]=	"GAM'\tA+\n"
			"V\tA\n";
static char L28[]=	"GJmov\t#+2(J),I+\n"
			"mov\t#(J),I\n"
			"M'\t#+2(J)\n"
			"V\t#(J)\n";
static char L29[]=	"GBQmov\t#+2(I),(sp)\n"
			"Qmov\t#(I),(sp)\n"
			"M'\t#+2(I)\n"
			"V\t#(I)\n"
			"mov\t(sp)+,I\n"
			"mov\t(sp)+,I+\n";
#define cr37 &optab[48]

static char L30[]=	"GAMP\tI\n";
static char L31[]=	"GAM\tI\n"
			"M\tI+\n"
			"V\tI\n";
#define cr80 &optab[53]

static char L32[]=	"KA<1\tI,8\n"
			"movC\tI,A\n"
			">1\tI,8\n";
static char L33[]=	"KAmovfo\tI,A\n";
static char L34[]=	"GBKAmovf\tI,#(I)\n";
static char L35[]=	"GBli\tr0,B*256\n"
			"movb\tr0,#(I)\n"
			">1\tr0,8\n"
			"mov\tr0,I\n";
static char L36[]=	"GBli\tr0,B\n"
			"mov\tr0,#(I)\n"
			"mov\tr0,I\n";
static char L37[]=	"GBmovb\tB,#(I)\n";
static char L38[]=	"GBmov\tB,r0\n"
			"<1\tr0,8\n"
			"MC\tr0,#(I)\n";
static char L39[]=	"GBmovC\tB',#(I)\n"
			"movC\tB,I\n"
			">1\tI,8\n";
static char L40[]=	"GBKAmovfo\tI,#(I)\n";
static char L41[]=	"GBKI<1\tJ,8\n"
			"movC\tJ,#(I)\n"
			">1\tJ,8\n"
			"mov\tJ,I\n";
static char L42[]=	"KAGJmovf\tI,#(J)\n";
static char L43[]=	"KAGJmovfo\tI,#(J)\n";
static char L44[]=	"GDKA<1\tI,8\n"
			"mov\t(sp)+,r0\n"
			"movC\tI,(r0)\n"
			">1\tI,8\n";
static char L45[]=	"GDKAmov\t(sp)+,r0\n"
			"movfo\tI,(r0)\n";
static char L46[]=	"KAmov\tI+,A+\n"
			"mov\tI,A\n";
static char L47[]=	"KAGJmov\tI+,#+2(J)\n"
			"mov\tI,#(J)\n";
static char L48[]=	"GDKAmov\t(sp)+,r0\n"
			"mov\tI,(r0)+\n"
			"mov\tI+,(r0)\n";
#define cr16 &optab[90]

static char L49[]=	"KAli\tr0,Z\n"
			"szcC\tr0,A'\n"
			"socC\tI,A''\n";
#define L50 fas1

static char L51[]=	"KCGBli\tr0,Z\n"
			"szcC\tr0,#(I)\n"
			"socC\t(sp),#(I)\n"
			"mov\t(sp)+,I\n";
#define cr45 &optab[94]

static char L52[]=	"GAM\tI,B\n";
static char L53[]=	"GAmov\tB,r0\n"
			">2\tr0,8\n"
			"jeq\t.+4\n"
			"M\tI,0\n";
static char L54[]=	"GAKImov\tJ,r0\n"
			"jeq\t.+4\n"
			"M\tI,0\n";
static char L55[]=	"KCGAmov\t(sp)+,r0\n"
			"jeq\t.+4\n"
			"M\tI,0\n";
static char L56[]=	"GA!li\tr0,B\n"
			"bl\tM'\n";
static char L57[]=	"GA!movD\tB,r0\n"
			">2\tr0,8\n"
			"jeq\t.+6\n"
			"bl\tM'\n";
static char L58[]=	"GA!KImov\tJ,r0\n"
			"jeq\t.+6\n"
			"bl\tM'\n";
static char L59[]=	"KCGA!mov\t(sp)+,r0\n"
			"jeq\t.+6\n"
			"bl\tM'\n";
#define cr91 &optab[111]

static char L60[]=	"GAM\tI\n";
static char L61[]=	"GAMt\tI\n";
#define cr40 &optab[114]

static char L62[]=	"GA";
static char L63[]=	"li\tI,A\n"
			"M\"\tI,B\n";
static char L64[]=	"movC\tA,I\n"
			">1\tI,8\n"
			"M\"\tI,B\n";
static char L65[]=	"GAM\"\tI,B\n";
#define add1 L66

static char L66[]=	"GAMD\tB,I\n";
#define add2 L67

static char L67[]=	"GAKJMD\t\"(J),I\n";
#define add3 L68

static char L68[]=	"GAKIMP\tJ,I\n";
#define add5 L69

static char L69[]=	"KCGAMP\t(sp)+,I\n";
static char L70[]=	"GAM\"\tI+,B\n"
			"V\tI\n";
static char L71[]=	"GAM\"\tI,B\n"
			"M\"\tI+,B+\n"
			"V\tI\n";
static char L72[]=	"GAM\tB,I+\n"
			"V\tI\n";
static char L73[]=	"GAKIM\tJ,I+\n"
			"V\tI\n";
static char L74[]=	"GAM\tB,I\n"
			"M\tB+,I+\n"
			"V\tI\n";
#define addl1 L75

static char L75[]=	"GAKIM\tJ+,I+\n"
			"M\tJ,I\n"
			"V\tI\n";
#define addl2 L76

static char L76[]=	"KCGAM\t(sp)+,I\n"
			"M\t(sp)+,I+\n"
			"V\tI\n";
#define cr49 &optab[148]

#define L77 add3

static char L78[]=	"GCKAM\t(sp)+,I\n";
#define L79 addl1

static char L80[]=	"KCGAM\t(sp)+,I\n"
			"M\t(sp)+,I+\n";
#define cr42 &optab[159]

static char L81[]=	"li\tI,B\n"
			"movb\tA,r0\n"
			">1\tr0,8\n"
			"M\tr0,I\n";
static char L82[]=	"li\tI,B\n"
			"M\tA,I\n";
static char L83[]=	"GAli\tr0,B\n"
			"M\tr0,I\n";
static char L84[]=	"GAMD\tB,I\n";
static char L85[]=	"GAKJMD\t\"(J),I\n";
static char L86[]=	"GAKIMP\tJ,I\n";
static char L87[]=	"KCGAMP\t(sp)+,I\n";
#define cr43 &optab[171]

static char L88[]=	"GA!KI!bl\tM\n";
static char L89[]=	"KCGA!mov\t(sp)+,J\n"
			"bl\tM\n";
#define L90 add1

#define L91 add2

#define L92 add3

#define L93 add5

#define cr14 &optab[178]

static char L94[]=	"GAli\tr0,B\n"
			"div\tr0,I\n";
#define cr70 &optab[181]

static char L95[]=	"M'\tA'\n"
			"xxx\n"
			"mov\tA,I\n";
static char L96[]=	"M't\tA'\n"
			"mov\tA,I\n";
static char L97[]=	"movC\tA',I\n"
			">1\tI,8\n"
			"M\"\tI,B\n"
			"<1\tI,8\n"
			"movC\tI,A\n"
			">1\tI,8\n";
#define addq1 L98

static char L98[]=	"M\tB,A'\n"
			"mov\tA,I\n";
#define addq20 L99

static char L99[]=	"movC\tA',I\n"
			">1\tI,8\n"
			"M\tI,B\n"
			"<1\tI,8\n"
			"movC\tI,A\n"
			">1\tI,8\n";
#define addq1a L100

static char L100[]=	"movC\tA',I\n"
			"MP\tB,I\n"
			"movC\tI,A\n";
#define addq2 L101

static char L101[]=	"KBM\t\"(I),A'\n"
			"mov\tA,I\n";
#define addq3 L102

static char L102[]=	"KAM\tI,A'\n"
			"mov\tA,I\n";
#define addq21 L103

static char L103[]=	"KCmovC\tA',I\n"
			">1\tI,8\n"
			"M\t(sp)+,I\n"
			"<1\tI,8\n"
			"movC\tI,A\n"
			">1\tI,8\n";
#define addq4 L104

static char L104[]=	"KBGJM\t\"(I),#(J)\n"
			"mov\t#(J),I\n";
#define addq4a L105

static char L105[]=	"movf\tA',I\n"
			"KIMP\tJ,I\n"
			"movf\tI,A\n";
#define addq5 L106

static char L106[]=	"KCmovC\tA',I\n"
			"MP\t(sp)+,I\n"
			"movC\tI,A\n";
#define addq6 L107

static char L107[]=	"KCmovof\tA',I\n"
			"MP\t(sp)+,I\n"
			"movfo\tI,A''\n";
#define addq7 L108

static char L108[]=	"KAGJM\tI,#(J)\n"
			"mov\t#(J),I\n";
#define addq8 L109

static char L109[]=	"KCGBM\t(sp)+,#(I)\n"
			"mov\t#(I),I\n";
#define addq9 L110

static char L110[]=	"KCGBmov\tI,r1\n"
			"movC\t#(r1),I\n"
			">1\tI,8\n"
			"MP\t(sp)+,I\n"
			"<1\tI,8\n"
			"movC\tI,#(r1)\n"
			">1\tI,8\n";
#define addq22 L111

#define L111 addq9

#define addq9a L112

static char L112[]=	"KCGBmovC\t#(I),I\n"
			"MP\t(sp)+,I\n"
			"movC\tI,#(I)\n";
#define addq10 L113

static char L113[]=	"KCGBmovof\t#(I),J\n"
			"MP\t(sp)+,J\n"
			"movfo\tJ,#(I)\n"
			"movf\tJ,I\n";
static char L114[]=	"M'\tA+\n"
			"V\tA\n"
			"GA";
static char L115[]=	"M't\tA+\n"
			"V\tA\n"
			"GA";
#define addq11 L116

static char L116[]=	"li\tI,B\n"
			"M\tI,A+\n"
			"V\tA\n"
			"GA";
#define addq12 L117

static char L117[]=	"M\tB+,A+\n"
			"V\tA\n"
			"M\tB,A\n"
			"GA";
#define addq13 L118

static char L118[]=	"KAM\tI+,A+\n"
			"V\tA\n"
			"M\tI,A\n"
			"GA";
#define addq14 L119

static char L119[]=	"GBli\tr0,B\n"
			"M\tr0,#+2(I)\n"
			"V\t#(I)\n"
			"mov\t#+2(I),I+\n"
			"mov\t#(I),I\n";
static char L120[]=	"GBli\tr0,B+\n"
			"M\tr0,#+2(I)\n"
			"V\t#(I)\n"
			"li\tr0,B\n"
			"M\tr0,#(I)\n"
			"mov\t#+2(I),I+\n"
			"mov\t#(I),I\n";
#define addq15 L121

static char L121[]=	"GBM\tB+,#+2(I)\n"
			"V\t#(I)\n"
			"M\tB,#(I)\n"
			"mov\t#+2(I),I+\n"
			"mov\t#(I),I\n";
#define addq16 L122

static char L122[]=	"KCGBM\t(sp)+,#(I)\n"
			"M\t(sp)+,#+2(I)\n"
			"V\t#(I)\n"
			"mov\t#+2(I),I+\n"
			"mov\t#(I),I\n";
#define cr72 &optab[231]

static char L123[]=	"movC\tA',I\n"
			">1\tI,8\n"
			"li\tJ,B\n"
			"M\tJ,I\n"
			"<1\tJ,8\n"
			"movC\tJ,A\n";
static char L124[]=	"movC\tA',I\n"
			">1\tI,8\n"
			"movC\tB,J\n"
			">2\tJ,8\n"
			"M\tJ,I\n"
			"<1\tJ,8\n"
			"movC\tJ,A\n";
static char L125[]=	"movC\tA',I\n"
			">1\tI,8\n"
			"M\tB,I\n"
			"<1\tJ,8\n"
			"movC\tJ,A\n";
static char L126[]=	"KCmovC\tA',I\n"
			">1\tI,8\n"
			"M\t(sp)+,I\n"
			"<1\tJ,8\n"
			"movC\tJ,A\n";
static char L127[]=	"GDKAmov\tI,r0\n"
			"mov\t(sp)+,r1\n"
			"movC\t#(r1),I\n"
			">1\tI,8\n"
			"M\tr0,I\n"
			"<1\tJ,8\n"
			"movC\tJ,#(r1)\n";
#define L128 addq1a

#define L129 addq4a

#define L130 addq5

#define L131 addq6

#define L132 addq9a

#define L133 addq10

#define cr73 &optab[250]

static char L134[]=	"movC\tA',I\n"
			">1\tI,8\n"
			"KI!bl\tM\n"
			"<1\tI,8\n"
			"movC\tI,A\n"
			">1\tI,8\n";
static char L135[]=	"KCmovC\tA',I\n"
			">1\tI,8\n"
			"mov\t(sp)+,J\n"
			"bl\tM\n"
			"<1\tI,8\n"
			"movC\tI,A\n"
			">1\tI,8\n";
static char L136[]=	"GDKA!mov\tI,J\n"
			"mov\t(sp)+,r13\n"
			"movC\t#(r13),I\n"
			">1\tI,8\n"
			"bl\tM\n"
			"<1\tI,8\n"
			"movC\tI,#(r13)\n"
			">1\tI,8\n";
#define L137 addq1a

#define L138 addq4a

#define L139 addq5

#define L140 addq6

#define L141 addq9a

#define L142 addq10

#define cr79 &optab[263]

static char L143[]=	"KAM\tA',I\n"
			"mov\tI,A\n";
static char L144[]=	"KCmovb\tA',I\n"
			">1\tI,8\n"
			"M\t(sp)+,I\n"
			"<1\tI,8\n"
			"movb\tI,A\n"
			">1\tI,8\n";
static char L145[]=	"GDKAmov\t(sp)+,r1\n"
			"movb\t#(r1),r0\n"
			">1\tr0,8\n"
			"M\tr0,I\n"
			"<1\tI,8\n"
			"movb\tI,#(r1)\n"
			">1\tI,8\n";
static char L146[]=	"GDKAmov\t(sp)+,r1\n"
			"M\t(r1),I\n"
			"mov\tI,(r1)\n";
#define cr75 &optab[270]

static char L147[]=	"M\tA,B\n"
			"mov\tA,I\n";
static char L148[]=	"KAmov\tI,r0\n"
			"jeq\t.+4\n"
			"M\tA,0\n"
			"mov\tA,I\n";
static char L149[]=	"movC\tA,I\n"
			">1\tI,8\n"
			"M\tI,B\n"
			"<1\tI,8\n"
			"movC\tI,A\n";
static char L150[]=	"KAmov\tI,r0\n"
			"movC\tA,I\n"
			">1\tI,8\n"
			"M\tI,0\n"
			"<1\tI,8\n"
			"movC\tI,A\n";
static char L151[]=	"GA!li\tr0,B\n"
			">2\tr0,8\n"
			"jeq\t.+6\n"
			"bl\tM'\n"
			"mov\tI+,A+\n"
			"mov\tI,A\n";
static char L152[]=	"GA!KImov\tJ,r0\n"
			"jeq\t.+6\n"
			"bl\tM'\n"
			"mov\tI+,A+\n"
			"mov\tI,A\n";
static char L153[]=	"KCGA!mov\t(sp)+,r0\n"
			"jeq\t.+6\n"
			"bl\tM'\n"
			"mov\tI+,A+\n"
			"mov\tI,A\n";
static char L154[]=	"KCGA!mov\t(sp)+,r0\n"
			"mov\t(sp)+,r0\n"
			"jeq\t.+6\n"
			"bl\tM'\n"
			"mov\tI+,A+\n"
			"mov\tI,A\n";
#define cr78 &optab[285]

#define L155 addq1

static char L156[]=	"ML\tB,A'\n"
			"clr\tI\n"
			"bisb\tA'',I\n";
#define L157 addq1a

#define L158 addq2

#define L159 addq3

static char L160[]=	"KCML\t(sp)+,A'\n"
			"clr\tI\n"
			"bisb\tA'',I\n";
#define L161 addq4

#define L162 addq4a

#define L163 addq5

#define L164 addq6

#define L165 addq7

#define L166 addq8

#define L167 addq9

static char L168[]=	"GDKCML\t(sp),*2(sp)\n"
			"tst\t(sp)+\n"
			"clr\tI\n"
			"bisb\t*(sp)+,I\n";
#define L169 addq9a

#define L170 addq10

#define L171 addq11

#define L172 addq12

#define L173 addq13

#define L174 addq14

#define L175 addq15

#define L176 addq16

#define cr51 &optab[324]

static char L177[]=	"movif\tA,I\n";
static char L178[]=	"GBmovif\t#(I),I\n";
static char L179[]=	"GAmovif\tI,I\n";
#define cr52 &optab[328]

static char L180[]=	"GAmovfi\tI,I\n";
#define cr56 &optab[330]

static char L181[]=	"GAsetl\n"
			"dect\tsp\n"
			"dect\tsp\n"
			"movfi\tI,(sp)\n"
			"mov\t(sp)+,I\n"
			"mov\t(sp)+,I+\n"
			"seti\n";
#define cr57 &optab[332]

static char L182[]=	"setl\n"
			"movif\tA,I\n"
			"seti\n";
static char L183[]=	"GBsetl\n"
			"movif\t#(I),I\n"
			"seti\n";
static char L184[]=	"GCsetl\n"
			"movif\t(sp)+,I\n"
			"seti\n";
#define cr127 &optab[336]

static char L185[]=	"mov\tA+,(sp)\n"
			"dect\tsp\n"
			"mov\tA,(sp)\n"
			"bl\tM\n"
			"c\t(sp)+,(sp)+\n";
static char L186[]=	"GBmov\t#+2(I),(sp)\n"
			"dect\tsp\n"
			"mov\t#(I),(sp)\n"
			"bl\tM\n"
			"c\t(sp)+,(sp)+\n";
static char L187[]=	"GCbl\tM\n"
			"c\t(sp)+,(sp)+\n";
#define cr58 &optab[340]

static char L188[]=	"GI!clr\tI\n";
static char L189[]=	"GAmov\tI,J\n"
			"clr\tI\n";
static char L190[]=	"GI!mov\tJ,I\n"
			"sra\tI,15\n";
static char L191[]=	"GAmov\tI,J\n"
			"sra\tI,15\n";
#define cr59 &optab[345]

static char L192[]=	"mov\tA+,I\n";
static char L193[]=	"GBmov\t#+2(I),I\n";
#define cr82 &optab[350]

#define l82 L194

static char L194[]=	"KCGCbl\tM\n"
			"ai\tsp,8\n";
#define cr121 &optab[355]

#define L195 l82

#define cr124 &optab[359]

#define L196 l86

#define cr86 &optab[364]

#define l86 L197

static char L197[]=	"KCGCbl\tM\n"
			"ai\tsp,6\n";
#define cr109 &optab[367]

static char L198[]=	"GAsla\tI,8\n"
			"sra\tI,8\n";
#define cr117 &optab[369]

static char L199[]=	"GATli\tr0,B\n"
			"M\tr0,I-\n";
static char L200[]=	"GATM\tB,I-\n";
static char L201[]=	"GATKJM\t\"(J),I-\n";
static char L202[]=	"GATKIM\tJ,I-\n";
static char L203[]=	"KCGATM\t(sp)+,I-\n";
#define cr119 &optab[375]

static char L204[]=	"movC\tA',I\n"
			">1\tI,8\n"
			"Tli\tr0,B\n"
			"M\tr0,I-\n"
			"<1\tI=,8\n"
			"movC\tI=,A\n"
			">1\tI=,8\n";
static char L205[]=	"movC\tA',I\n"
			">1\tI,8\n"
			"TM\tB,I-\n"
			"<1\tI=,8\n"
			"movC\tI=,A\n"
			">1\tI=,8\n";
static char L206[]=	"KCmovC\tA',I\n"
			">1\tI,8\n"
			"TM\t(sp)+,I-\n"
			"<1\tI=,8\n"
			"movC\tI=,A\n"
			">1\tI=,8\n";
static char L207[]=	"KCGJmovC\t#(J),I\n"
			">1\tI,8\n"
			"TM\t(sp)+,I-\n"
			"<1\tI=,8\n"
			"movC\tI=,#(J)\n"
			">1\tI=,8\n";
static char L208[]=	"GDKCmov\t@2(sp),r1\n"
			"movC\t#(r1),I\n"
			">1\tI,8\n"
			"TM\t(sp)+,I-\n"
			"<1\tI=,8\n"
			"movC\tI=,#(J)\n"
			">1\tI=,8\n"
			"inct\tsp\n";
#define cr107 &optab[386]

static char L209[]=	"GA?sra\tI,1\n";
#define cr130 &optab[388]

static char L210[]=	"GAli\tJ,B\n"
			"s\tJ,I\n";
static char L211[]=	"GAli\tr0,B\n"
			"s\tr0,I+\n"
			"V\tI\n";
#define ci80 &optab[391]

static char L212[]=	"M'\tA\n";
static char L213[]=	"M'\tr0\n"
			"MC\tr0,A\n";
static char L214[]=	"li\tA,B\n";
static char L215[]=	"MD\tB,A\n"
			">2\tA,8\n";
static char L216[]=	"KAM\tI,A\n";
static char L217[]=	"li\tI,256*B\n"
			"MC\tI,A\n";
static char L218[]=	"MC\tB,A\n";
static char L219[]=	"KA<1\tI,8\n"
			"MC\tI,A\n";
static char L220[]=	"M'C\tA\n";
static char L221[]=	"GBclr\tr0\n"
			"MC\tr0,#(I)\n";
#define move2 L222

static char L222[]=	"GBM'C\t#(I)\n";
static char L223[]=	"li\tA,B\n";
static char L224[]=	"li\tI,B\n"
			"M\tI,A\n";
static char L225[]=	"li\tI,B*256\n"
			"ML\tI,A\n";
#define move3 L226

static char L226[]=	"M\tB,A\n";
#define move4 L227

static char L227[]=	"KBmovD\t\"(I),I\n"
			">2\tI,8\n"
			"<1\tI,8\n"
			"MC\tI,A\n";
#define move5 L228

static char L228[]=	"KA<1\tI,8\n"
			"MC\tI,A\n";
static char L229[]=	"GBli\tr1,B*256\n"
			"MC\tr1,#(I)\n";
static char L230[]=	"GBli\tJ,B\n"
			"MC\tJ,#(I)\n";
static char L231[]=	"GBmovb\tB,#(I)\n";
#define move6 L232

static char L232[]=	"GBmov\tB,r0\n"
			"<1\tr0,8\n"
			"MC\tr0,#(I)\n";
static char L233[]=	"GBM\tB,#(I)\n";
#define move7 L234

static char L234[]=	"GBKJML\t\"(J),#(I)\n";
#define move8 L235

static char L235[]=	"GBKI<1\tJ,8\n"
			"MC\tJ,#(I)\n";
#define move9 L236

static char L236[]=	"KBGJML\t\"(I),#(J)\n";
#define move10 L237

static char L237[]=	"KAGJ<1\tI,8\n"
			"MC\tI,#(J)\n";
#define move11 L238

static char L238[]=	"GDKBmov\t(sp)+,r1\n"
			"ML\t\"(I),#(r1)\n";
#define move12 L239

static char L239[]=	"GDKAmov\t(sp)+,r1\n"
			"MC\tI,#(r1)\n";
static char L240[]=	"KAmovfi\tI,A\n";
static char L241[]=	"KAGJmovfi\tI,#(J)\n";
static char L242[]=	"clr\tA\n"
			"clr\tA+\n";
static char L243[]=	"GBclr\t#(I)\n"
			"clr\t#+2(I)\n";
static char L244[]=	"li\tr0,B\n"
			"M\tr0,A+\n"
			"sra\tr0,15\n"
			"M\tr0,A\n";
#define move13a L245

static char L245[]=	"M\tB,A+\n"
			"V\tA\n";
static char L246[]=	"KBmov\t\"(I),A+\n"
			"V\tA\n";
static char L247[]=	"KAmov\tI,A+\n"
			"V\tA\n";
static char L248[]=	"KAsetl\n"
			"movfi\tI,A\n"
			"seti\n";
static char L249[]=	"KAGJsetl\n"
			"movfi\tI,#(J)\n"
			"seti\n";
static char L250[]=	"li\tr0,B\n"
			"M\tr0,A\n"
			"li\tr0,B+\n"
			"M\tr0,A+\n";
#define move13 L251

static char L251[]=	"M\tB,A\n"
			"M\tB+,A+\n"
			"V\tA\n";
#define move14 L252

static char L252[]=	"KBM\t\"(I),A\n"
			"M\t\"+2(I),A+\n"
			"V\tA\n";
#define move15 L253

static char L253[]=	"KAM\tI,A\n"
			"M\tI+,A+\n"
			"V\tA\n";
#define move14a L254

static char L254[]=	"GBM\tB,#+2(I)\n"
			"V\t#(I)\n";
#define move16a L255

static char L255[]=	"GBM\tB+,#+2(I)\n"
			"V\t#(I)\n"
			"M\tB,#(I)\n";
#define move16 L256

static char L256[]=	"KAGJM\tI+,#+2(J)\n"
			"V\t#(J)\n"
			"M\tI,#(J)\n";
static char L257[]=	"KCGBmov\t(sp)+,#+2(I)\n"
			"V\t#(I)\n";
#define move17 L258

static char L258[]=	"KCGBM\t(sp)+,#(I)\n"
			"M\t(sp)+,#+2(I)\n"
			"V\t#(I)\n";
#define ci78 &optab[496]

static char L259[]=	"M\"\tA,B\n";
static char L260[]=	"GAM\"\tI,B\n"
			"<1\tI,8\n"
			"movC\tI,A\n";
#define L261 move3

static char L262[]=	"KAML\tI,A\n";
#define L263 move5

static char L264[]=	"GBmovC\t#(I),r0\n"
			"M\"\tr0,B*256\n"
			"movC\tr0,#(I)\n";
static char L265[]=	"GBmov\t#(I),r0\n"
			"M\"\tr0,B\n"
			"mov\tr0,#(I)\n";
#define L266 move6

#define L267 move7

#define L268 move8

#define L269 move9

#define L270 move10

#define L271 move11

#define L272 move12

#define L273 move13a

#define L274 move13

#define L275 move14

#define L276 move15

#define L277 move14a

#define L278 move16a

#define L279 move16

#define L280 move17

#define ci79 &optab[552]

static char L281[]=	"KAM\tA,I\n"
			"M\tA+,I+\n"
			"mov\tI,A\n"
			"mov\tI+,A+\n";
static char L282[]=	"KAGJM\t#(J),I\n"
			"M\t#+2(J),I+\n"
			"mov\tI,#(J)\n"
			"mov\tI+,#+2(J)\n";
static char L283[]=	"GDKAmov\t(sp)+,r1\n"
			"M\t#(r1),I\n"
			"M\t#+2(r1),I+\n"
			"mov\tI,#(J)\n"
			"mov\tI+,#+2(J)\n";
#define ci70 &optab[565]

static char L284[]=	"M'\tA\n";
static char L285[]=	"M't\tA\n";
static char L286[]=	"M\"\tA,B\n";
static char L287[]=	"li\tr0,B*256\n"
			"Mb\tr0,A\n";
static char L288[]=	"li\tr0,B\n"
			"M\tr0,A\n";
#define L289 move3

#define L290 move4

#define L291 move5

static char L292[]=	"GBmov\t#(I),r0\n"
			"M'\t#(I)\n"
			"mov\tr0,r0\n";
#define L293 move9

static char L294[]=	"KBmovC\tA',J\n"
			">1\tJ,8\n"
			"M\t\"(I),J\n"
			"<1\tJ,8\n"
			"movC\tJ,A\n";
static char L295[]=	"KAmovC\tA',J\n"
			">1\tJ,8\n"
			"M\tI,J\n"
			"<1\tJ,8\n"
			"movC\tJ,A\n";
#define L296 move10

#define L297 move12

static char L298[]=	"KCGBmovC\t#(I),J\n"
			">1\tJ,8\n"
			"M\t(sp)+,J\n"
			"<1\tJ,8\n"
			"movC\tJ,#(I)\n";
static char L299[]=	"li\tr0,B\n"
			"M\tr0,A+\n"
			"V\tA\n"
			"sra\tr0,15\n"
			"M\tr0,A\n";
static char L300[]=	"li\tr0,B\n"
			"M\tr0,A\n"
			"li\tr0,B+\n"
			"M\tr0,A+\n"
			"V\tA\n";
#define L301 move13a

#define L302 move13

#define L303 move14

#define L304 move15

#define L305 move14a

#define L306 move16a

#define L307 move16

#define L308 move17

#define ci16 &optab[620]

static char L309[]=	"li\tr0,Z*256\n"
			"szcb\tr0,A'\n"
			"li\tr0,B*256\n"
			"socb\tr0,A\n";
static char L310[]=	"li\tr0,Z\n"
			"szc\tr0,A'\n"
			"li\tr0,B\n"
			"soc\tr0,A\n";
static char L311[]=	"li\tr0,Z\n"
			"szcC\tr0,A'\n"
			"li\tr0,B\n"
			"socC\tr0,A\n";
static char L312[]=	"KAli\tr0,Z\n"
			"szcC\tr0,A'\n"
			"<1\tI,8\n"
			"socC\tI,A\n";
static char L313[]=	"GBli\tr0,Z\n"
			"szcC\tr0,#(I)\n"
			"li\tr0,B\n"
			"socC\tr0,#(I)\n";
#define fas1 L314

static char L314[]=	"KAGJli\tr0,Z\n"
			"szcC\tr0,#(J)\n"
			"socC\tI,#(J)\n";
static char L315[]=	"GBKIli\tr0,Z\n"
			"szcC\tr0,#(I)\n"
			"socC\tJ,#(I)\n";
static char L316[]=	"KCGBli\tr0,Z\n"
			"szcC\tr0,#(I)\n"
			"socC\t(sp)+,#(I)\n";
#define cc60 &optab[629]

static char L317[]=	"li\tr0,A\n";
static char L318[]=	"movC\tA,r0\n";
static char L319[]=	"movof\tA,I\n";
static char L320[]=	"GBmovC\t#(I),r0\n";
static char L321[]=	"GBmovof\t#(I),I\n";
static char L322[]=	"GE";
static char L323[]=	"ci\tA,B\n";
static char L324[]=	"GAci\tI,B\n";
static char L325[]=	"ML\tA,B\n";
static char L326[]=	"GBML\t#(I),B\n";
static char L327[]=	"GAMD\tI,B\n";
static char L328[]=	"GBKJML\t#(I),\"(J)\n";
static char L329[]=	"GBKIMC\t#(I),J\n";
static char L330[]=	"GAKJMD\tI,\"(J)\n";
static char L331[]=	"GAKIMP\tI,J\n";
static char L332[]=	"KCGAMP\tI,(sp)+\n";
static char L333[]=	"mov\tA,r0\n"
			"X0mov\tA+,r0\n"
			"X1";
static char L334[]=	"mov\tA,r0\n"
			"X0mov\tA+,r0\n"
			"ci\tr0,B\n"
			"X1";
static char L335[]=	"mov\tA,r0\n"
			"ci\tr0,B\n"
			"X0mov\tA+,r0\n"
			"ci\tr0,B+\n"
			"X1";
static char L336[]=	"li\tr0,A\n"
			"M\tr0,B\n"
			"X0li\tr0,A+\n"
			"M\tr0,B+\n"
			"X1";
static char L337[]=	"mov\tA,r0\n"
			"X0M\tA+,B\n"
			"X1";
#define lcmp1 L338

static char L338[]=	"M\tA,B\n"
			"X0M\tA+,B+\n"
			"X1";
static char L339[]=	"GBmov\t#(I),r0\n"
			"X0mov\t#+2(I),r0\n"
			"X1";
static char L340[]=	"GBmov\t#(I),r0\n"
			"X0mov\t#+2(I),r0\n"
			"ci\tr0,B\n"
			"X1";
static char L341[]=	"GBmov\t#(I),r0\n"
			"X0M\t#+2(I),B\n"
			"X1";
#define lcmp2 L342

static char L342[]=	"GBM\t#(I),B\n"
			"X0M\t#+2(I),B+\n"
			"X1";
static char L343[]=	"GAmov\tI,r0\n"
			"X0mov\tI+,r0\n"
			"X1";
static char L344[]=	"GAmov\tI,r0\n"
			"X0ci\tI+,B\n"
			"X1";
static char L345[]=	"GAci\tI,B\n"
			"X0ci\tI+,B+\n"
			"X1";
static char L346[]=	"GAmov\tI,r0\n"
			"X0M\tI+,B\n"
			"X1";
#define lcmp3 L347

static char L347[]=	"GAM\tI,B\n"
			"X0M\tI+,B+\n"
			"X1";
#define lcmp4 L348

static char L348[]=	"GBKJM\t#(I),\"(J)\n"
			"X0M\t#+2(I),\"+2(J)\n"
			"X1";
#define lcmp5 L349

static char L349[]=	"GAKJM\tI,\"(J)\n"
			"X0M\tI+,\"+2(J)\n"
			"X1";
#define lcmp6 L350

static char L350[]=	"GCKAQmov\tI,(sp)\n"
			"mov\t@4(sp),I\n"
			"mov\t(sp)+,@2(sp)\n"
			"M\t(sp)+,(sp)+\n"
			"X0M\tI,I+\n"
			"X1";
#define cc81 &optab[709]

static char L351[]=	"movb\tA,I\n"
			"M\"\tI,B*256\n";
static char L352[]=	"GAM\"\tI,B\n";
static char L353[]=	"KAmovC\tA,r0\n"
			">1\tr0,8\n"
			"M\tI,r0\n";
#define L354 move6

#define L355 add1

#define L356 add3

#define L357 add5

#define rest &optab[720]

static char L358[]=	"HA";
#define cs106 &optab[723]

static char L359[]=	"Qclr\t(sp)\n";
static char L360[]=	"Qli\tr0,A\n"
			"mov\tr0,(sp)\n";
static char L361[]=	"Qmovb\tA,r0\n"
			">1\tr0,8\n"
			"mov\tr0,(sp)\n";
static char L362[]=	"Qmov\tA,(sp)\n";
static char L363[]=	"GBQmov\t#(I),(sp)\n";
static char L364[]=	"Qli\tr0,A+\n"
			"mov\tr0,(sp)\n"
			"Qli\tr0,A\n"
			"mov\tr0,(sp)\n";
static char L365[]=	"Qmov\tA+,(sp)\n"
			"Qmov\tA,(sp)\n";
#define cs91 &optab[734]

static char L366[]=	"GCM\t(sp)\n";
static char L367[]=	"GCMt\t(sp)\n";
#define cs40 &optab[737]

static char L368[]=	"QmovC\tA,r0\n"
			">1\tr0,8\n"
			"M\"\tr0,B\n"
			"mov\tr0,(sp)\n";
static char L369[]=	"GCmov\t(sp),r0\n"
			"M\"\tr0,B\n"
			"mov\tr0,(sp)\n";
static char L370[]=	"GCM\tB,(sp)\n";
static char L371[]=	"GCKBM\t\"(I),(sp)\n";
static char L372[]=	"GCKAM\tI,(sp)\n";
#define cs58 &optab[745]

static char L373[]=	"Qli\tr0,A\n"
			"mov\tr0,(sp)\n"
			"sra\tr0,15\n"
			"Qmov\tr0,(sp)\n";
static char L374[]=	"GCQclr\t(sp)\n";
static char L375[]=	"Qmov\tA,r0\n"
			"mov\tr0,(sp)\n"
			"sra\tr0,15\n"
			"Qmov\tr0,(sp)\n";
#define cs56 &optab[749]

static char L376[]=	"GAsetl\n"
			"Qmovfi\tI,(sp)\n"
			"seti\n";
#define ci116 &optab[751]

static char L377[]=	"GA!KI!";
static char L378[]=	"KCGA!mov\t(sp)+,r1\n";

/* goto (are these entries still used?) */
/* cr102 */

struct optab optab[]={
	{DFIX,		0,		DALL,		0,		L1},	/* 0 */
	{DPTR+DALL,	0,		DALL,		0,		L2},	/* 1 */
/* call */
	{0},
/* cr100 */
	{DFIX,		0,		DALL,		0,		L3},	/* 3 */
	{DPTR+DALL,	0,		DALL,		0,		L4},	/* 4 */
	{DALL,		0,		DALL,		0,		L5},	/* 5 */
/* load */
	{0},
/* cr106 */
	{DZER,		0,		DALL,		0,		L6},	/* 7 */
	{DZER,		FLOAT+2,	DALL,		0,		L7},	/* 8 */
	{DCON,		0,		DALL,		0,		L8},	/* 9 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L9},	/* 10 */
	{DFIX,		CHAR+2,		DALL,		0,		L9},	/* 11 */
	{DFIX,		0,		DALL,		0,		L9},	/* 12 */
	{DFIX,		DOUBLE+2,	DALL,		0,		L9},	/* 13 */
	{DFIX,		FLOAT+2,	DALL,		0,		L10},	/* 14 */
	{DPTR+DALL,	UNCHAR+2,	DALL,		0,		L11},	/* 15 */
	{DPTR+DALL,	CHAR+2,		DALL,		0,		L11},	/* 16 */
	{DPTR+DALL,	0,		DALL,		0,		L11},	/* 17 */
	{DPTR+DALL,	DOUBLE+2,	DALL,		0,		L11},	/* 18 */
	{DPTR+DALL,	FLOAT+2,	DALL,		0,		L12},	/* 19 */
	{DCON,		LONG+2,		DALL,		0,		L13},	/* 20 */
	{DCON,		UNLONG+2,	DALL,		0,		L13},	/* 21 */
	{DFIX,		LONG+2,		DALL,		0,		L14},	/* 22 */
	{DFIX,		UNLONG+2,	DALL,		0,		L14},	/* 23 */
	{DPTR+DALL,	LONG+2,		DALL,		0,		L15},	/* 24 */
	{DPTR+DALL,	UNLONG+2,	DALL,		0,		L15},	/* 25 */
	{DALL,		0,		DALL,		0,		L16},	/* 26 */
/* ++,	-- postfix; the right operand is always a CON */
	{0},
/* cr32 */
	{DFIX,		1,		DONE,		0,		L17},	/* 28 */
	{DFIX,		1,		DTWO,		0,		L18},	/* 29 */
	{DFIX,		1,		DALL,		0,		L19},	/* 30 */
	{DFIX,		CHAR+2,		DALL,		0,		L20},	/* 31 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L20},	/* 32 */
	{DPTR+DREG,	1,		DONE,		0,		L21},	/* 33 */
	{DPTR+DALL,	1,		DONE,		0,		L22},	/* 34 */
	{DPTR+DREG,	1,		DALL,		0,		L23},	/* 35 */
	{DPTR+DREG,	CHAR+2,		DALL,		0,		L24},	/* 36 */
	{DPTR+DREG,	UNCHAR+2,	DALL,		0,		L24},	/* 37 */
	{DPTR+DALL,	1,		DALL,		0,		L25},	/* 38 */
	{DPTR+DALL,	CHAR+2,		DALL,		0,		L26},	/* 39 */
	{DPTR+DALL,	UNCHAR+2,	DALL,		0,		L26},	/* 40 */
	{DFIX,		LONG+2,		DONE,		0,		L27},	/* 41 */
	{DFIX,		UNLONG+2,	DONE,		0,		L27},	/* 42 */
	{DPTR+DREG,	LONG+2,		DONE,		0,		L28},	/* 43 */
	{DPTR+DREG,	UNLONG+2,	DONE,		0,		L28},	/* 44 */
	{DPTR+DALL,	LONG+2,		DONE,		0,		L29},	/* 45 */
	{DPTR+DALL,	UNLONG+2,	DONE,		0,		L29},	/* 46 */
/* - unary,	 ~ */
	{0},
/* cr37 */
	{DALL,		0,		DALL,		0,		L30},	/* 48 */
	{DALL,		FLOAT+2,	DALL,		0,		L30},	/* 49 */
	{DALL,		LONG+2,		DALL,		0,		L31},	/* 50 */
	{DALL,		UNLONG+2,	DALL,		0,		L31},	/* 51 */
/* = */
	{0},
/* cr80 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L32},	/* 53 */
	{DFIX,		0,		DALL,		0,		L32},	/* 54 */
	{DFIX,		DOUBLE+2,	DALL,		FLOAT+2,	L32},	/* 55 */
	{DFIX,		FLOAT+2,	DALL,		FLOAT+2,	L33},	/* 56 */
	{DPTR+DALL,	DOUBLE+2,	DFIX,		FLOAT+2,	L34},/* 57 */
	{DPTR+DALL,	FLOAT+2,	DCON,		0,		L35},	/* 58 */
	{DPTR+DALL,	UNCHAR+2,	DCON,		0,		L35},	/* 59 */
	{DPTR+DALL,	0,		DCON,		0,		L36},	/* 60 */
	{DPTR+DALL,	CHAR+2,		DFIX,		CHAR+2,		L37},	/* 61 */
	{DPTR+DALL,	UNCHAR+2,	DFIX,		CHAR+2,		L37},	/* 62 */
	{DPTR+DALL,	CHAR+2,		DFIX,		UNCHAR+2,	L37},	/* 63 */
	{DPTR+DALL,	UNCHAR+2,	DFIX,		UNCHAR+2,	L37},	/* 64 */
	{DPTR+DALL,	CHAR+2,		DFIX,		0,		L38},	/* 65 */
	{DPTR+DALL,	UNCHAR+2,	DFIX,		0,		L38},	/* 66 */
	{DPTR+DALL,	0,		DFIX,		1,		L39},	/* 67 */
	{DPTR+DALL,	FLOAT+2,	DFIX,		FLOAT+2,	L40},	/* 68 */
	{DPTR+DALL,	UNCHAR+2,	DREG,		0,		L41},	/* 69 */
	{DPTR+DALL,	0,		DREG,		0,		L41},	/* 70 */
	{DPTR+DREG,	DOUBLE+2,	DALL,		FLOAT+2,	L42},	/* 71 */
	{DPTR+DREG,	FLOAT+2,	DALL,		FLOAT+2,	L43},	/* 72 */
	{DPTR+DALL,	UNCHAR+2,	DALL,		0,		L44},	/* 73 */
	{DPTR+DALL,	0,		DALL,		0,		L44},	/* 74 */
	{DPTR+DALL,	DOUBLE+2,	DALL,		FLOAT+2,	L44},	/* 75 */
	{DPTR+DALL,	FLOAT+2,	DALL,		FLOAT+2,	L45},	/* 76 */
	// dest is reg
	{DFIX,		LONG+2,		DALL,		LONG+2,		L46},	/* 77 */
	{DFIX,		LONG+2,		DALL,		UNLONG+2,	L46},	/* 78 */
	{DFIX,		UNLONG+2,	DALL,		LONG+2,		L46},	/* 79 */
	{DFIX,		UNLONG+2,	DALL,		UNLONG+2,	L46},	/* 80 */
	// dest is addr
	{DPTR+DREG,	LONG+2,		DALL,		LONG+2,		L47},	/* 81 */
	{DPTR+DREG,	LONG+2,		DALL,		UNLONG+2,	L47},	/* 82 */
	{DPTR+DREG,	UNLONG+2,	DALL,		LONG+2,		L47},	/* 83 */
	{DPTR+DREG,	UNLONG+2,	DALL,		UNLONG+2,	L47},	/* 84 */
	// dest is long *
	{DPTR+DALL,	LONG+2,		DALL,		LONG+2,		L48},	/* 85 */
	{DPTR+DALL,	LONG+2,		DALL,		UNLONG+2,	L48},	/* 86 */
	{DPTR+DALL,	UNLONG+2,	DALL,		LONG+2,		L48},	/* 87 */
	{DPTR+DALL,	UNLONG+2,	DALL,		UNLONG+2,	L48},	/* 88 */
/* field assign,	 value in reg. */
	{0},
/* cr16 */
	{DFIX,		0,		DALL,		0,		L49},	/* 90 */
	{DPTR+DREG,	0,		DALL,		0,		L50},	/* 91 */


	{DPTR+DALL,	0,		DALL,		0,		L51},	/* 92 */
/* <<,	 >>,	 unsigned >> */
	{0},
/* cr45 */
	{DALL,		UNCHAR+2,	DCON,		0,		L52},	/* 94 */
	{DALL,		0,		DCON,		0,		L52},	/* 95 */
	{DALL,		UNCHAR+2,	DFIX,		0,		L53},	/* 96 */
	{DALL,		0,		DFIX,		0,		L53},	/* 97 */
	{DALL,		UNCHAR+2,	DREG,		0,		L54},	/* 98 */
	{DALL,		0,		DREG,		0,		L54},	/* 99 */
	{DALL,		UNCHAR+2,	DALL,		0,		L55},	/* 100 */
	{DALL,		0,		DALL,		0,		L55},	/* 101 */
	{DALL,		LONG+2,		DCON,		0,		L56},	/* 102 */
	{DALL,		UNLONG+2,	DCON,		0,		L56},	/* 103 */
	{DALL,		UNLONG+2,	DFIX,		0,		L57},	/* 104 */
	{DALL,		LONG+2,		DFIX,		0,		L57},	/* 105 */
	{DALL,		UNLONG+2,	DREG,		0,		L58},	/* 106 */
	{DALL,		LONG+2,		DREG,		0,		L58},	/* 107 */
	{DALL,		UNLONG+2,	DALL,		0,		L59},	/* 108 */
	{DALL,		LONG+2,		DALL,		0,		L59},	/* 109 */
/* +1,	 +2,	 -1,	 -2 */
	{0},
/* cr91 */
	{DALL,		0,		DONE,		0,		L60},	/* 111 */
	{DALL,		0,		DTWO,		0,		L61},	/* 112 */
/* +,	 -,	 |,	 &~ */
	{0},
/* cr40 */
	{DALL,		0,		DZER,		0,		L62},	/* 114 */
	{DCON,		0,		DCON,		0,		L63},	/* 115 */
	{DFIX,		UNCHAR+2,	DCON,		0,		L64},	/* 116 */
	{DFIX,		0,		DCON,		0,		L64},	/* 117 */
	{DALL,		0,		DCON,		0,		L65},	/* 118 */
	{DALL,		0,		DFIX,		1,		L66},	/* 119 */
	{DALL,		FLOAT+2,	DFIX,		DOUBLE+2,	L66},	/* 120 */
	{DALL,		0,		DPTR+DREG,	1,		L67},	/* 121 */
	{DALL,		FLOAT+2,	DPTR+DREG,	DOUBLE+2,	L67},	/* 122 */
	{DALL,		0,		DREG,		0,		L68},	/* 123 */
	{DALL,		FLOAT+2,	DREG,		FLOAT+2,	L68},	/* 124 */
	{DALL,		0,		DALL,		0,		L69},	/* 125 */
	{DALL,		FLOAT+2,	DALL,		FLOAT+2,	L69},	/* 126 */
	{DALL,		LONG+2,		DCON,		0,		L70},	/* 127 */
	{DALL,		UNLONG+2,	DCON,		0,		L70},	/* 128 */
	{DALL,		LONG+2,		DCON,		LONG+2,		L71},	/* 129 */
	{DALL,		UNLONG+2,	DCON,		LONG+2,		L71},	/* 130 */
	{DALL,		LONG+2,		DFIX,		0,		L72},	/* 131 */
	{DALL,		UNLONG+2,	DFIX,		0,		L72},	/* 132 */
	{DALL,		LONG+2,		DREG,		0,		L73},	/* 133 */
	{DALL,		UNLONG+2,	DREG,		0,		L73},	/* 134 */
	{DALL,		LONG+2,		DFIX,		LONG+2,		L74},	/* 135 */
	{DALL,		LONG+2,		DFIX,		UNLONG+2,	L74},	/* 136 */
	{DALL,		UNLONG+2,	DFIX,		LONG+2,		L74},	/* 137 */
	{DALL,		UNLONG+2,	DFIX,		UNLONG+2,	L74},	/* 138 */
	{DALL,		LONG+2,		DREG,		LONG+2,		L75},	/* 139 */
	{DALL,		LONG+2,		DREG,		UNLONG+2,	L75},	/* 140 */
	{DALL,		UNLONG+2,	DREG,		LONG+2,		L75},	/* 141 */
	{DALL,		UNLONG+2,	DREG,		UNLONG+2,	L75},	/* 142 */
	{DALL,		LONG+2,		DALL,		LONG+2,		L76},	/* 143 */
	{DALL,		LONG+2,		DALL,		UNLONG+2,	L76},	/* 144 */
	{DALL,		UNLONG+2,	DALL,		LONG+2,		L76},	/* 145 */
	{DALL,		UNLONG+2,	DALL,		UNLONG+2,	L76},	/* 146 */
/* ^ -- xor */
	{0},
/* cr49 */
	{DALL,		0,		DREG,		0,		L77},	/* 148 */


	{DALL,		0,		DALL,		0,		L78},	/* 149 */
	{DALL,		LONG+2,		DREG,		LONG+2,		L79},	/* 150 */
	{DALL,		LONG+2,		DREG,		UNLONG+2,	L79},	/* 151 */
	{DALL,		UNLONG+2,	DREG,		LONG+2,		L79},	/* 152 */
	{DALL,		UNLONG+2,	DREG,		UNLONG+2,	L79},	/* 153 */


	{DALL,		LONG+2,		DALL,		LONG+2,		L80},	/* 154 */
	{DALL,		LONG+2,		DALL,		UNLONG+2,	L80},	/* 155 */
	{DALL,		UNLONG+2,	DALL,		LONG+2,		L80},	/* 156 */
	{DALL,		UNLONG+2,	DALL,		UNLONG+2,	L80},	/* 157 */
/* '*' -- low word of result is okay for both signed and unsigned.
 * R is increased by 1 following these snippets.
 */
	{0},
/* cr42 */
	{DFIX,		CHAR+2,		DCON,		0,		L81},	/* 159 */
	{DFIX,		0,		DCON,		0,		L82},	/* 160 */
	{DALL,		0,		DCON,		0,		L83},	/* 161 */
	{DALL,		0,		DFIX,		1,		L84},	/* 162 */
	{DALL,		FLOAT+2,	DFIX,		DOUBLE+2,	L84},	/* 163 */
	{DALL,		0,		DPTR+DREG,	1,		L85},	/* 164 */
	{DALL,		FLOAT+2,	DPTR+DREG,	DOUBLE+2,	L85},	/* 165 */
	{DALL,		0,		DREG,		0,		L86},	/* 166 */
	{DALL,		FLOAT+2,	DREG,		FLOAT+2,	L86},	/* 167 */
	{DALL,		0,		DALL,		0,		L87},	/* 168 */
	{DALL,		FLOAT+2,	DALL,		FLOAT+2,	L87},	/* 169 */
/* / and % -- signed */
	{0},
/* cr43 */
	{DALL,		0,		DREG,		0,		L88},	/* 171 */
	{DALL,		0,		DALL,		0,		L89},	/* 172 */
	{DALL,		FLOAT+2,	DFIX,		DOUBLE+2,	L90},	/* 173 */


	{DALL,		FLOAT+2,	DPTR+DREG,	DOUBLE+2,	L91},	/* 174 */


	{DALL,		FLOAT+2,	DREG,		FLOAT+2,	L92},	/* 175 */


	{DALL,		FLOAT+2,	DALL,		FLOAT+2,	L93},	/* 176 */


/* PTOI */
	{0},
/* cr14 */
	{DALL,		LONG+2,		DFIX,		0,		L94},	/* 178 */
	{DALL,		UNLONG+2,	DFIX,		0,		L94},	/* 179 */
/* +=,	 -= */
	{0},
/* cr70 */
	{DFIX,		1,		DONE,		0,		L95},	/* 181 */
	{DFIX,		1,		DTWO,		0,		L96},	/* 182 */
	{DFIX,		UNCHAR+2,	DCON,		0,		L97},	/* 183 */
	{DFIX,		0,		DCON,		0,		L97},	/* 184 */
	{DFIX,		1,		DFIX,		1,		L98},	/* 185 */
	{DFIX,		UNCHAR+2,	DFIX,		1,		L99},	/* 186 */
	{DFIX,		0,		DFIX,		1,		L99},	/* 187 */
	{DFIX,		DOUBLE+2,	DFIX,		DOUBLE+2,	L100},	/* 188 */
	{DFIX,		1,		DPTR+DALL,	1,		L101},	/* 189 */
	{DFIX,		1,		DALL,		0,		L102},	/* 190 */
	{DFIX,		0,		DALL,		0,		L103},	/* 191 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L103},	/* 192 */
	{DPTR+DREG,	1,		DPTR+DALL,	1,		L104},	/* 193 */
	{DFIX,		DOUBLE+2,	DREG,		FLOAT+2,	L105},	/* 194 */
	{DFIX,		DOUBLE+2,	DALL,		FLOAT+2,	L106},	/* 195 */
	{DFIX,		FLOAT+2,	DALL,		FLOAT+2,	L107},	/* 196 */
	{DPTR+DREG,	1,		DALL,		0,		L108},	/* 197 */
	{DPTR+DALL,	1,		DALL,		0,		L109},	/* 198 */
	{DPTR+DALL,	UNCHAR+2,	DALL,		0,		L110},	/* 199 */
	{DPTR+DALL,	0,		DALL,		0,		L110},	/* 200 */
	{DPTR+DALL,	UNCHAR+2,	DALL,		0,		L111},	/* 201 */


	{DPTR+DALL,	DOUBLE+2,	DALL,		FLOAT+2,	L112},	/* 202 */
	{DPTR+DALL,	FLOAT+2,	DALL,		FLOAT+2,	L113},	/* 203 */
	{DFIX,		LONG+2,	DONE,			0,		L114},	/* 204 */
	{DFIX,		UNLONG+2,	DONE,		0,		L114},	/* 205 */
	{DFIX,		LONG+2,	DTWO,			0,		L115},	/* 206 */
	{DFIX,		UNLONG+2,	DTWO,		0,		L115},	/* 207 */
	{DFIX,		LONG+2,	DCON,			0,		L116},	/* 208 */
	{DFIX,		UNLONG+2,	DCON,		0,		L116},	/* 209 */
	{DFIX,		LONG+2,		DFIX,		LONG+2,		L117},	/* 210 */
	{DFIX,		LONG+2,		DFIX,		UNLONG+2,	L117},	/* 211 */
	{DFIX,		UNLONG+2,	DFIX,		LONG+2,		L117},	/* 212 */
	{DFIX,		UNLONG+2,	DFIX,		UNLONG+2,	L117},	/* 213 */
	{DFIX,		LONG+2,		DALL,		LONG+2,		L118},	/* 214 */
	{DFIX,		LONG+2,		DALL,		UNLONG+2,	L118},	/* 215 */
	{DFIX,		UNLONG+2,	DALL,		LONG+2,		L118},	/* 216 */
	{DFIX,		UNLONG+2,	DALL,		UNLONG+2,	L118},	/* 217 */
	{DPTR+DALL,	LONG+2,		DCON,		0,		L119},	/* 218 */
	{DPTR+DALL,	UNLONG+2,	DCON,		0,		L119},	/* 219 */
	{DPTR+DALL,	LONG+2,		DCON,		LONG+2,		L120},	/* 220 */
	{DPTR+DALL,	UNLONG+2,	DCON,		LONG+2,		L120},	/* 221 */
	{DPTR+DALL,	LONG+2,		DFIX,		LONG+2,		L121},	/* 222 */
	{DPTR+DALL,	LONG+2,		DFIX,		UNLONG+2,	L121},	/* 223 */
	{DPTR+DALL,	UNLONG+2,	DFIX,		LONG+2,		L121},	/* 224 */
	{DPTR+DALL,	UNLONG+2,	DFIX,		UNLONG+2,	L121},	/* 225 */
	{DPTR+DALL,	LONG+2,		DALL,		LONG+2,		L122},	/* 226 */
	{DPTR+DALL,	LONG+2,		DALL,		UNLONG+2,	L122},	/* 227 */
	{DPTR+DALL,	UNLONG+2,	DALL,		LONG+2,		L122},	/* 228 */
	{DPTR+DALL,	UNLONG+2,	DALL,		UNLONG+2,	L122},	/* 229 */
/* '*=' */
	{0},
/* cr72 */
	{DFIX,		UNCHAR+2,	DCON,		0,		L123},	/* 231 */
	{DFIX,		0,		DCON,		0,		L123},	/* 232 */
	{DFIX,		UNCHAR+2,	DFIX,		CHAR+2,		L124},	/* 233 */
	{DFIX,		UNCHAR+2,	DFIX,		UNCHAR+2,	L124},	/* 234 */
	{DFIX,		0,		DFIX,		CHAR+2,		L124},	/* 235 */
	{DFIX,		0,		DFIX,		UNCHAR+2,	L124},	/* 236 */
	{DFIX,		UNCHAR+2,	DFIX,		1,		L125},	/* 237 */
	{DFIX,		0,		DFIX,		1,		L125},	/* 238 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L126},	/* 239 */
	{DFIX,		0,		DALL,		0,		L126},	/* 240 */
	{DPTR+DALL,	UNCHAR+2,	DALL,		0,		L127},	/* 241 */
	{DPTR+DALL,	0,		DALL,		0,		L127},	/* 242 */
	{DFIX,		DOUBLE+2,	DFIX,		DOUBLE+2,	L128},	/* 243 */


	{DFIX,		DOUBLE+2,	DREG,		FLOAT+2,	L129},	/* 244 */


	{DFIX,		DOUBLE+2,	DALL,		FLOAT+2,	L130},	/* 245 */


	{DFIX,		FLOAT+2,	DALL,		FLOAT+2,	L131},	/* 246 */


	{DPTR+DALL,	DOUBLE+2,	DALL,		FLOAT+2,	L132},	/* 247 */


	{DPTR+DALL,	FLOAT+2,	DALL,		FLOAT+2,	L133},	/* 248 */


/* /= and %= -- signed int */
	{0},
/* cr73 */
	{DFIX,		0,		DREG,		0,		L134},	/* 250 */
	{DFIX,		UNCHAR+2,	DREG,		0,		L134},	/* 251 */
	{DFIX,		0,		DALL,		0,		L135},	/* 252 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L135},	/* 253 */
	{DPTR+DALL,	0,		DALL,		0,		L136},	/* 254 */
	{DPTR+DALL,	UNCHAR+2,	DALL,		0,		L136},	/* 255 */
	{DFIX,		DOUBLE+2,	DFIX,		DOUBLE+2,	L137},	/* 256 */


	{DFIX,		DOUBLE+2,	DREG,		FLOAT+2,	L138},	/* 257 */


	{DFIX,		DOUBLE+2,	DALL,		FLOAT+2,	L139},	/* 258 */


	{DFIX,		FLOAT+2,	DALL,		FLOAT+2,	L140},	/* 259 */


	{DPTR+DALL,	DOUBLE+2,	DALL,		FLOAT+2,	L141},	/* 260 */


	{DPTR+DALL,	FLOAT+2,	DALL,		FLOAT+2,	L142},	/* 261 */


/* ^= -- =xor */
	{0},
/* cr79 */
	{DFIX,		1,		DALL,		0,		L143},	/* 263 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L144},	/* 264 */
	{DFIX,		CHAR+2,		DALL,		0,		L144},	/* 265 */
	{DPTR+DALL,	UNCHAR+2,	DALL,		0,		L145},	/* 266 */
	{DPTR+DALL,	CHAR+2,		DALL,		0,		L145},	/* 267 */
	{DPTR+DALL,	0,		DALL,		0,		L146},	/* 268 */
/* <<=,	 >>=,	 unsigned >>= */
	{0},
/* cr75 */
	{DCHR,		0,		DCON,		0,		L147},	/* 270 */
	{DCHR,		0,		DALL,		0,		L148},	/* 271 */
	{DFIX,		UNCHAR+2,	DCON,		0,		L149},	/* 272 */
	{DFIX,		0,		DCON,		0,		L149},	/* 273 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L150},	/* 274 */
	{DFIX,		0,		DALL,		0,		L150},	/* 275 */
	{DFIX,		UNLONG+2,	DCON,		0,		L151},	/* 276 */
	{DFIX,		LONG+2,		DCON,		0,		L151},	/* 277 */
	{DFIX,		UNLONG+2,	DREG,		0,		L152},	/* 278 */
	{DFIX,		LONG+2,		DREG,		0,		L152},	/* 279 */
	{DFIX,		UNLONG+2,	DALL,		0,		L153},	/* 280 */
	{DFIX,		LONG+2,		DALL,		0,		L153},	/* 281 */
/* with a long shift ignore the high word */
	{DFIX,		UNLONG+2,	DALL,		LONG+2,		L154},	/* 282 */
	{DFIX,		LONG+2,		DALL,		LONG+2,		L154},	/* 283 */
/* =|,	 =&~ */
	{0},
/* cr78 */
	{DFIX,		1,		DFIX,		1,		L155},	/* 285 */


	{DFIX,		UNCHAR+2,	DFIX,		0,		L156},	/* 286 */
	{DFIX,		0,		DFIX,		1,		L157},	/* 287 */
	{DFIX,		DOUBLE+2,	DFIX,		DOUBLE+2,	L157},	/* 288 */


	{DFIX,		1,		DPTR+DALL,	1,		L158},	/* 289 */


	{DFIX,		1,		DALL,		0,		L159},	/* 290 */


	{DFIX,		UNCHAR+2,	DALL,		0,		L160},	/* 291 */
	{DPTR+DREG,	1,		DPTR+DALL,	1,		L161},	/* 292 */


	{DFIX,		DOUBLE+2,	DREG,		FLOAT+2,	L162},	/* 293 */


	{DFIX,		0,		DALL,		0,		L163},	/* 294 */
	{DFIX,		DOUBLE+2,	DALL,		FLOAT+2,	L163},	/* 295 */


	{DFIX,		FLOAT+2,	DALL,		FLOAT+2,	L164},	/* 296 */


	{DPTR+DREG,	1,		DALL,		0,		L165},	/* 297 */


	{DPTR+DALL,	1,		DALL,		0,		L166},	/* 298 */


	{DPTR+DALL,	0,		DALL,		0,		L167},	/* 299 */


	{DPTR+DALL,	UNCHAR+2,	DALL,		0,		L168},	/* 300 */
	{DPTR+DALL,	DOUBLE+2,	DALL,		FLOAT+2,	L169},	/* 301 */


	{DPTR+DALL,	FLOAT+2,	DALL,		FLOAT+2,	L170},	/* 302 */


	{DFIX,		LONG+2,		DCON,		0,		L171},	/* 303 */
	{DFIX,		UNLONG+2,	DCON,		0,		L171},	/* 304 */


	{DFIX,		LONG+2,		DFIX,		LONG+2,		L172},	/* 305 */
	{DFIX,		LONG+2,		DFIX,		UNLONG+2,	L172},	/* 306 */
	{DFIX,		UNLONG+2,	DFIX,		LONG+2,		L172},	/* 307 */
	{DFIX,		UNLONG+2,	DFIX,		UNLONG+2,	L172},	/* 308 */


	{DFIX,		LONG+2,		DALL,		LONG+2,		L173},	/* 309 */
	{DFIX,		LONG+2,		DALL,		UNLONG+2,	L173},	/* 310 */
	{DFIX,		UNLONG+2,	DALL,		LONG+2,		L173},	/* 311 */
	{DFIX,		UNLONG+2,	DALL,		UNLONG+2,	L173},	/* 312 */


	{DPTR+DALL,	LONG+2,		DCON,		0,		L174},	/* 313 */
	{DPTR+DALL,	UNLONG+2,	DCON,		0,		L174},	/* 314 */


	{DPTR+DALL,	LONG+2,		DFIX,		LONG+2,		L175},	/* 315 */
	{DPTR+DALL,	LONG+2,		DFIX,		UNLONG+2,	L175},	/* 316 */
	{DPTR+DALL,	UNLONG+2,	DFIX,		LONG+2,		L175},	/* 317 */
	{DPTR+DALL,	UNLONG+2,	DFIX,		UNLONG+2,	L175},	/* 318 */


	{DPTR+DALL,	LONG+2,		DALL,		LONG+2,		L176},	/* 319 */
	{DPTR+DALL,	LONG+2,		DALL,		UNLONG+2,	L176},	/* 320 */
	{DPTR+DALL,	UNLONG+2,	DALL,		LONG+2,		L176},	/* 321 */
	{DPTR+DALL,	UNLONG+2,	DALL,		UNLONG+2,	L176},	/* 322 */


/* int -> float */
	{0},
/* cr51 */
	{DFIX,		1,		DALL,		0,		L177},	/* 324 */
	{DPTR+DALL,	1,		DALL,		0,		L178},	/* 325 */
	{DALL,		0,		DALL,		0,		L179},	/* 326 */
/* float,	 double -> int */
	{0},
/* cr52 */
	{DALL,		FLOAT+2,	DALL,		0,		L180},	/* 328 */
/* double (float) to long */
	{0},
/* cr56 */
	{DALL,		FLOAT+2,	DALL,		0,		L181},	/* 330 */
/* long to double */
	{0},
/* cr57 */
	{DFIX,		LONG+2,		DALL,		0,		L182},	/* 332 */
	{DPTR+DALL,	LONG+2,		DALL,		0,		L183},	/* 333 */
	{DALL,		LONG+2,		DALL,		0,		L184},	/* 334 */
/* unsigned long to float(double) */
	{0},
/* cr127 */
	{DFIX,		UNLONG+2,	DALL,		0,		L185},	/* 336 */
	{DPTR+DALL,	UNLONG+2,	DALL,		0,		L186},	/* 337 */
	{DALL,		UNLONG+2,	DALL,		0,		L187},	/* 338 */
/* integer to long */
	{0},
/* cr58 */
	{DREG,		UNSIGN+2,	DALL,		0,		L188},	/* 340 */
	{DALL,		UNSIGN+2,	DALL,		0,		L189},	/* 341 */
	{DREG,		0,		DALL,		0,		L190},	/* 342 */
	{DALL,		0,		DALL,		0,		L191},	/* 343 */
/* long to integer */
	{0},
/* cr59 */
	{DFIX,		LONG+2,		DALL,		0,		L192},	/* 345 */
	{DFIX,		UNLONG+2,	DALL,		0,		L192},	/* 346 */
	{DPTR+DALL,	LONG+2,		DALL,		0,		L193},	/* 347 */
	{DPTR+DALL,	UNLONG+2,	DALL,		0,		L193},	/* 348 */
/* *,	 /,	 % for longs. */
	{0},
/* cr82 */
	{DALL,		LONG+2,		DALL,		LONG+2,		L194},	/* 350 */
	{DALL,		LONG+2,		DALL,		UNLONG+2,	L194},	/* 351 */
	{DALL,		UNLONG+2,	DALL,		LONG+2,		L194},	/* 352 */
	{DALL,		UNLONG+2,	DALL,		UNLONG+2,	L194},	/* 353 */
/* *,	 /,	 % for unsigned long */
	{0},
/* cr121 */
	{DALL,		UNLONG+2,	DALL,		LONG+2,		L195},	/* 355 */
	{DALL,		LONG+2,		DALL,		UNLONG+2,	L195},	/* 356 */
	{DALL,		UNLONG+2,	DALL,		UNLONG+2,	L195},	/* 357 */


/* *=,	 /=,	 %= for unsigned long */
	{0},
/* cr124 */
	{DALL,		0,		DALL,		LONG+2,		L196},	/* 359 */
	{DALL,		0,		DALL,		UNLONG+2,	L196},	/* 360 */
	{DALL,		LONG+2,		DALL,		0,		L196},	/* 361 */
	{DALL,		UNLONG+2,	DALL,		0,		L196},	/* 362 */


/* *=,	 /=,	 %= for longs */
/* Operands of the form &x op y,	 so stack space is known. */
	{0},
/* cr86 */
	{DALL,		0,		DALL,		LONG+2,		L197},	/* 364 */
	{DALL,		0,		DALL,		UNLONG+2,	L197},	/* 365 */
/* convert integer to character (sign extend) */
	{0},
/* cr109 */
	{DALL,		0,		DALL,		0,	L198},	/* 367 */
/* / and % where divisor is unsigned or known to be positive */
	{0},
/* cr117 */
	{DALL,		0,		DCON,		0,		L199},	/* 369 */
	{DALL,		0,		DFIX,		1,		L200},	/* 370 */
	{DALL,		0,		DPTR+DREG,	1,		L201},	/* 371 */
	{DALL,		0,		DREG,		0,		L202},	/* 372 */
	{DALL,		0,		DALL,		0,		L203},	/* 373 */

/* /= and %= where divisor is unsigned or known to be positive */
	{0},
/* cr119 */
	{DFIX,		UNCHAR+2,	DCON,		0,		L204},	/* 375 */
	{DFIX,		0,		DCON,		0,		L204},	/* 376 */
	{DFIX,		UNCHAR+2,	DFIX,		1,		L205},	/* 377 */
	{DFIX,		0,		DFIX,		1,		L205},	/* 378 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L206},	/* 379 */
	{DFIX,		0,		DALL,		0,		L206},	/* 380 */
	{DPTR+DREG,	UNCHAR+2,	DALL,		0,		L207},	/* 381 */
	{DPTR+DREG,	0,		DALL,		0,		L207},	/* 382 */
	{DPTR+DALL,	UNCHAR+2,	DALL,		0,		L208},	/* 383 */
	{DPTR+DALL,	0,		DALL,		0,		L208},	/* 384 */
/* (int *) - (int *) */
	{0},
/* cr107 */
	{DALL,		0,		DALL,		0,		L209},	/* 386 */
/* x - &name */
	{0},
/* cr130 */
	{DALL,		0,		DALL,		0,		L210},	/* 388 */
	{DALL,		LONG+2,		DALL,		0,		L211},	/* 389 */


/* = */
	{0},
/* ci80 */
	{DCHR,		0,		DZER,		0,		L212},	/* 391 */
	{DFIX,		CHAR+2,		DZER,		0,		L213},	/* 392 */
	{DFIX,		UNCHAR+2,	DZER,		0,		L213},	/* 393 */
	{DCHR,		0,		DCON,		0,		L214},	/* 394 */
	{DCHR,		0,		DFIX,		0,		L215},	/* 395 */
	{DCHR,		0,		DFIX,		UNCHAR+2,	L215},	/* 396 */
	{DCHR,		0,		DALL,		0,		L216},	/* 397 */
	{DFIX,		CHAR+2,		DCON,		0,		L217},	/* 398 */
	{DFIX,		UNCHAR+2,	DCON,		0,		L217},	/* 399 */
	{DFIX,		CHAR+2,		DFIX,		CHAR+2,		L218},	/* 400 */
	{DFIX,		CHAR+2,		DFIX,		UNCHAR+2,	L218},	/* 401 */
	{DFIX,		UNCHAR+2,	DFIX,		CHAR+2,		L218},	/* 402 */
	{DFIX,		UNCHAR+2,	DFIX,		UNCHAR+2,	L218},	/* 403 */
	{DFIX,		CHAR+2,		DALL,		0,		L219},	/* 404 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L219},	/* 405 */
	{DFIX,		1,		DZER,		0,		L220},	/* 406 */
	{DFIX,		DOUBLE+2,	DZER,		FLOAT+2,	L220},	/* 407 */
	{DPTR+DALL,	CHAR+2,		DZER,		0,		L221},	/* 408 */
	{DPTR+DALL,	UNCHAR+2,	DZER,		0,		L221},	/* 409 */
	{DPTR+DALL,	0,		DZER,		0,		L222},	/* 410 */
	{DPTR+DALL,	DOUBLE+2,	DZER,		FLOAT+2,	L222},	/* 411 */
	{DPTR+DALL,	UNCHAR+2,	DZER,		0,		L222},	/* 412 */
	{DCHR,		0,		DCON,		0,		L223},	/* 413 */
	{DFIX,		0,		DCON,		0,		L224},	/* 414 */
	{DFIX,		CHAR+2,		DCON,		0,		L225},	/* 415 */
	{DFIX,		UNCHAR+2,	DCON,		0,		L225},	/* 416 */
	{DFIX,		1,		DFIX,		1,		L226},	/* 417 */
	{DFIX,		0,		DPTR+DALL,	0,		L227},	/* 418 */
	{DFIX,		UNCHAR+2,	DPTR+DALL,	0,		L227},	/* 419 */
	{DFIX,		0,		DALL,		0,		L228},	/* 420 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L228},	/* 421 */
	{DPTR+DALL,	CHAR+2,		DCON,		0,		L229},	/* 422 */
	{DPTR+DALL,	UNCHAR+2,	DCON,		0,		L229},	/* 423 */
	{DPTR+DALL,	0,		DCON,		0,		L230},	/* 424 */
	{DPTR+DALL,	CHAR+2,		DFIX,		CHAR+2,		L231},	/* 425 */
	{DPTR+DALL,	UNCHAR+2,	DFIX,		CHAR+2,		L231},	/* 426 */
	{DPTR+DALL,	CHAR+2,		DFIX,		UNCHAR+2,	L231},	/* 427 */
	{DPTR+DALL,	UNCHAR+2,	DFIX,		UNCHAR+2,	L231},	/* 428 */
	{DPTR+DALL,	CHAR+2,		DFIX,		0,		L232},	/* 429 */
	{DPTR+DALL,	UNCHAR+2,	DFIX,		0,		L232},	/* 430 */
	{DPTR+DALL,	0,		DFIX,		0,		L233},	/* 431 */
	{DPTR+DALL,	0,		DPTR+DREG,	1,		L234},	/* 432 */
	{DPTR+DALL,	CHAR+2,		DPTR+DREG,	0,		L234},	/* 433 */
	{DPTR+DALL,	UNCHAR+2,	DPTR+DREG,	0,		L234},	/* 434 */
	{DPTR+DALL,	UNCHAR+2,	DREG,		0,		L235},	/* 435 */
	{DPTR+DALL,	0,		DREG,		0,		L235},	/* 436 */
	{DPTR+DREG,	0,		DPTR+DALL,	1,		L236},	/* 437 */
	{DPTR+DREG,	CHAR+2,		DPTR+DALL,	0,		L236},	/* 438 */
	{DPTR+DREG,	UNCHAR+2,	DPTR+DALL,	0,		L236},	/* 439 */
	{DPTR+DREG,	0,		DALL,		0,		L237},	/* 440 */
	{DPTR+DREG,	UNCHAR+2,	DALL,		0,		L237},	/* 441 */
	{DPTR+DALL,	0,		DPTR+DALL,	1,		L238},	/* 442 */
	{DPTR+DALL,	CHAR+2,		DPTR+DALL,	0,		L238},	/* 443 */
	{DPTR+DALL,	UNCHAR+2,	DPTR+DALL,	0,		L238},	/* 444 */
	{DPTR+DALL,	0,		DALL,		0,		L239},	/* 445 */
	{DPTR+DALL,	UNCHAR+2,	DALL,		0,		L239},	/* 446 */
	{DFIX,		1,		DALL,		FLOAT+2,	L240},	/* 447 */
	{DPTR+DREG,	1,		DALL,		FLOAT+2,	L241},	/* 448 */
	{DFIX,		LONG+2,		DZER,		0,		L242},	/* 449 */
	{DFIX,		UNLONG+2,	DZER,		0,		L242},	/* 450 */
	{DPTR+DALL,	LONG+2,		DZER,		0,		L243},	/* 451 */
	{DPTR+DALL,	UNLONG+2,	DZER,		0,		L243},	/* 452 */

	{DFIX,		LONG+2,		DCON,		1,		L244},	/* 453 */
	{DFIX,		UNLONG+2,	DCON,		1,		L244},	/* 454 */
	{DFIX,		LONG+2,		DFIX,		1,		L245},	/* 455 */
	{DFIX,		UNLONG+2,	DFIX,		1,		L245},	/* 456 */
	{DFIX,		LONG+2,		DPTR+DALL,	1,		L246},	/* 457 */
	{DFIX,		UNLONG+2,	DPTR+DALL,	1,		L246},	/* 458 */
	{DFIX,		LONG+2,		DALL,		0,		L247},	/* 459 */
	{DFIX,		UNLONG+2,	DALL,		0,		L247},	/* 460 */
	{DFIX,		LONG+2,		DALL,		FLOAT+2,	L248},	/* 461 */
	{DFIX,		UNLONG+2,	DALL,		FLOAT+2,	L248},	/* 462 */
	{DPTR+DREG,	LONG+2,		DALL,		FLOAT+2,	L249},	/* 463 */
	{DPTR+DREG,	UNLONG+2,	DALL,		FLOAT+2,	L249},	/* 464 */
	{DFIX,		LONG+2,		DCON,		LONG+2,		L250},	/* 465 */
	{DFIX,		UNLONG+2,	DCON,		LONG+2,		L250},	/* 466 */
	{DFIX,		LONG+2,		DFIX,		LONG+2,		L251},	/* 467 */
	{DFIX,		LONG+2,		DFIX,		UNLONG+2,	L251},	/* 468 */
	{DFIX,		UNLONG+2,	DFIX,		LONG+2,		L251},	/* 469 */
	{DFIX,		UNLONG+2,	DFIX,		UNLONG+2,	L251},	/* 470 */
	{DFIX,		LONG+2,		DPTR+DALL,	LONG+2,		L252},	/* 471 */
	{DFIX,		LONG+2,		DPTR+DALL,	UNLONG+2,	L252},	/* 472 */
	{DFIX,		UNLONG+2,	DPTR+DALL,	LONG+2,		L252},	/* 473 */
	{DFIX,		UNLONG+2,	DPTR+DALL,	UNLONG+2,	L252},	/* 474 */
	{DFIX,		LONG+2,		DALL,		LONG+2,		L253},	/* 475 */
	{DFIX,		LONG+2,		DALL,		UNLONG+2,	L253},	/* 476 */
	{DFIX,		UNLONG+2,	DALL,		LONG+2,		L253},	/* 477 */
	{DFIX,		UNLONG+2,	DALL,		UNLONG+2,	L253},	/* 478 */
	{DPTR+DALL,	LONG+2,		DFIX,		1,		L254},	/* 479 */
	{DPTR+DALL,	UNLONG+2,	DFIX,		1,		L254},	/* 480 */
	{DPTR+DALL,	LONG+2,		DFIX,		LONG+2,		L255},	/* 481 */
	{DPTR+DALL,	LONG+2,		DFIX,		UNLONG+2,	L255},	/* 482 */
	{DPTR+DALL,	UNLONG+2,	DFIX,		LONG+2,		L255},	/* 483 */
	{DPTR+DALL,	UNLONG+2,	DFIX,		UNLONG+2,	L255},	/* 484 */
	{DPTR+DREG,	LONG+2,		DALL,		LONG+2,		L256},	/* 485 */
	{DPTR+DREG,	LONG+2,		DALL,		UNLONG+2,	L256},	/* 486 */
	{DPTR+DREG,	UNLONG+2,	DALL,		LONG+2,		L256},	/* 487 */
	{DPTR+DREG,	UNLONG+2,	DALL,		UNLONG+2,	L256},	/* 488 */
	{DPTR+DALL,	LONG+2,		DALL,		0,		L257},	/* 489 */
	{DPTR+DALL,	UNLONG+2,	DALL,		0,		L257},	/* 490 */
	{DPTR+DALL,	LONG+2,		DALL,		LONG+2,		L258},	/* 491 */
	{DPTR+DALL,	LONG+2,		DALL,		UNLONG+2,	L258},	/* 492 */
	{DPTR+DALL,	UNLONG+2,	DALL,		LONG+2,		L258},	/* 493 */
	{DPTR+DALL,	UNLONG+2,	DALL,		UNLONG+2,	L258},	/* 494 */
/* |= and &~= */
	{0},
/* ci78 */
	{DCHR,		0,		DCON,		0,		L259},	/* 496 */
	{DFIX,		0,		DCON,		0,		L260},	/* 497 */
	{DFIX,		UNCHAR+2,	DCON,		0,		L260},	/* 498 */
	{DFIX,		1,		DFIX,		1,		L261},	/* 499 */


	{DFIX,		UNCHAR+2,	DALL,		0,		L262},	/* 500 */
	{DFIX,		0,		DALL,		0,		L263},	/* 501 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L263},	/* 502 */


	{DPTR+DALL,	CHAR+2,		DCON,		0,		L264},	/* 503 */
	{DPTR+DALL,	UNCHAR+2,	DCON,		0,		L264},	/* 504 */
	{DPTR+DALL,	0,		DCON,		0,		L265},	/* 505 */
	{DPTR+DALL,	0,		DFIX,		1,		L266},	/* 506 */
	{DPTR+DALL,	CHAR+2,		DFIX,		0,		L266},	/* 507 */
	{DPTR+DALL,	UNCHAR+2,	DFIX,		0,		L266},	/* 508 */


	{DPTR+DALL,	0,		DPTR+DREG,	1,		L267},	/* 509 */
	{DPTR+DALL,	CHAR+2,		DPTR+DREG,	0,		L267},	/* 510 */
	{DPTR+DALL,	UNCHAR+2,	DPTR+DREG,	0,		L267},	/* 511 */


	{DPTR+DALL,	0,		DREG,		0,		L268},	/* 512 */


	{DPTR+DREG,	0,		DPTR+DALL,	1,		L269},	/* 513 */
	{DPTR+DREG,	CHAR+2,		DPTR+DALL,	0,		L269},	/* 514 */
	{DPTR+DREG,	UNCHAR+2,	DPTR+DALL,	0,		L269},	/* 515 */


	{DPTR+DREG,	0,		DALL,		0,		L270},	/* 516 */


	{DPTR+DALL,	0,		DPTR+DALL,	1,		L271},	/* 517 */
	{DPTR+DALL,	CHAR+2,		DPTR+DALL,	0,		L271},	/* 518 */
	{DPTR+DALL,	UNCHAR+2,	DPTR+DALL,	0,		L271},	/* 519 */


	{DPTR+DALL,	0,		DALL,		0,		L272},	/* 520 */


	{DFIX,		LONG+2,		DCON,		0,		L273},	/* 521 */
	{DFIX,		LONG+2,		DFIX,		UNSIGN+2,	L273},	/* 522 */
	{DFIX,		UNLONG+2,	DCON,		0,		L273},	/* 523 */
	{DFIX,		UNLONG+2,	DFIX,		UNSIGN+2,	L273},	/* 524 */


	{DFIX,		LONG+2,		DFIX,		LONG+2,		L274},	/* 525 */
	{DFIX,		LONG+2,		DFIX,		UNLONG+2,	L274},	/* 526 */
	{DFIX,		UNLONG+2,	DFIX,		LONG+2,		L274},	/* 527 */
	{DFIX,		UNLONG+2,	DFIX,		UNLONG+2,	L274},	/* 528 */


	{DFIX,		LONG+2,		DPTR+DALL,	LONG+2,		L275},	/* 529 */
	{DFIX,		LONG+2,		DPTR+DALL,	UNLONG+2,	L275},	/* 530 */
	{DFIX,		UNLONG+2,	DPTR+DALL,	LONG+2,		L275},	/* 531 */
	{DFIX,		UNLONG+2,	DPTR+DALL,	UNLONG+2,	L275},	/* 532 */


	{DFIX,		LONG+2,		DALL,		LONG+2,		L276},	/* 533 */
	{DFIX,		LONG+2,		DALL,		UNLONG+2,	L276},	/* 534 */
	{DFIX,		UNLONG+2,	DALL,		LONG+2,		L276},	/* 535 */
	{DFIX,		UNLONG+2,	DALL,		UNLONG+2,	L276},	/* 536 */


	{DPTR+DALL,	LONG+2,		DCON,		0,		L277},	/* 537 */
	{DPTR+DALL,	UNLONG+2,	DCON,		0,		L277},	/* 538 */


	{DPTR+DALL,	LONG+2,		DFIX,		LONG+2,		L278},	/* 539 */
	{DPTR+DALL,	LONG+2,		DFIX,		UNLONG+2,	L278},	/* 540 */
	{DPTR+DALL,	UNLONG+2,	DFIX,		LONG+2,		L278},	/* 541 */
	{DPTR+DALL,	UNLONG+2,	DFIX,		UNLONG+2,	L278},	/* 542 */


	{DPTR+DREG,	LONG+2,		DALL,		LONG+2,		L279},	/* 543 */
	{DPTR+DREG,	LONG+2,		DALL,		UNLONG+2,	L279},	/* 544 */
	{DPTR+DREG,	UNLONG+2,	DALL,		LONG+2,		L279},	/* 545 */
	{DPTR+DREG,	UNLONG+2,	DALL,		UNLONG+2,	L279},	/* 546 */


	{DPTR+DALL,	LONG+2,		DALL,		LONG+2,		L280},	/* 547 */
	{DPTR+DALL,	LONG+2,		DALL,		UNLONG+2,	L280},	/* 548 */
	{DPTR+DALL,	UNLONG+2,	DALL,		LONG+2,		L280},	/* 549 */
	{DPTR+DALL,	UNLONG+2,	DALL,		UNLONG+2,	L280},	/* 550 */


/* ^= */
	{0},
/* ci79 */
	{DFIX,		LONG+2,		DALL,		LONG+2,		L281},	/* 552 */
	{DFIX,		LONG+2,		DALL,		UNLONG+2,	L281},	/* 553 */
	{DFIX,		UNLONG+2,	DALL,		LONG+2,		L281},	/* 554 */
	{DFIX,		UNLONG+2,	DALL,		UNLONG+2,	L281},	/* 555 */
	{DPTR+DREG,	LONG+2,		DALL,		LONG+2,		L282},	/* 556 */
	{DPTR+DREG,	LONG+2,		DALL,		UNLONG+2,	L282},	/* 557 */
	{DPTR+DREG,	UNLONG+2,	DALL,		LONG+2,		L282},	/* 558 */
	{DPTR+DREG,	UNLONG+2,	DALL,		UNLONG+2,	L282},	/* 559 */
	{DPTR+DALL,	LONG+2,		DALL,		LONG+2,		L283},	/* 560 */
	{DPTR+DALL,	LONG+2,		DALL,		UNLONG+2,	L283},	/* 561 */
	{DPTR+DALL,	UNLONG+2,	DALL,		LONG+2,		L283},	/* 562 */
	{DPTR+DALL,	UNLONG+2,	DALL,		UNLONG+2,	L283},	/* 563 */
/* +=,	 -=,	 ++,	 -- */
	{0},
/* ci70 */
	{DFIX,		1,		DONE,		0,		L284},	/* 565 */
	{DFIX,		1,		DTWO,		0,		L285},	/* 566 */
	{DCHR,		0,		DCON,		0,		L286},	/* 567 */
	{DFIX,		CHAR+2,		DCON,		0,		L287},	/* 568 */
	{DFIX,		UNCHAR+2,	DCON,		0,		L287},	/* 569 */
	{DFIX,		0,		DCON,		0,		L288},	/* 570 */
	{DFIX,		1,		DFIX,		1,		L289},	/* 571 */


	{DFIX,		0,		DPTR+DALL,	0,		L290},	/* 572 */
	{DFIX,		UNCHAR+2,	DPTR+DALL,	0,		L290},	/* 573 */


	{DFIX,		0,		DALL,		0,		L291},	/* 574 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L291},	/* 575 */


	{DPTR+DALL,	1,		DONE,		0,		L292},	/* 576 */
	{DPTR+DREG,	1,		DPTR+DALL,	1,		L293},	/* 577 */


	{DFIX,		0,		DPTR+DREG,	1,		L294},	/* 578 */
	{DFIX,		UNCHAR+2,	DPTR+DREG,	1,		L294},	/* 579 */
	{DFIX,		0,		DALL,		0,		L295},	/* 580 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L295},	/* 581 */
	{DPTR+DREG,	1,		DALL,		0,		L296},	/* 582 */


	{DPTR+DALL,	1,		DALL,		0,		L297},	/* 583 */


	{DPTR+DALL,	0,		DALL,		0,		L298},	/* 584 */
	{DFIX,		LONG+2,		DCON,		0,		L299},	/* 585 */
	{DFIX,		UNLONG+2,	DCON,		0,		L299},	/* 586 */
	{DFIX,		LONG+2,		DCON,		LONG+2,		L300},	/* 587 */
	{DFIX,		UNLONG+2,	DCON,		LONG+2,		L300},	/* 588 */
	{DFIX,		LONG+2,		DFIX,		UNSIGN+2,	L301},	/* 589 */
	{DFIX,		UNLONG+2,	DFIX,		UNSIGN+2,	L301},	/* 590 */


	{DFIX,		LONG+2,		DFIX,		LONG+2,		L302},	/* 591 */
	{DFIX,		LONG+2,		DFIX,		UNLONG+2,	L302},	/* 592 */
	{DFIX,		UNLONG+2,	DFIX,		LONG+2,		L302},	/* 593 */
	{DFIX,		UNLONG+2,	DFIX,		UNLONG+2,	L302},	/* 594 */


	{DFIX,		LONG+2,		DPTR+DALL,	LONG+2,		L303},	/* 595 */
	{DFIX,		LONG+2,		DPTR+DALL,	UNLONG+2,	L303},	/* 596 */
	{DFIX,		UNLONG+2,	DPTR+DALL,	LONG+2,		L303},	/* 597 */
	{DFIX,		UNLONG+2,	DPTR+DALL,	UNLONG+2,	L303},	/* 598 */


	{DFIX,		LONG+2,		DALL,		LONG+2,		L304},	/* 599 */
	{DFIX,		LONG+2,		DALL,		UNLONG+2,	L304},	/* 600 */
	{DFIX,		UNLONG+2,	DALL,		LONG+2,		L304},	/* 601 */
	{DFIX,		UNLONG+2,	DALL,		UNLONG+2,	L304},	/* 602 */


	{DPTR+DALL,	LONG+2,		DCON,		0,		L305},	/* 603 */
	{DPTR+DALL,	LONG+2,		DFIX,		UNSIGN+2,	L305},	/* 604 */
	{DPTR+DALL,	UNLONG+2,	DCON,		0,		L305},	/* 605 */
	{DPTR+DALL,	UNLONG+2,	DFIX,		UNSIGN+2,	L305},	/* 606 */


	{DPTR+DALL,	LONG+2,		DFIX,		LONG+2,		L306},	/* 607 */
	{DPTR+DALL,	LONG+2,		DFIX,		UNLONG+2,	L306},	/* 608 */
	{DPTR+DALL,	UNLONG+2,	DFIX,		LONG+2,		L306},	/* 609 */
	{DPTR+DALL,	UNLONG+2,	DFIX,		UNLONG+2,	L306},	/* 610 */


	{DPTR+DREG,	LONG+2,		DALL,		LONG+2,		L307},	/* 611 */
	{DPTR+DREG,	LONG+2,		DALL,		UNLONG+2,	L307},	/* 612 */
	{DPTR+DREG,	UNLONG+2,	DALL,		LONG+2,		L307},	/* 613 */
	{DPTR+DREG,	UNLONG+2,	DALL,		UNLONG+2,	L307},	/* 614 */


	{DPTR+DALL,	LONG+2,		DALL,		LONG+2,		L308},	/* 615 */
	{DPTR+DALL,	LONG+2,		DALL,		UNLONG+2,	L308},	/* 616 */
	{DPTR+DALL,	UNLONG+2,	DALL,		LONG+2,		L308},	/* 617 */
	{DPTR+DALL,	UNLONG+2,	DALL,		UNLONG+2,	L308},	/* 618 */


/* field = ... */
	{0},
/* ci16 */
	{DFIX,		CHAR+2,		DCON,		0,		L309},	/* 620 */
	{DFIX,		0,		DCON,		0,		L310},	/* 621 */
	{DFIX,		0,		DFIX,		0,		L311},	/* 622 */
	{DFIX,		0,		DALL,		0,		L312},	/* 623 */
	{DPTR+DALL,	0,		DFIX,		0,		L313},	/* 624 */
	{DPTR+DREG,	0,		DALL,		0,		L314},	/* 625 */
	{DPTR+DALL,	0,		DREG,		0,		L315},	/* 626 */
	{DPTR+DALL,	0,		DALL,		0,		L316},	/* 627 */


/* relationals */
	{0},
/* cc60 */
	{DCON,		0,		DZER,		0,		L317},	/* 629 */
	{DFIX,		0,		DZER,		0,		L318},	/* 630 */
	{DFIX,		DOUBLE+2,	DZER,		FLOAT+2,	L318},	/* 631 */
	{DFIX,		UNCHAR+2,	DZER,		0,		L318},	/* 632 */
	{DFIX,		FLOAT+2,	DZER,		0,		L319},	/* 633 */
	{DPTR+DALL,	0,		DZER,		0,		L320},	/* 634 */
	{DPTR+DALL,	DOUBLE+2,	DZER,		FLOAT+2,	L320},	/* 635 */
	{DPTR+DALL,	UNCHAR+2,	DZER,		0,		L320},	/* 636 */
	{DPTR+DALL,	FLOAT+2,	DZER,		0,		L321},	/* 637 */
	{DALL,		0,		DZER,		0,		L322},	/* 638 */
	{DALL,		FLOAT+2,	DZER,		FLOAT+2,	L322},	/* 639 */
	{DCHR,		0,		DCON,		0,		L323},	/* 640 */
	{DALL,		0,		DCON,		0,		L324},	/* 641 */
	{DFIX,		1,		DFIX,		1,		L325},	/* 642 */
	{DFIX,		CHAR+2,		DFIX,		CHAR+2,		L325},	/* 643 */
	{DFIX,		UNCHAR+2,	DFIX,		UNCHAR+2,	L325},	/* 644 */
	{DPTR+DALL,	1,		DFIX,		1,		L326},	/* 645 */
	{DPTR+DALL,	CHAR+2,		DFIX,		CHAR+2,		L326},	/* 646 */
	{DPTR+DALL,	UNCHAR+2,	DFIX,		UNCHAR+2,	L326},	/* 647 */
	{DALL,		0,		DFIX,		1,		L327},	/* 648 */
	{DALL,		FLOAT+2,	DFIX,		DOUBLE+2,	L327},	/* 649 */
	{DPTR+DALL,	1,		DPTR+DREG,	1,		L328},	/* 650 */
	{DPTR+DALL,	CHAR+2,		DPTR+DREG,	CHAR+2,		L328},	/* 651 */
	{DPTR+DALL,	UNCHAR+2,	DPTR+DREG,	UNCHAR+2,	L328},	/* 652 */
	{DPTR+DALL,	1,		DREG,		0,		L329},	/* 653 */
	{DALL,		0,		DPTR+DREG,	1,		L330},	/* 654 */
	{DALL,		FLOAT+2,	DPTR+DREG,	DOUBLE+2,	L330},	/* 655 */
	{DALL,		0,		DREG,		0,		L331},	/* 656 */
	{DALL,		FLOAT+2,	DREG,		FLOAT+2,	L331},	/* 657 */
	{DALL,		0,		DALL,		0,		L332},	/* 658 */
	{DALL,		FLOAT+2,	DALL,		FLOAT+2,	L332},	/* 659 */
	{DFIX,		LONG+2,		DZER,		0,		L333},	/* 660 */
	{DFIX,		UNLONG+2,	DZER,		0,		L333},	/* 661 */
	{DFIX,		LONG+2,		DCON,		0,		L334},	/* 662 */
	{DFIX,		UNLONG+2,	DCON,		0,		L334},	/* 663 */
	{DFIX,		LONG+2,		DCON,		LONG+2,		L335},	/* 664 */
	{DFIX,		UNLONG+2,	DCON,		LONG+2,		L335},	/* 665 */

	{DCON,		LONG+2,		DFIX,		LONG+2,		L336},	/* 666 */
	{DCON,		LONG+2,		DFIX,		UNLONG+2,	L336},	/* 667 */
	{DFIX,		LONG+2,		DFIX,		UNSIGN+2,	L337},	/* 668 */
	{DFIX,		UNLONG+2,	DFIX,		UNSIGN+2,	L337},	/* 669 */
	{DFIX,		LONG+2,		DFIX,		LONG+2,		L338},	/* 670 */
	{DFIX,		LONG+2,		DFIX,		UNLONG+2,	L338},	/* 671 */
	{DFIX,		UNLONG+2,	DFIX,		LONG+2,		L338},	/* 672 */
	{DFIX,		UNLONG+2,	DFIX,		UNLONG+2,	L338},	/* 673 */
	{DPTR+DALL,	LONG+2,		DZER,		0,		L339},	/* 674 */
	{DPTR+DALL,	UNLONG+2,	DZER,		0,		L339},	/* 675 */
	{DPTR+DALL,	LONG+2,		DCON,		0,		L340},	/* 676 */
	{DPTR+DALL,	UNLONG+2,	DCON,		0,		L340},	/* 677 */
	{DPTR+DALL,	LONG+2,		DFIX,		UNSIGN+2,	L341},	/* 678 */
	{DPTR+DALL,	UNLONG+2,	DFIX,		UNSIGN+2,	L341},	/* 679 */
	{DPTR+DALL,	LONG+2,		DFIX,		LONG+2,		L342},	/* 680 */
	{DPTR+DALL,	LONG+2,		DFIX,		UNLONG+2,	L342},	/* 681 */
	{DPTR+DALL,	UNLONG+2,	DFIX,		LONG+2,		L342},	/* 682 */
	{DPTR+DALL,	LONG+2,		DFIX,		UNLONG+2,	L342},	/* 683 */
	{DALL,		LONG+2,		DZER,		0,		L343},	/* 684 */
	{DALL,		UNLONG+2,	DZER,		0,		L343},	/* 685 */
	{DALL,		LONG+2,		DCON,		0,		L344},	/* 686 */
	{DALL,		UNLONG+2,	DCON,		0,		L344},	/* 687 */
	{DALL,		LONG+2,		DCON,		LONG+2,		L345},	/* 688 */
	{DALL,		UNLONG+2,	DCON,		LONG+2,		L345},	/* 689 */
	{DALL,		LONG+2,		DFIX,		UNSIGN+2,	L346},	/* 690 */
	{DALL,		UNLONG+2,	DFIX,		UNSIGN+2,	L346},	/* 691 */
	// src is reg,	 dest is adr,	 check flags
	{DALL,		LONG+2,		DFIX,		LONG+2,		L347},	/* 692 */
	{DALL,		LONG+2,		DFIX,		UNLONG+2,	L347},	/* 693 */
	{DALL,		UNLONG+2,	DFIX,		LONG+2,		L347},	/* 694 */
	{DALL,		UNLONG+2,	DFIX,		UNLONG+2,	L347},	/* 695 */
	// src is ptr in reg,	 dest is ptr in reg1,	 check flags
	{DPTR+DALL,	LONG+2,		DPTR+DREG,	LONG+2,		L348},	/* 696 */
	{DPTR+DALL,	LONG+2,		DPTR+DREG,	UNLONG+2,	L348},	/* 697 */
	{DPTR+DALL,	UNLONG+2,	DPTR+DREG,	LONG+2,		L348},	/* 698 */
	{DPTR+DALL,	UNLONG+2,	DPTR+DREG,	UNLONG+2,	L348},	/* 699 */
	// src is reg,	 dest is ptr in reg1,	 check flags
	{DALL,		LONG+2,		DPTR+DREG,	LONG+2,		L349},	/* 700 */
	{DALL,		LONG+2,		DPTR+DREG,	UNLONG+2,	L349},	/* 701 */
	{DALL,		UNLONG+2,	DPTR+DREG,	LONG+2,		L349},	/* 702 */
	{DALL,		UNLONG+2,	DPTR+DREG,	UNLONG+2,	L349},	/* 703 */
	{DALL,		LONG+2,		DALL,		LONG+2,		L350},	/* 704 */
	{DALL,		LONG+2,		DALL,		UNLONG+2,	L350},	/* 705 */
	{DALL,		UNLONG+2,	DALL,		LONG+2,		L350},	/* 706 */
	{DALL,		UNLONG+2,	DALL,		UNLONG+2,	L350},	/* 707 */
/* & as in "if ((a&b) ==0)" */
	{0},
/* cc81 */
	{DFIX,		CHAR+2,		DCON,		0,		L351},	/* 709 */
	{DFIX,		UNCHAR+2,	DCON,		0,		L351},	/* 710 */
	{DALL,		0,		DCON,		0,		L352},	/* 711 */
	{DFIX,		0,		DREG,		0,		L353},	/* 712 */
	{DFIX,		UNCHAR+2,	DREG,		0,		L353},	/* 713 */
	{DPTR+DALL,	0,		DFIX,		0,		L354},	/* 714 */
	{DPTR+DALL,	UNCHAR+2,	DFIX,		0,		L354},	/* 715 */


	{DALL,		0,		DFIX,		1,		L355},	/* 716 */


	{DALL,		0,		DREG,		0,		L356},	/* 717 */


	{DALL,		0,		DALL,		0,		L357},	/* 718 */


/* set codes right by moving the result */
	{0},
/* rest */
	{DALL,		0,		DALL,		0,		L358},	/* 720 */
	{DALL,		FLOAT+2,	DALL,		FLOAT+2,	L358},	/* 721 */


/* load */
	{0},
/* cs106 */
	{DZER,		0,		DALL,		0,		L359},	/* 723 */
	{DZER,		FLOAT+2,	DALL,		0,		L359},	/* 724 */
	{DCON,		0,		DALL,		0,		L360},	/* 725 */
	{DFIX,		CHAR+2,		DALL,		0,		L361},	/* 726 */
	{DFIX,		UNCHAR+2,	DALL,		0,		L361},	/* 727 */
	{DFIX,		1,		DALL,		0,		L362},	/* 728 */
	{DPTR+DALL,	1,		DALL,		0,		L363},	/* 729 */
	{DCON,		LONG+2,		DALL,		0,		L364},	/* 730 */
	{DFIX,		LONG+2,		DALL,		0,		L365},	/* 731 */
	{DFIX,		UNLONG+2,	DALL,		0,		L365},	/* 732 */
/* +1,	 +2,	 -1,	 -2 */
	{0},
/* cs91 */
	{DALL,		0,		DONE,		0,		L366},	/* 734 */
	{DALL,		0,		DTWO,		0,		L367},	/* 735 */
/* +,	 -,	 |,	 &~ */
	{0},
/* cs40 */
	{DFIX,		0,		DCON,		0,		L368},	/* 737 */
	{DFIX,		UNCHAR+2,	DCON,		0,		L368},	/* 738 */
	{DALL,		0,		DCON,		0,		L369},	/* 739 */
	{DALL,		UNCHAR+2,	DCON,		0,		L369},	/* 740 */
	{DFIX,		0,		DFIX,		1,		L370},	/* 741 */
	{DFIX,		0,		DPTR+DALL,	1,		L371},	/* 742 */
	{DFIX,		0,		DALL,		0,		L372},	/* 743 */
/* integer to long */
	{0},
/* cs58 */
	{DCON,		0,		DALL,		0,		L373},	/* 745 */
	{DALL,		UNSIGN+2,	DALL,		0,		L374},	/* 746 */
	{DFIX,		1,		DALL,		0,		L375},	/* 747 */
/* float to long */
	{0},
/* cs56 */
	{DALL,		FLOAT+2,	DALL,		0,		L376},	/* 749 */
/* setup for structure assign */
	{0},
/* ci116 */
	{DALL,		0,		DREG,		0,		L377},	/* 751 */
	{DALL,		0,		DALL,		0,		L378},	/* 752 */
/* end of table */
	{0},
};

/*
 * c code tables-- compile to register
 */

struct table regtab[] = {
	{106,cr106},	/* load */
	{30,cr70},	/* prefix ++, handled as += */
	{31,cr70},	/* prefix --, handled as -= */
	{32,cr32},	/* postfix ++ */
	{33,cr32},	/* postfix -- */
	{37,cr37},	/* unary - */
	{38,cr37},	/* ~ */
	{98,cr100},	/* call */
	{99,cr100},	/* call */
	{80,cr80},	/* = */
	{91,cr91},	/* +1, +2 */
	{92,cr91},	/* -1, -2 */
	{40,cr40},	/* + */
	{41,cr40},	/* - */
	{42,cr42},	/* *, char/int, both signed & unsigned */
	{43,cr43},	/* /, signed char/int, calls @idiv */
	{14,cr14},	/* PTOI: scale pointer difference to int */
	{44,cr43},	/* %, signed char/int, calls @irem */
	{45,cr45},	/* >>, signed */
	{17,cr45},	/* >>, unsigned */
	{46,cr45},	/* << */
	{55,cr40},	/* &~ */
	{48,cr40},	/* | */
	{49,cr49},	/* ^ */
	{70,cr70},	/* += */
	{71,cr70},	/* -= */
	{72,cr72},	/* *= */
	{73,cr73},	/* /=, signed char/int, calls @idiv */
	{74,cr73},	/* %=, signed char/int, calls @irem */
	{75,cr75},	/* >>=, signed */
	{18,cr75},	/* >>=, unsigned */
	{76,cr75},	/* <<= */
	{78,cr78},	/* |= */
	{85,cr78},	/* &= */
	{79,cr79},	/* ^= */
	{102,cr102},	/* goto */
	{51,cr51},	/* cvt int => float */
	{52,cr52},	/* cvt float => int */
	{56,cr56},	/* cvt long => float */
	{57,cr57},	/* cvt float => long */
	{58,cr58},	/* cvt int => long */
	{59,cr59},	/* cvt long => int */
	{82,cr82},	/* *, signed long, calls @lmul */
	{83,cr82},	/* /, signed long, calls @ldiv */
	{84,cr82},	/* %, signed long, calls @lrem */
	{86,cr86},	/* *=, signed long, calls @almul */
	{87,cr86},	/* /=, signed long, calls @aldiv */
	{88,cr86},	/* %=, signed long, calls @alrem */
	{16,cr16},	/* = for bit fields */
	{109,cr109},	/* cvt int -> char */
	{117,cr117},	/* /  for unsigned char/int or divisor known positive */
	{118,cr117},	/* %  for unsigned char/int or divisor known positive */
	{119,cr119},	/* /= for unsigned char/int or divisor known positive */
	{120,cr119},	/* %= for unsigned char/int or divisor known positive */
	{107,cr107},	/* special case (int *) - (int *) */
	{121,cr121},	/* *, unsigned long, calls @lmul */
	{122,cr121},	/* /, unsigned long, calls @uldiv */
	{123,cr121},	/* %, unsigned long, calls @ulrem */
	{124,cr124},	/* *=, unsigned long, calls @almul */
	{125,cr124},	/* /=, unsigned long, calls @ualdiv */
	{126,cr124},	/* %=, unsigned long, calls @ualrem */
	{127,cr127},	/* cvt unsigned long to float, calls @ultof */
	{130,cr130},	/* special handling of 'x - &name' */
	{0}
};

/*
 * c code tables -- compile for side effects.
 * Also set condition codes properly (except for ++, --)
 */

struct table efftab[] = {
	{30,ci70},	/* prefix ++ */
	{31,ci70},	/* prefix -- */
	{32,ci70},	/* postfix ++ */
	{33,ci70},	/* postfix -- */
	{80,ci80},	/* = */
	{70,ci70},	/* += */
	{71,ci70},	/* -= */
	{78,ci78},	/* |= */
	{79,ci79},	/* ^= */
	{85,ci78},	/* &~= */
	{16,ci16},	/* field assign, FSELA */
	{116,ci116},	/* structure assignment setup */
	{0}
};

/*
 * c code tables-- set condition codes
 */

struct table cctab[] = {
	{106,cc60},	/* load */
	{55,rest},	/* &~ */
	{34,rest},	/* ! */
	{35,rest},	/* & */
	{36,rest},	/* * */
	{37,rest},	/* unary - */
	{40,rest},	/* + */
	{41,rest},	/* - */
	{43,rest},	/* / */
	{81,cc81},	/* & as in "if ((a&b)==0)" */
	{48,rest},	/* | */
	{60,cc60},	/* == */
	{61,cc60},	/* =! */
	{62,cc60},	/* <=, signed */
	{63,cc60},	/* <, signed */
	{64,cc60},	/* >=, signed */
	{65,cc60},	/* >, signed */
	{66,cc60},	/* <=, unsigned */
	{67,cc60},	/* <, unsigned */
	{68,cc60},	/* >=, unsigned */
	{69,cc60},	/* > , unsigned */
	{72,rest},	/* *= */
	{73,rest},	/* /= */
	{79,rest},	/* ^= */
	{0}
};

/*
 * c code tables-- expression to -(sp)
 */

struct table sptab[] = {
	{106,cs106},		/* load */
	{91,cs91},		/* +1, +2 */
	{92,cs91},		/* -1, -2 */
	{40,cs40},		/* + */
	{41,cs40},		/* - */
	{55,cs40},		/* &~ */
	{48,cs40},		/* | */
	{58,cs58},		/* cvt int->long */
	{56,cs56},		/* cvt float->long */
	{0}
};
