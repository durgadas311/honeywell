#include "c1.h"
#define cr102 &optab[0]

static char L1[]="b\301\n";
static char L2[]="GBb\243(I)\n";
#define cr100 &optab[3]

static char L3[]="bl\301\n";
static char L4[]="GBbl\243(I)\n";
static char L5[]="GAbl\250I)\n";
#define cr106 &optab[7]

static char L6[]="clr\311\n";
static char L7[]="clrf\311\n";
static char L8[]="li\311,A\n";
static char L9[]="movC\301,I\n>1\311,8\n";
static char L10[]="movof\301,I\n";
static char L11[]="GBmovC\243(I),I\n>1\311,8\n";
static char L12[]="GBmovof\243(I),I\n";
static char L13[]="li\311+,A+\nli\311,A\n";
static char L14[]="mov\301+,I+\nmov\301,I\n";
static char L15[]="GBmov\243+2(I),I+\nmov\243(I),I\n";
static char L16[]="GA";
#define cr32 &optab[28]

static char L17[]="mov\301',I\nM'\301''\n";
static char L18[]="mov\301',I\nM't\301''\n";
static char L19[]="mov\301',I\nli\3620,B\nM\3620,A''\n";
static char L20[]="movb\301',I\n>1\311,8\nli\3620,256*B\nMb\3620,A''\n";
static char L21[]="GJmov\243(J),I\nM'\243(J)\n";
static char L22[]="GBQmov\243(I),(sp)\nM'\243(I)\nmov\250sp)+,I\n";
static char L23[]="GJmov\243(J),I\nli\3620,B\nM\3620,#(J)\n";
static char L24[]="GJmovb\243(J),I\n>1\311,8\nli\3620,256*B\nMb\3620,#(J)\n";
static char L25[]="GBQmov\243(I),(sp)\nM\302,#(I)\nmov\250sp)+,I\n";
static char L26[]="GBQmovb\250I),(sp)\nli\3620,256*B\nMb\3620,(I)\nmovb\250sp)+,I\n>1\311,8\n";
static char L27[]="GAM'\301+\nV\301\n";
static char L28[]="GJmov\243+2(J),I+\nmov\243(J),I\nM'\243+2(J)\nV\243(J)\n";
static char L29[]="GBQmov\243+2(I),(sp)\nQmov\243(I),(sp)\nM'\243+2(I)\nV\243(I)\nmov\250sp)+,I\nmov\250sp)+,I+\n";
#define cr37 &optab[48]

static char L30[]="GAMP\311\n";
static char L31[]="GAM\311\nM\311+\nV\311\n";
#define cr80 &optab[53]

static char L32[]="KA<1\311,8\nmovC\311,A\n>1\311,8\n";
static char L33[]="KAmovfo\311,A\n";
static char L34[]="GBKAmovf\311,#(I)\n";
static char L35[]="GBli\3620,B*256\nmovb\3620,#(I)\n>1\3620,8\nmov\3620,I\n";
static char L36[]="GBli\3620,B\nmov\3620,#(I)\nmov\3620,I\n";
static char L37[]="GBmovb\302,#(I)\n";
static char L38[]="GBmov\302,r0\n<1\3620,8\nMC\3620,#(I)\n";
static char L39[]="GBmovC\302',#(I)\nmovC\302,I\n>1\311,8\n";
static char L40[]="GBKAmovfo\311,#(I)\n";
static char L41[]="GBKI<1\312,8\nmovC\312,#(I)\n>1\312,8\nmov\312,I\n";
static char L42[]="KAGJmovf\311,#(J)\n";
static char L43[]="KAGJmovfo\311,#(J)\n";
static char L44[]="GDKA<1\311,8\nmov\250sp)+,r0\nmovC\311,(r0)\n>1\311,8\n";
static char L45[]="GDKAmov\250sp)+,r0\nmovfo\311,(r0)\n";
static char L46[]="KAmov\311+,A+\nmov\311,A\n";
static char L47[]="KAGJmov\311+,#+2(J)\nmov\311,#(J)\n";
static char L48[]="GDKAmov\250sp)+,r0\nmov\311,(r0)+\nmov\311+,(r0)\n";
#define cr16 &optab[90]

static char L49[]="KAli\3620,Z\nszcC\3620,A'\nsocC\311,A''\n";
#define L50 fas1

static char L51[]="KCGBli\3620,Z\nszcC\3620,#(I)\nsocC\250sp),#(I)\nmov\250sp)+,I\n";
#define cr45 &optab[94]

static char L52[]="GAM\311,B\n";
static char L53[]="GAmov\302,r0\n>2\3620,8\njeq\256+4\nM\311,0\n";
static char L54[]="GAKImov\312,r0\njeq\256+4\nM\311,0\n";
static char L55[]="KCGAmov\250sp)+,r0\njeq\256+4\nM\311,0\n";
static char L56[]="GA!li\3620,B\nbl\315'\n";
static char L57[]="GA!movD\302,r0\n>2\3620,8\njeq\256+6\nbl\315'\n";
static char L58[]="GA!KImov\312,r0\njeq\256+6\nbl\315'\n";
static char L59[]="KCGA!mov\250sp)+,r0\njeq\256+6\nbl\315'\n";
#define cr91 &optab[111]

static char L60[]="GAM\311\n";
static char L61[]="GAMt\311\n";
#define cr40 &optab[114]

static char L62[]="GA";
static char L63[]="li\311,A\nM\"\311,B\n";
static char L64[]="movC\301,I\n>1\311,8\nM\"\311,B\n";
static char L65[]="GAM\"\311,B\n";
#define add1 L66

static char L66[]="GAMD\302,I\n";
#define add2 L67

static char L67[]="GAKJMD\242(J),I\n";
#define add3 L68

static char L68[]="GAKIMP\312,I\n";
#define add5 L69

static char L69[]="KCGAMP\250sp)+,I\n";
static char L70[]="GAM\"\311+,B\nV\311\n";
static char L71[]="GAM\"\311,B\nM\"\311+,B+\nV\311\n";
static char L72[]="GAM\302,I+\nV\311\n";
static char L73[]="GAKIM\312,I+\nV\311\n";
static char L74[]="GAM\302,I\nM\302+,I+\nV\311\n";
#define addl1 L75

static char L75[]="GAKIM\312+,I+\nM\312,I\nV\311\n";
#define addl2 L76

static char L76[]="KCGAM\250sp)+,I\nM\250sp)+,I+\nV\311\n";
#define cr49 &optab[148]

#define L77 add3

static char L78[]="GCKAM\250sp)+,I\n";
#define L79 addl1

static char L80[]="KCGAM\250sp)+,I\nM\250sp)+,I+\n";
#define cr42 &optab[159]

static char L81[]="li\311,B\nmovb\301,r0\n>1\3620,8\nM\3620,I\n";
static char L82[]="li\311,B\nM\301,I\n";
static char L83[]="GAli\3620,B\nM\3620,I\n";
static char L84[]="GAMD\302,I\n";
static char L85[]="GAKJMD\242(J),I\n";
static char L86[]="GAKIMP\312,I\n";
static char L87[]="KCGAMP\250sp)+,I\n";
#define cr43 &optab[171]

static char L88[]="GA!KI!bl\315\n";
static char L89[]="KCGA!mov\250sp)+,J\nbl\315\n";
#define L90 add1

#define L91 add2

#define L92 add3

#define L93 add5

#define cr14 &optab[178]

static char L94[]="GAli\3620,B\ndiv\3620,I\n";
#define cr70 &optab[181]

static char L95[]="M'\301'\nxxx\nmov\301,I\n";
static char L96[]="M't\301'\nmov\301,I\n";
static char L97[]="movC\301',I\n>1\311,8\nM\"\311,B\n<1\311,8\nmovC\311,A\n>1\311,8\n";
#define addq1 L98

static char L98[]="M\302,A'\nmov\301,I\n";
#define addq20 L99

static char L99[]="movC\301',I\n>1\311,8\nM\311,B\n<1\311,8\nmovC\311,A\n>1\311,8\n";
#define addq1a L100

static char L100[]="movC\301',I\nMP\302,I\nmovC\311,A\n";
#define addq2 L101

static char L101[]="KBM\242(I),A'\nmov\301,I\n";
#define addq3 L102

static char L102[]="KAM\311,A'\nmov\301,I\n";
#define addq21 L103

static char L103[]="KCmovC\301',I\n>1\311,8\nM\250sp)+,I\n<1\311,8\nmovC\311,A\n>1\311,8\n";
#define addq4 L104

static char L104[]="KBGJM\242(I),#(J)\nmov\243(J),I\n";
#define addq4a L105

static char L105[]="movf\301',I\nKIMP\312,I\nmovf\311,A\n";
#define addq5 L106

static char L106[]="KCmovC\301',I\nMP\250sp)+,I\nmovC\311,A\n";
#define addq6 L107

static char L107[]="KCmovof\301',I\nMP\250sp)+,I\nmovfo\311,A''\n";
#define addq7 L108

static char L108[]="KAGJM\311,#(J)\nmov\243(J),I\n";
#define addq8 L109

static char L109[]="KCGBM\250sp)+,#(I)\nmov\243(I),I\n";
#define addq9 L110

static char L110[]="KCGBmov\311,r1\nmovC\243(r1),I\n>1\311,8\nMP\250sp)+,I\n<1\311,8\nmovC\311,#(r1)\n>1\311,8\n";
#define addq22 L111

#define L111 addq9

#define addq9a L112

static char L112[]="KCGBmovC\243(I),I\nMP\250sp)+,I\nmovC\311,#(I)\n";
#define addq10 L113

static char L113[]="KCGBmovof\243(I),J\nMP\250sp)+,J\nmovfo\312,#(I)\nmovf\312,I\n";
static char L114[]="M'\301+\nV\301\nGA";
static char L115[]="M't\301+\nV\301\nGA";
#define addq11 L116

static char L116[]="li\311,B\nM\311,A+\nV\301\nGA";
#define addq12 L117

static char L117[]="M\302+,A+\nV\301\nM\302,A\nGA";
#define addq13 L118

static char L118[]="KAM\311+,A+\nV\301\nM\311,A\nGA";
#define addq14 L119

static char L119[]="GBli\3620,B\nM\3620,#+2(I)\nV\243(I)\nmov\243+2(I),I+\nmov\243(I),I\n";
static char L120[]="GBli\3620,B+\nM\3620,#+2(I)\nV\243(I)\nli\3620,B\nM\3620,#(I)\nmov\243+2(I),I+\nmov\243(I),I\n";
#define addq15 L121

static char L121[]="GBM\302+,#+2(I)\nV\243(I)\nM\302,#(I)\nmov\243+2(I),I+\nmov\243(I),I\n";
#define addq16 L122

static char L122[]="KCGBM\250sp)+,#(I)\nM\250sp)+,#+2(I)\nV\243(I)\nmov\243+2(I),I+\nmov\243(I),I\n";
#define cr72 &optab[231]

static char L123[]="movC\301',I\n>1\311,8\nli\312,B\nM\312,I\n<1\312,8\nmovC\312,A\n";
static char L124[]="movC\301',I\n>1\311,8\nmovC\302,J\n>2\312,8\nM\312,I\n<1\312,8\nmovC\312,A\n";
static char L125[]="movC\301',I\n>1\311,8\nM\302,I\n<1\312,8\nmovC\312,A\n";
static char L126[]="KCmovC\301',I\n>1\311,8\nM\250sp)+,I\n<1\312,8\nmovC\312,A\n";
static char L127[]="GDKAmov\311,r0\nmov\250sp)+,r1\nmovC\243(r1),I\n>1\311,8\nM\3620,I\n<1\312,8\nmovC\312,#(r1)\n";
#define L128 addq1a

#define L129 addq4a

#define L130 addq5

#define L131 addq6

#define L132 addq9a

#define L133 addq10

#define cr73 &optab[250]

static char L134[]="movC\301',I\n>1\311,8\nKI!bl\315\n<1\311,8\nmovC\311,A\n>1\311,8\n";
static char L135[]="KCmovC\301',I\n>1\311,8\nmov\250sp)+,J\nbl\315\n<1\311,8\nmovC\311,A\n>1\311,8\n";
static char L136[]="GDKA!mov\311,J\nmov\250sp)+,r13\nmovC\243(r13),I\n>1\311,8\nbl\315\n<1\311,8\nmovC\311,#(r13)\n>1\311,8\n";
#define L137 addq1a

#define L138 addq4a

#define L139 addq5

#define L140 addq6

#define L141 addq9a

#define L142 addq10

#define cr79 &optab[263]

static char L143[]="KAM\301',I\nmov\311,A\n";
static char L144[]="KCmovb\301',I\n>1\311,8\nM\250sp)+,I\n<1\311,8\nmovb\311,A\n>1\311,8\n";
static char L145[]="GDKAmov\250sp)+,r1\nmovb\243(r1),r0\n>1\3620,8\nM\3620,I\n<1\311,8\nmovb\311,#(r1)\n>1\311,8\n";
static char L146[]="GDKAmov\250sp)+,r1\nM\250r1),I\nmov\311,(r1)\n";
#define cr75 &optab[270]

static char L147[]="M\301,B\nmov\301,I\n";
static char L148[]="KAmov\311,r0\njeq\256+4\nM\301,0\nmov\301,I\n";
static char L149[]="movC\301,I\n>1\311,8\nM\311,B\n<1\311,8\nmovC\311,A\n";
static char L150[]="KAmov\311,r0\nmovC\301,I\n>1\311,8\nM\311,0\n<1\311,8\nmovC\311,A\n";
static char L151[]="GA!li\3620,B\n>2\3620,8\njeq\256+6\nbl\315'\nmov\311+,A+\nmov\311,A\n";
static char L152[]="GA!KImov\312,r0\njeq\256+6\nbl\315'\nmov\311+,A+\nmov\311,A\n";
static char L153[]="KCGA!mov\250sp)+,r0\njeq\256+6\nbl\315'\nmov\311+,A+\nmov\311,A\n";
static char L154[]="KCGA!mov\250sp)+,r0\nmov\250sp)+,r0\njeq\256+6\nbl\315'\nmov\311+,A+\nmov\311,A\n";
#define cr78 &optab[285]

#define L155 addq1

static char L156[]="ML\302,A'\nclr\311\nbisb\301'',I\n";
#define L157 addq1a

#define L158 addq2

#define L159 addq3

static char L160[]="KCML\250sp)+,A'\nclr\311\nbisb\301'',I\n";
#define L161 addq4

#define L162 addq4a

#define L163 addq5

#define L164 addq6

#define L165 addq7

#define L166 addq8

#define L167 addq9

static char L168[]="GDKCML\250sp),*2(sp)\ntst\250sp)+\nclr\311\nbisb\252(sp)+,I\n";
#define L169 addq9a

#define L170 addq10

#define L171 addq11

#define L172 addq12

#define L173 addq13

#define L174 addq14

#define L175 addq15

#define L176 addq16

#define cr51 &optab[324]

static char L177[]="movif\301,I\n";
static char L178[]="GBmovif\243(I),I\n";
static char L179[]="GAmovif\311,I\n";
#define cr52 &optab[328]

static char L180[]="GAmovfi\311,I\n";
#define cr56 &optab[330]

static char L181[]="GAsetl\ndect\363p\ndect\363p\nmovfi\311,(sp)\nmov\250sp)+,I\nmov\250sp)+,I+\nseti\n";
#define cr57 &optab[332]

static char L182[]="setl\nmovif\301,I\nseti\n";
static char L183[]="GBsetl\nmovif\243(I),I\nseti\n";
static char L184[]="GCsetl\nmovif\250sp)+,I\nseti\n";
#define cr127 &optab[336]

static char L185[]="mov\301+,(sp)\ndect\363p\nmov\301,(sp)\nbl\315\nc\250sp)+,(sp)+\n";
static char L186[]="GBmov\243+2(I),(sp)\ndect\363p\nmov\243(I),(sp)\nbl\315\nc\250sp)+,(sp)+\n";
static char L187[]="GCbl\315\nc\250sp)+,(sp)+\n";
#define cr58 &optab[340]

static char L188[]="GI!clr\311\n";
static char L189[]="GAmov\311,J\nclr\311\n";
static char L190[]="GI!mov\312,I\nsra\311,15\n";
static char L191[]="GAmov\311,J\nsra\311,15\n";
#define cr59 &optab[345]

static char L192[]="mov\301+,I\n";
static char L193[]="GBmov\243+2(I),I\n";
#define cr82 &optab[350]

#define l82 L194

static char L194[]="KCGCbl\315\nai\363p,8\n";
#define cr121 &optab[355]

#define L195 l82

#define cr124 &optab[359]

#define L196 l86

#define cr86 &optab[364]

#define l86 L197

static char L197[]="KCGCbl\315\nai\363p,6\n";
#define cr109 &optab[367]

static char L198[]="GAsla\311,8\nsra\311,8\n";
#define cr117 &optab[369]

static char L199[]="GATli\3620,B\nM\3620,I-\n";
static char L200[]="GATM\302,I-\n";
static char L201[]="GATKJM\242(J),I-\n";
static char L202[]="GATKIM\312,I-\n";
static char L203[]="KCGATM\250sp)+,I-\n";
#define cr119 &optab[375]

static char L204[]="movC\301',I\n>1\311,8\nTli\3620,B\nM\3620,I-\n<1\311=,8\nmovC\311=,A\n>1\311=,8\n";
static char L205[]="movC\301',I\n>1\311,8\nTM\302,I-\n<1\311=,8\nmovC\311=,A\n>1\311=,8\n";
static char L206[]="KCmovC\301',I\n>1\311,8\nTM\250sp)+,I-\n<1\311=,8\nmovC\311=,A\n>1\311=,8\n";
static char L207[]="KCGJmovC\243(J),I\n>1\311,8\nTM\250sp)+,I-\n<1\311=,8\nmovC\311=,#(J)\n>1\311=,8\n";
static char L208[]="GDKCmov\3002(sp),r1\nmovC\243(r1),I\n>1\311,8\nTM\250sp)+,I-\n<1\311=,8\nmovC\311=,#(J)\n>1\311=,8\ninct\363p\n";
#define cr107 &optab[386]

static char L209[]="GA?sra\311,1\n";
#define cr130 &optab[388]

static char L210[]="GAli\312,B\ns\312,I\n";
static char L211[]="GAli\3620,B\ns\3620,I+\nV\311\n";
#define ci80 &optab[391]

static char L212[]="M'\301\n";
static char L213[]="M'\3620\nMC\3620,A\n";
static char L214[]="li\301,B\n";
static char L215[]="MD\302,A\n>2\301,8\n";
static char L216[]="KAM\311,A\n";
static char L217[]="li\311,256*B\nMC\311,A\n";
static char L218[]="MC\302,A\n";
static char L219[]="KA<1\311,8\nMC\311,A\n";
static char L220[]="M'C\301\n";
static char L221[]="GBclr\3620\nMC\3620,#(I)\n";
#define move2 L222

static char L222[]="GBM'C\243(I)\n";
static char L223[]="li\301,B\n";
static char L224[]="li\311,B\nM\311,A\n";
static char L225[]="li\311,B*256\nML\311,A\n";
#define move3 L226

static char L226[]="M\302,A\n";
#define move4 L227

static char L227[]="KBmovD\242(I),I\n>2\311,8\n<1\311,8\nMC\311,A\n";
#define move5 L228

static char L228[]="KA<1\311,8\nMC\311,A\n";
static char L229[]="GBli\3621,B*256\nMC\3621,#(I)\n";
static char L230[]="GBli\312,B\nMC\312,#(I)\n";
static char L231[]="GBmovb\302,#(I)\n";
#define move6 L232

static char L232[]="GBmov\302,r0\n<1\3620,8\nMC\3620,#(I)\n";
static char L233[]="GBM\302,#(I)\n";
#define move7 L234

static char L234[]="GBKJML\242(J),#(I)\n";
#define move8 L235

static char L235[]="GBKI<1\312,8\nMC\312,#(I)\n";
#define move9 L236

static char L236[]="KBGJML\242(I),#(J)\n";
#define move10 L237

static char L237[]="KAGJ<1\311,8\nMC\311,#(J)\n";
#define move11 L238

static char L238[]="GDKBmov\250sp)+,r1\nML\242(I),#(r1)\n";
#define move12 L239

static char L239[]="GDKAmov\250sp)+,r1\nMC\311,#(r1)\n";
static char L240[]="KAmovfi\311,A\n";
static char L241[]="KAGJmovfi\311,#(J)\n";
static char L242[]="clr\301\nclr\301+\n";
static char L243[]="GBclr\243(I)\nclr\243+2(I)\n";
static char L244[]="li\3620,B\nM\3620,A+\nsra\3620,15\nM\3620,A\n";
#define move13a L245

static char L245[]="M\302,A+\nV\301\n";
static char L246[]="KBmov\242(I),A+\nV\301\n";
static char L247[]="KAmov\311,A+\nV\301\n";
static char L248[]="KAsetl\nmovfi\311,A\nseti\n";
static char L249[]="KAGJsetl\nmovfi\311,#(J)\nseti\n";
static char L250[]="li\3620,B\nM\3620,A\nli\3620,B+\nM\3620,A+\n";
#define move13 L251

static char L251[]="M\302,A\nM\302+,A+\nV\301\n";
#define move14 L252

static char L252[]="KBM\242(I),A\nM\242+2(I),A+\nV\301\n";
#define move15 L253

static char L253[]="KAM\311,A\nM\311+,A+\nV\301\n";
#define move14a L254

static char L254[]="GBM\302,#+2(I)\nV\243(I)\n";
#define move16a L255

static char L255[]="GBM\302+,#+2(I)\nV\243(I)\nM\302,#(I)\n";
#define move16 L256

static char L256[]="KAGJM\311+,#+2(J)\nV\243(J)\nM\311,#(J)\n";
static char L257[]="KCGBmov\250sp)+,#+2(I)\nV\243(I)\n";
#define move17 L258

static char L258[]="KCGBM\250sp)+,#(I)\nM\250sp)+,#+2(I)\nV\243(I)\n";
#define ci78 &optab[496]

static char L259[]="M\"\301,B\n";
static char L260[]="GAM\"\311,B\n<1\311,8\nmovC\311,A\n";
#define L261 move3

static char L262[]="KAML\311,A\n";
#define L263 move5

static char L264[]="GBmovC\243(I),r0\nM\"\3620,B*256\nmovC\3620,#(I)\n";
static char L265[]="GBmov\243(I),r0\nM\"\3620,B\nmov\3620,#(I)\n";
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

static char L281[]="KAM\301,I\nM\301+,I+\nmov\311,A\nmov\311+,A+\n";
static char L282[]="KAGJM\243(J),I\nM\243+2(J),I+\nmov\311,#(J)\nmov\311+,#+2(J)\n";
static char L283[]="GDKAmov\250sp)+,r1\nM\243(r1),I\nM\243+2(r1),I+\nmov\311,#(J)\nmov\311+,#+2(J)\n";
#define ci70 &optab[565]

static char L284[]="M'\301\n";
static char L285[]="M't\301\n";
static char L286[]="M\"\301,B\n";
static char L287[]="li\3620,B*256\nMb\3620,A\n";
static char L288[]="li\3620,B\nM\3620,A\n";
#define L289 move3

#define L290 move4

#define L291 move5

static char L292[]="GBmov\243(I),r0\nM'\243(I)\nmov\3620,r0\n";
#define L293 move9

static char L294[]="KBmovC\301',J\n>1\312,8\nM\242(I),J\n<1\312,8\nmovC\312,A\n";
static char L295[]="KAmovC\301',J\n>1\312,8\nM\311,J\n<1\312,8\nmovC\312,A\n";
#define L296 move10

#define L297 move12

static char L298[]="KCGBmovC\243(I),J\n>1\312,8\nM\250sp)+,J\n<1\312,8\nmovC\312,#(I)\n";
static char L299[]="li\3620,B\nM\3620,A+\nV\301\nsra\3620,15\nM\3620,A\n";
static char L300[]="li\3620,B\nM\3620,A\nli\3620,B+\nM\3620,A+\nV\301\n";
#define L301 move13a

#define L302 move13

#define L303 move14

#define L304 move15

#define L305 move14a

#define L306 move16a

#define L307 move16

#define L308 move17

#define ci16 &optab[620]

static char L309[]="li\3620,Z*256\nszcb\3620,A'\nli\3620,B*256\nsocb\3620,A\n";
static char L310[]="li\3620,Z\nszc\3620,A'\nli\3620,B\nsoc\3620,A\n";
static char L311[]="li\3620,Z\nszcC\3620,A'\nli\3620,B\nsocC\3620,A\n";
static char L312[]="KAli\3620,Z\nszcC\3620,A'\n<1\311,8\nsocC\311,A\n";
static char L313[]="GBli\3620,Z\nszcC\3620,#(I)\nli\3620,B\nsocC\3620,#(I)\n";
#define fas1 L314

static char L314[]="KAGJli\3620,Z\nszcC\3620,#(J)\nsocC\311,#(J)\n";
static char L315[]="GBKIli\3620,Z\nszcC\3620,#(I)\nsocC\312,#(I)\n";
static char L316[]="KCGBli\3620,Z\nszcC\3620,#(I)\nsocC\250sp)+,#(I)\n";
#define cc60 &optab[629]

static char L317[]="li\3620,A\n";
static char L318[]="movC\301,r0\n";
static char L319[]="movof\301,I\n";
static char L320[]="GBmovC\243(I),r0\n";
static char L321[]="GBmovof\243(I),I\n";
static char L322[]="GE";
static char L323[]="ci\301,B\n";
static char L324[]="GAci\311,B\n";
static char L325[]="ML\301,B\n";
static char L326[]="GBML\243(I),B\n";
static char L327[]="GAMD\311,B\n";
static char L328[]="GBKJML\243(I),\"(J)\n";
static char L329[]="GBKIMC\243(I),J\n";
static char L330[]="GAKJMD\311,\"(J)\n";
static char L331[]="GAKIMP\311,J\n";
static char L332[]="KCGAMP\311,(sp)+\n";
static char L333[]="mov\301,r0\nX0mov\301+,r0\nX1";
static char L334[]="mov\301,r0\nX0mov\301+,r0\nci\3620,B\nX1";
static char L335[]="mov\301,r0\nci\3620,B\nX0mov\301+,r0\nci\3620,B+\nX1";
static char L336[]="li\3620,A\nM\3620,B\nX0li\3620,A+\nM\3620,B+\nX1";
static char L337[]="mov\301,r0\nX0M\301+,B\nX1";
#define lcmp1 L338

static char L338[]="M\301,B\nX0M\301+,B+\nX1";
static char L339[]="GBmov\243(I),r0\nX0mov\243+2(I),r0\nX1";
static char L340[]="GBmov\243(I),r0\nX0mov\243+2(I),r0\nci\3620,B\nX1";
static char L341[]="GBmov\243(I),r0\nX0M\243+2(I),B\nX1";
#define lcmp2 L342

static char L342[]="GBM\243(I),B\nX0M\243+2(I),B+\nX1";
static char L343[]="GAmov\311,r0\nX0mov\311+,r0\nX1";
static char L344[]="GAmov\311,r0\nX0ci\311+,B\nX1";
static char L345[]="GAci\311,B\nX0ci\311+,B+\nX1";
static char L346[]="GAmov\311,r0\nX0M\311+,B\nX1";
#define lcmp3 L347

static char L347[]="GAM\311,B\nX0M\311+,B+\nX1";
#define lcmp4 L348

static char L348[]="GBKJM\243(I),\"(J)\nX0M\243+2(I),\"+2(J)\nX1";
#define lcmp5 L349

static char L349[]="GAKJM\311,\"(J)\nX0M\311+,\"+2(J)\nX1";
#define lcmp6 L350

static char L350[]="GCKAQmov\311,(sp)\nmov\3004(sp),I\nmov\250sp)+,@2(sp)\nM\250sp)+,(sp)+\nX0M\311,I+\nX1";
#define cc81 &optab[709]

static char L351[]="movb\301,I\nM\"\311,B*256\n";
static char L352[]="GAM\"\311,B\n";
static char L353[]="KAmovC\301,r0\n>1\3620,8\nM\311,r0\n";
#define L354 move6

#define L355 add1

#define L356 add3

#define L357 add5

#define rest &optab[720]

static char L358[]="HA";
#define cs106 &optab[723]

static char L359[]="Qclr\250sp)\n";
static char L360[]="Qli\3620,A\nmov\3620,(sp)\n";
static char L361[]="Qmovb\301,r0\n>1\3620,8\nmov\3620,(sp)\n";
static char L362[]="Qmov\301,(sp)\n";
static char L363[]="GBQmov\243(I),(sp)\n";
static char L364[]="Qli\3620,A+\nmov\3620,(sp)\nQli\3620,A\nmov\3620,(sp)\n";
static char L365[]="Qmov\301+,(sp)\nQmov\301,(sp)\n";
#define cs91 &optab[734]

static char L366[]="GCM\250sp)\n";
static char L367[]="GCMt\250sp)\n";
#define cs40 &optab[737]

static char L368[]="QmovC\301,r0\n>1\3620,8\nM\"\3620,B\nmov\3620,(sp)\n";
static char L369[]="GCmov\250sp),r0\nM\"\3620,B\nmov\3620,(sp)\n";
static char L370[]="GCM\302,(sp)\n";
static char L371[]="GCKBM\242(I),(sp)\n";
static char L372[]="GCKAM\311,(sp)\n";
#define cs58 &optab[745]

static char L373[]="Qli\3620,A\nmov\3620,(sp)\nsra\3620,15\nQmov\3620,(sp)\n";
static char L374[]="GCQclr\250sp)\n";
static char L375[]="Qmov\301,r0\nmov\3620,(sp)\nsra\3620,15\nQmov\3620,(sp)\n";
#define cs56 &optab[749]

static char L376[]="GAsetl\nQmovfi\311,(sp)\nseti\n";
#define ci116 &optab[751]

static char L377[]="GA!KI!";
static char L378[]="KCGA!mov\250sp)+,r1\n";

/* goto (are these entries still used?) */
/* cr102 */

struct optab optab[]={
	{16,0,63,0,L1},	/* 0 */
	{127,0,63,0,L2},	/* 1 */
/* call */
	{0},
/* cr100 */
	{16,0,63,0,L3},	/* 3 */
	{127,0,63,0,L4},	/* 4 */
	{63,0,63,0,L5},	/* 5 */
/* load */
	{0},
/* cr106 */
	{4,0,63,0,L6},	/* 7 */
	{4,4,63,0,L7},	/* 8 */
	{8,0,63,0,L8},	/* 9 */
	{16,10,63,0,L9},	/* 10 */
	{16,3,63,0,L9},	/* 11 */
	{16,0,63,0,L9},	/* 12 */
	{16,5,63,0,L9},	/* 13 */
	{16,4,63,0,L10},	/* 14 */
	{127,10,63,0,L11},	/* 15 */
	{127,3,63,0,L11},	/* 16 */
	{127,0,63,0,L11},	/* 17 */
	{127,5,63,0,L11},	/* 18 */
	{127,4,63,0,L12},	/* 19 */
	{8,8,63,0,L13},	/* 20 */
	{8,11,63,0,L13},	/* 21 */
	{16,8,63,0,L14},	/* 22 */
	{16,11,63,0,L14},	/* 23 */
	{127,8,63,0,L15},	/* 24 */
	{127,11,63,0,L15},	/* 25 */
	{63,0,63,0,L16},	/* 26 */
/* ++,-- postfix; the right operand is always a CON */
	{0},
/* cr32 */
	{16,1,5,0,L17},	/* 28 */
	{16,1,6,0,L18},	/* 29 */
	{16,1,63,0,L19},	/* 30 */
	{16,3,63,0,L20},	/* 31 */
	{16,10,63,0,L20},	/* 32 */
	{84,1,5,0,L21},	/* 33 */
	{127,1,5,0,L22},	/* 34 */
	{84,1,63,0,L23},	/* 35 */
	{84,3,63,0,L24},	/* 36 */
	{84,10,63,0,L24},	/* 37 */
	{127,1,63,0,L25},	/* 38 */
	{127,3,63,0,L26},	/* 39 */
	{127,10,63,0,L26},	/* 40 */
	{16,8,5,0,L27},	/* 41 */
	{16,11,5,0,L27},	/* 42 */
	{84,8,5,0,L28},	/* 43 */
	{84,11,5,0,L28},	/* 44 */
	{127,8,5,0,L29},	/* 45 */
	{127,11,5,0,L29},	/* 46 */
/* - unary, ~ */
	{0},
/* cr37 */
	{63,0,63,0,L30},	/* 48 */
	{63,4,63,0,L30},	/* 49 */
	{63,8,63,0,L31},	/* 50 */
	{63,11,63,0,L31},	/* 51 */
/* = */
	{0},
/* cr80 */
	{16,10,63,0,L32},	/* 53 */
	{16,0,63,0,L32},	/* 54 */
	{16,5,63,4,L32},	/* 55 */
	{16,4,63,4,L33},	/* 56 */
	{127,5,16,4,L34},	/* 57 */
	{127,3,8,0,L35},	/* 58 */
	{127,10,8,0,L35},	/* 59 */
	{127,0,8,0,L36},	/* 60 */
	{127,3,16,3,L37},	/* 61 */
	{127,10,16,3,L37},	/* 62 */
	{127,3,16,10,L37},	/* 63 */
	{127,10,16,10,L37},	/* 64 */
	{127,3,16,0,L38},	/* 65 */
	{127,10,16,0,L38},	/* 66 */
	{127,0,16,1,L39},	/* 67 */
	{127,4,16,4,L40},	/* 68 */
	{127,10,20,0,L41},	/* 69 */
	{127,0,20,0,L41},	/* 70 */
	{84,5,63,4,L42},	/* 71 */
	{84,4,63,4,L43},	/* 72 */
	{127,10,63,0,L44},	/* 73 */
	{127,0,63,0,L44},	/* 74 */
	{127,5,63,4,L44},	/* 75 */
	{127,4,63,4,L45},	/* 76 */
	{16,8,63,8,L46},	/* 77 */
	{16,8,63,11,L46},	/* 78 */
	{16,11,63,8,L46},	/* 79 */
	{16,11,63,11,L46},	/* 80 */
	{84,8,63,8,L47},	/* 81 */
	{84,8,63,11,L47},	/* 82 */
	{84,11,63,8,L47},	/* 83 */
	{84,11,63,11,L47},	/* 84 */
	{127,8,63,8,L48},	/* 85 */
	{127,8,63,11,L48},	/* 86 */
	{127,11,63,8,L48},	/* 87 */
	{127,11,63,11,L48},	/* 88 */
/* field assign, value in reg. */
	{0},
/* cr16 */
	{16,0,63,0,L49},	/* 90 */
	{84,0,63,0,L50},	/* 91 */


	{127,0,63,0,L51},	/* 92 */
/* <<, >>, unsigned >> */
	{0},
/* cr45 */
	{63,10,8,0,L52},	/* 94 */
	{63,0,8,0,L52},	/* 95 */
	{63,10,16,0,L53},	/* 96 */
	{63,0,16,0,L53},	/* 97 */
	{63,10,20,0,L54},	/* 98 */
	{63,0,20,0,L54},	/* 99 */
	{63,10,63,0,L55},	/* 100 */
	{63,0,63,0,L55},	/* 101 */
	{63,8,8,0,L56},	/* 102 */
	{63,11,8,0,L56},	/* 103 */
	{63,11,16,0,L57},	/* 104 */
	{63,8,16,0,L57},	/* 105 */
	{63,11,20,0,L58},	/* 106 */
	{63,8,20,0,L58},	/* 107 */
	{63,11,63,0,L59},	/* 108 */
	{63,8,63,0,L59},	/* 109 */
/* +1, +2, -1, -2 */
	{0},
/* cr91 */
	{63,0,5,0,L60},	/* 111 */
	{63,0,6,0,L61},	/* 112 */
/* +, -, |, &~ */
	{0},
/* cr40 */
	{63,0,4,0,L62},	/* 114 */
	{8,0,8,0,L63},	/* 115 */
	{16,10,8,0,L64},	/* 116 */
	{16,0,8,0,L64},	/* 117 */
	{63,0,8,0,L65},	/* 118 */
	{63,0,16,1,L66},	/* 119 */
	{63,4,16,5,L66},	/* 120 */
	{63,0,84,1,L67},	/* 121 */
	{63,4,84,5,L67},	/* 122 */
	{63,0,20,0,L68},	/* 123 */
	{63,4,20,4,L68},	/* 124 */
	{63,0,63,0,L69},	/* 125 */
	{63,4,63,4,L69},	/* 126 */
	{63,8,8,0,L70},	/* 127 */
	{63,11,8,0,L70},	/* 128 */
	{63,8,8,8,L71},	/* 129 */
	{63,11,8,8,L71},	/* 130 */
	{63,8,16,0,L72},	/* 131 */
	{63,11,16,0,L72},	/* 132 */
	{63,8,20,0,L73},	/* 133 */
	{63,11,20,0,L73},	/* 134 */
	{63,8,16,8,L74},	/* 135 */
	{63,8,16,11,L74},	/* 136 */
	{63,11,16,8,L74},	/* 137 */
	{63,11,16,11,L74},	/* 138 */
	{63,8,20,8,L75},	/* 139 */
	{63,8,20,11,L75},	/* 140 */
	{63,11,20,8,L75},	/* 141 */
	{63,11,20,11,L75},	/* 142 */
	{63,8,63,8,L76},	/* 143 */
	{63,8,63,11,L76},	/* 144 */
	{63,11,63,8,L76},	/* 145 */
	{63,11,63,11,L76},	/* 146 */
/* ^ -- xor */
	{0},
/* cr49 */
	{63,0,20,0,L77},	/* 148 */


	{63,0,63,0,L78},	/* 149 */
	{63,8,20,8,L79},	/* 150 */
	{63,8,20,11,L79},	/* 151 */
	{63,11,20,8,L79},	/* 152 */
	{63,11,20,11,L79},	/* 153 */


	{63,8,63,8,L80},	/* 154 */
	{63,8,63,11,L80},	/* 155 */
	{63,11,63,8,L80},	/* 156 */
	{63,11,63,11,L80},	/* 157 */
/* '*' -- low word of result is okay for both signed and unsigned.
 * R is increased by 1 following these snippets.
 */
	{0},
/* cr42 */
	{16,3,8,0,L81},	/* 159 */
	{16,0,8,0,L82},	/* 160 */
	{63,0,8,0,L83},	/* 161 */
	{63,0,16,1,L84},	/* 162 */
	{63,4,16,5,L84},	/* 163 */
	{63,0,84,1,L85},	/* 164 */
	{63,4,84,5,L85},	/* 165 */
	{63,0,20,0,L86},	/* 166 */
	{63,4,20,4,L86},	/* 167 */
	{63,0,63,0,L87},	/* 168 */
	{63,4,63,4,L87},	/* 169 */
/* / and % -- signed */
	{0},
/* cr43 */
	{63,0,20,0,L88},	/* 171 */
	{63,0,63,0,L89},	/* 172 */
	{63,4,16,5,L90},	/* 173 */


	{63,4,84,5,L91},	/* 174 */


	{63,4,20,4,L92},	/* 175 */


	{63,4,63,4,L93},	/* 176 */


/* PTOI */
	{0},
/* cr14 */
	{63,8,16,0,L94},	/* 178 */
	{63,11,16,0,L94},	/* 179 */
/* +=, -= */
	{0},
/* cr70 */
	{16,1,5,0,L95},	/* 181 */
	{16,1,6,0,L96},	/* 182 */
	{16,10,8,0,L97},	/* 183 */
	{16,0,8,0,L97},	/* 184 */
	{16,1,16,1,L98},	/* 185 */
	{16,10,16,1,L99},	/* 186 */
	{16,0,16,1,L99},	/* 187 */
	{16,5,16,5,L100},	/* 188 */
	{16,1,127,1,L101},	/* 189 */
	{16,1,63,0,L102},	/* 190 */
	{16,0,63,0,L103},	/* 191 */
	{16,10,63,0,L103},	/* 192 */
	{84,1,127,1,L104},	/* 193 */
	{16,5,20,4,L105},	/* 194 */
	{16,5,63,4,L106},	/* 195 */
	{16,4,63,4,L107},	/* 196 */
	{84,1,63,0,L108},	/* 197 */
	{127,1,63,0,L109},	/* 198 */
	{127,10,63,0,L110},	/* 199 */
	{127,0,63,0,L110},	/* 200 */
	{127,10,63,0,L111},	/* 201 */


	{127,5,63,4,L112},	/* 202 */
	{127,4,63,4,L113},	/* 203 */
	{16,8,5,0,L114},	/* 204 */
	{16,11,5,0,L114},	/* 205 */
	{16,8,6,0,L115},	/* 206 */
	{16,11,6,0,L115},	/* 207 */
	{16,8,8,0,L116},	/* 208 */
	{16,11,8,0,L116},	/* 209 */
	{16,8,16,8,L117},	/* 210 */
	{16,8,16,11,L117},	/* 211 */
	{16,11,16,8,L117},	/* 212 */
	{16,11,16,11,L117},	/* 213 */
	{16,8,63,8,L118},	/* 214 */
	{16,8,63,11,L118},	/* 215 */
	{16,11,63,8,L118},	/* 216 */
	{16,11,63,11,L118},	/* 217 */
	{127,8,8,0,L119},	/* 218 */
	{127,11,8,0,L119},	/* 219 */
	{127,8,8,8,L120},	/* 220 */
	{127,11,8,8,L120},	/* 221 */
	{127,8,16,8,L121},	/* 222 */
	{127,8,16,11,L121},	/* 223 */
	{127,11,16,8,L121},	/* 224 */
	{127,11,16,11,L121},	/* 225 */
	{127,8,63,8,L122},	/* 226 */
	{127,8,63,11,L122},	/* 227 */
	{127,11,63,8,L122},	/* 228 */
	{127,11,63,11,L122},	/* 229 */
/* '*=' */
	{0},
/* cr72 */
	{16,10,8,0,L123},	/* 231 */
	{16,0,8,0,L123},	/* 232 */
	{16,10,16,3,L124},	/* 233 */
	{16,10,16,10,L124},	/* 234 */
	{16,0,16,3,L124},	/* 235 */
	{16,0,16,10,L124},	/* 236 */
	{16,10,16,1,L125},	/* 237 */
	{16,0,16,1,L125},	/* 238 */
	{16,10,63,0,L126},	/* 239 */
	{16,0,63,0,L126},	/* 240 */
	{127,10,63,0,L127},	/* 241 */
	{127,0,63,0,L127},	/* 242 */
	{16,5,16,5,L128},	/* 243 */


	{16,5,20,4,L129},	/* 244 */


	{16,5,63,4,L130},	/* 245 */


	{16,4,63,4,L131},	/* 246 */


	{127,5,63,4,L132},	/* 247 */


	{127,4,63,4,L133},	/* 248 */


/* /= and %= -- signed int */
	{0},
/* cr73 */
	{16,0,20,0,L134},	/* 250 */
	{16,10,20,0,L134},	/* 251 */
	{16,0,63,0,L135},	/* 252 */
	{16,10,63,0,L135},	/* 253 */
	{127,0,63,0,L136},	/* 254 */
	{127,10,63,0,L136},	/* 255 */
	{16,5,16,5,L137},	/* 256 */


	{16,5,20,4,L138},	/* 257 */


	{16,5,63,4,L139},	/* 258 */


	{16,4,63,4,L140},	/* 259 */


	{127,5,63,4,L141},	/* 260 */


	{127,4,63,4,L142},	/* 261 */


/* ^= -- =xor */
	{0},
/* cr79 */
	{16,1,63,0,L143},	/* 263 */
	{16,10,63,0,L144},	/* 264 */
	{16,3,63,0,L144},	/* 265 */
	{127,10,63,0,L145},	/* 266 */
	{127,3,63,0,L145},	/* 267 */
	{127,0,63,0,L146},	/* 268 */
/* <<=, >>=, unsigned >>= */
	{0},
/* cr75 */
	{9,0,8,0,L147},	/* 270 */
	{9,0,63,0,L148},	/* 271 */
	{16,10,8,0,L149},	/* 272 */
	{16,0,8,0,L149},	/* 273 */
	{16,10,63,0,L150},	/* 274 */
	{16,0,63,0,L150},	/* 275 */
	{16,11,8,0,L151},	/* 276 */
	{16,8,8,0,L151},	/* 277 */
	{16,11,20,0,L152},	/* 278 */
	{16,8,20,0,L152},	/* 279 */
	{16,11,63,0,L153},	/* 280 */
	{16,8,63,0,L153},	/* 281 */
/* with a long shift ignore the high word */
	{16,11,63,8,L154},	/* 282 */
	{16,8,63,8,L154},	/* 283 */
/* =|, =&~ */
	{0},
/* cr78 */
	{16,1,16,1,L155},	/* 285 */


	{16,10,16,0,L156},	/* 286 */
	{16,0,16,1,L157},	/* 287 */
	{16,5,16,5,L157},	/* 288 */


	{16,1,127,1,L158},	/* 289 */


	{16,1,63,0,L159},	/* 290 */


	{16,10,63,0,L160},	/* 291 */
	{84,1,127,1,L161},	/* 292 */


	{16,5,20,4,L162},	/* 293 */


	{16,0,63,0,L163},	/* 294 */
	{16,5,63,4,L163},	/* 295 */


	{16,4,63,4,L164},	/* 296 */


	{84,1,63,0,L165},	/* 297 */


	{127,1,63,0,L166},	/* 298 */


	{127,0,63,0,L167},	/* 299 */


	{127,10,63,0,L168},	/* 300 */
	{127,5,63,4,L169},	/* 301 */


	{127,4,63,4,L170},	/* 302 */


	{16,8,8,0,L171},	/* 303 */
	{16,11,8,0,L171},	/* 304 */


	{16,8,16,8,L172},	/* 305 */
	{16,8,16,11,L172},	/* 306 */
	{16,11,16,8,L172},	/* 307 */
	{16,11,16,11,L172},	/* 308 */


	{16,8,63,8,L173},	/* 309 */
	{16,8,63,11,L173},	/* 310 */
	{16,11,63,8,L173},	/* 311 */
	{16,11,63,11,L173},	/* 312 */


	{127,8,8,0,L174},	/* 313 */
	{127,11,8,0,L174},	/* 314 */


	{127,8,16,8,L175},	/* 315 */
	{127,8,16,11,L175},	/* 316 */
	{127,11,16,8,L175},	/* 317 */
	{127,11,16,11,L175},	/* 318 */


	{127,8,63,8,L176},	/* 319 */
	{127,8,63,11,L176},	/* 320 */
	{127,11,63,8,L176},	/* 321 */
	{127,11,63,11,L176},	/* 322 */


/* int -> float */
	{0},
/* cr51 */
	{16,1,63,0,L177},	/* 324 */
	{127,1,63,0,L178},	/* 325 */
	{63,0,63,0,L179},	/* 326 */
/* float, double -> int */
	{0},
/* cr52 */
	{63,4,63,0,L180},	/* 328 */
/* double (float) to long */
	{0},
/* cr56 */
	{63,4,63,0,L181},	/* 330 */
/* long to double */
	{0},
/* cr57 */
	{16,8,63,0,L182},	/* 332 */
	{127,8,63,0,L183},	/* 333 */
	{63,8,63,0,L184},	/* 334 */
/* unsigned long to float(double) */
	{0},
/* cr127 */
	{16,11,63,0,L185},	/* 336 */
	{127,11,63,0,L186},	/* 337 */
	{63,11,63,0,L187},	/* 338 */
/* integer to long */
	{0},
/* cr58 */
	{20,9,63,0,L188},	/* 340 */
	{63,9,63,0,L189},	/* 341 */
	{20,0,63,0,L190},	/* 342 */
	{63,0,63,0,L191},	/* 343 */
/* long to integer */
	{0},
/* cr59 */
	{16,8,63,0,L192},	/* 345 */
	{16,11,63,0,L192},	/* 346 */
	{127,8,63,0,L193},	/* 347 */
	{127,11,63,0,L193},	/* 348 */
/* *, /, % for longs. */
	{0},
/* cr82 */
	{63,8,63,8,L194},	/* 350 */
	{63,8,63,11,L194},	/* 351 */
	{63,11,63,8,L194},	/* 352 */
	{63,11,63,11,L194},	/* 353 */
/* *, /, % for unsigned long */
	{0},
/* cr121 */
	{63,11,63,8,L195},	/* 355 */
	{63,8,63,11,L195},	/* 356 */
	{63,11,63,11,L195},	/* 357 */


/* *=, /=, %= for unsigned long */
	{0},
/* cr124 */
	{63,0,63,8,L196},	/* 359 */
	{63,0,63,11,L196},	/* 360 */
	{63,8,63,0,L196},	/* 361 */
	{63,11,63,0,L196},	/* 362 */


/* *=, /=, %= for longs */
/* Operands of the form &x op y, so stack space is known. */
	{0},
/* cr86 */
	{63,0,63,8,L197},	/* 364 */
	{63,0,63,11,L197},	/* 365 */
/* convert integer to character (sign extend) */
	{0},
/* cr109 */
	{63,0,63,0,L198},	/* 367 */
/* / and % where divisor is unsigned or known to be positive */
	{0},
/* cr117 */
	{63,0,8,0,L199},	/* 369 */
	{63,0,16,1,L200},	/* 370 */
	{63,0,84,1,L201},	/* 371 */
	{63,0,20,0,L202},	/* 372 */
	{63,0,63,0,L203},	/* 373 */

/* /= and %= where divisor is unsigned or known to be positive */
	{0},
/* cr119 */
	{16,10,8,0,L204},	/* 375 */
	{16,0,8,0,L204},	/* 376 */
	{16,10,16,1,L205},	/* 377 */
	{16,0,16,1,L205},	/* 378 */
	{16,10,63,0,L206},	/* 379 */
	{16,0,63,0,L206},	/* 380 */
	{84,10,63,0,L207},	/* 381 */
	{84,0,63,0,L207},	/* 382 */
	{127,10,63,0,L208},	/* 383 */
	{127,0,63,0,L208},	/* 384 */
/* (int *) - (int *) */
	{0},
/* cr107 */
	{63,0,63,0,L209},	/* 386 */
/* x - &name */
	{0},
/* cr130 */
	{63,0,63,0,L210},	/* 388 */
	{63,8,63,0,L211},	/* 389 */


/* = */
	{0},
/* ci80 */
	{9,0,4,0,L212},	/* 391 */
	{16,3,4,0,L213},	/* 392 */
	{16,10,4,0,L213},	/* 393 */
	{9,0,8,0,L214},	/* 394 */
	{9,0,16,0,L215},	/* 395 */
	{9,0,16,10,L215},	/* 396 */
	{9,0,63,0,L216},	/* 397 */
	{16,3,8,0,L217},	/* 398 */
	{16,10,8,0,L217},	/* 399 */
	{16,3,16,3,L218},	/* 400 */
	{16,3,16,10,L218},	/* 401 */
	{16,10,16,3,L218},	/* 402 */
	{16,10,16,10,L218},	/* 403 */
	{16,3,63,0,L219},	/* 404 */
	{16,10,63,0,L219},	/* 405 */
	{16,1,4,0,L220},	/* 406 */
	{16,5,4,4,L220},	/* 407 */
	{127,3,4,0,L221},	/* 408 */
	{127,10,4,0,L221},	/* 409 */
	{127,0,4,0,L222},	/* 410 */
	{127,5,4,4,L222},	/* 411 */
	{127,10,4,0,L222},	/* 412 */
	{9,0,8,0,L223},	/* 413 */
	{16,0,8,0,L224},	/* 414 */
	{16,3,8,0,L225},	/* 415 */
	{16,10,8,0,L225},	/* 416 */
	{16,1,16,1,L226},	/* 417 */
	{16,0,127,0,L227},	/* 418 */
	{16,10,127,0,L227},	/* 419 */
	{16,0,63,0,L228},	/* 420 */
	{16,10,63,0,L228},	/* 421 */
	{127,3,8,0,L229},	/* 422 */
	{127,10,8,0,L229},	/* 423 */
	{127,0,8,0,L230},	/* 424 */
	{127,3,16,3,L231},	/* 425 */
	{127,10,16,3,L231},	/* 426 */
	{127,3,16,10,L231},	/* 427 */
	{127,10,16,10,L231},	/* 428 */
	{127,3,16,0,L232},	/* 429 */
	{127,10,16,0,L232},	/* 430 */
	{127,0,16,0,L233},	/* 431 */
	{127,0,84,1,L234},	/* 432 */
	{127,3,84,0,L234},	/* 433 */
	{127,10,84,0,L234},	/* 434 */
	{127,10,20,0,L235},	/* 435 */
	{127,0,20,0,L235},	/* 436 */
	{84,0,127,1,L236},	/* 437 */
	{84,3,127,0,L236},	/* 438 */
	{84,10,127,0,L236},	/* 439 */
	{84,0,63,0,L237},	/* 440 */
	{84,10,63,0,L237},	/* 441 */
	{127,0,127,1,L238},	/* 442 */
	{127,3,127,0,L238},	/* 443 */
	{127,10,127,0,L238},	/* 444 */
	{127,0,63,0,L239},	/* 445 */
	{127,10,63,0,L239},	/* 446 */
	{16,1,63,4,L240},	/* 447 */
	{84,1,63,4,L241},	/* 448 */
	{16,8,4,0,L242},	/* 449 */
	{16,11,4,0,L242},	/* 450 */
	{127,8,4,0,L243},	/* 451 */
	{127,11,4,0,L243},	/* 452 */

	{16,8,8,1,L244},	/* 453 */
	{16,11,8,1,L244},	/* 454 */
	{16,8,16,1,L245},	/* 455 */
	{16,11,16,1,L245},	/* 456 */
	{16,8,127,1,L246},	/* 457 */
	{16,11,127,1,L246},	/* 458 */
	{16,8,63,0,L247},	/* 459 */
	{16,11,63,0,L247},	/* 460 */
	{16,8,63,4,L248},	/* 461 */
	{16,11,63,4,L248},	/* 462 */
	{84,8,63,4,L249},	/* 463 */
	{84,11,63,4,L249},	/* 464 */
	{16,8,8,8,L250},	/* 465 */
	{16,11,8,8,L250},	/* 466 */
	{16,8,16,8,L251},	/* 467 */
	{16,8,16,11,L251},	/* 468 */
	{16,11,16,8,L251},	/* 469 */
	{16,11,16,11,L251},	/* 470 */
	{16,8,127,8,L252},	/* 471 */
	{16,8,127,11,L252},	/* 472 */
	{16,11,127,8,L252},	/* 473 */
	{16,11,127,11,L252},	/* 474 */
	{16,8,63,8,L253},	/* 475 */
	{16,8,63,11,L253},	/* 476 */
	{16,11,63,8,L253},	/* 477 */
	{16,11,63,11,L253},	/* 478 */
	{127,8,16,1,L254},	/* 479 */
	{127,11,16,1,L254},	/* 480 */
	{127,8,16,8,L255},	/* 481 */
	{127,8,16,11,L255},	/* 482 */
	{127,11,16,8,L255},	/* 483 */
	{127,11,16,11,L255},	/* 484 */
	{84,8,63,8,L256},	/* 485 */
	{84,8,63,11,L256},	/* 486 */
	{84,11,63,8,L256},	/* 487 */
	{84,11,63,11,L256},	/* 488 */
	{127,8,63,0,L257},	/* 489 */
	{127,11,63,0,L257},	/* 490 */
	{127,8,63,8,L258},	/* 491 */
	{127,8,63,11,L258},	/* 492 */
	{127,11,63,8,L258},	/* 493 */
	{127,11,63,11,L258},	/* 494 */
/* |= and &~= */
	{0},
/* ci78 */
	{9,0,8,0,L259},	/* 496 */
	{16,0,8,0,L260},	/* 497 */
	{16,10,8,0,L260},	/* 498 */
	{16,1,16,1,L261},	/* 499 */


	{16,10,63,0,L262},	/* 500 */
	{16,0,63,0,L263},	/* 501 */
	{16,10,63,0,L263},	/* 502 */


	{127,3,8,0,L264},	/* 503 */
	{127,10,8,0,L264},	/* 504 */
	{127,0,8,0,L265},	/* 505 */
	{127,0,16,1,L266},	/* 506 */
	{127,3,16,0,L266},	/* 507 */
	{127,10,16,0,L266},	/* 508 */


	{127,0,84,1,L267},	/* 509 */
	{127,3,84,0,L267},	/* 510 */
	{127,10,84,0,L267},	/* 511 */


	{127,0,20,0,L268},	/* 512 */


	{84,0,127,1,L269},	/* 513 */
	{84,3,127,0,L269},	/* 514 */
	{84,10,127,0,L269},	/* 515 */


	{84,0,63,0,L270},	/* 516 */


	{127,0,127,1,L271},	/* 517 */
	{127,3,127,0,L271},	/* 518 */
	{127,10,127,0,L271},	/* 519 */


	{127,0,63,0,L272},	/* 520 */


	{16,8,8,0,L273},	/* 521 */
	{16,8,16,9,L273},	/* 522 */
	{16,11,8,0,L273},	/* 523 */
	{16,11,16,9,L273},	/* 524 */


	{16,8,16,8,L274},	/* 525 */
	{16,8,16,11,L274},	/* 526 */
	{16,11,16,8,L274},	/* 527 */
	{16,11,16,11,L274},	/* 528 */


	{16,8,127,8,L275},	/* 529 */
	{16,8,127,11,L275},	/* 530 */
	{16,11,127,8,L275},	/* 531 */
	{16,11,127,11,L275},	/* 532 */


	{16,8,63,8,L276},	/* 533 */
	{16,8,63,11,L276},	/* 534 */
	{16,11,63,8,L276},	/* 535 */
	{16,11,63,11,L276},	/* 536 */


	{127,8,8,0,L277},	/* 537 */
	{127,11,8,0,L277},	/* 538 */


	{127,8,16,8,L278},	/* 539 */
	{127,8,16,11,L278},	/* 540 */
	{127,11,16,8,L278},	/* 541 */
	{127,11,16,11,L278},	/* 542 */


	{84,8,63,8,L279},	/* 543 */
	{84,8,63,11,L279},	/* 544 */
	{84,11,63,8,L279},	/* 545 */
	{84,11,63,11,L279},	/* 546 */


	{127,8,63,8,L280},	/* 547 */
	{127,8,63,11,L280},	/* 548 */
	{127,11,63,8,L280},	/* 549 */
	{127,11,63,11,L280},	/* 550 */


/* ^= */
	{0},
/* ci79 */
	{16,8,63,8,L281},	/* 552 */
	{16,8,63,11,L281},	/* 553 */
	{16,11,63,8,L281},	/* 554 */
	{16,11,63,11,L281},	/* 555 */
	{84,8,63,8,L282},	/* 556 */
	{84,8,63,11,L282},	/* 557 */
	{84,11,63,8,L282},	/* 558 */
	{84,11,63,11,L282},	/* 559 */
	{127,8,63,8,L283},	/* 560 */
	{127,8,63,11,L283},	/* 561 */
	{127,11,63,8,L283},	/* 562 */
	{127,11,63,11,L283},	/* 563 */
/* +=, -=, ++, -- */
	{0},
/* ci70 */
	{16,1,5,0,L284},	/* 565 */
	{16,1,6,0,L285},	/* 566 */
	{9,0,8,0,L286},	/* 567 */
	{16,3,8,0,L287},	/* 568 */
	{16,10,8,0,L287},	/* 569 */
	{16,0,8,0,L288},	/* 570 */
	{16,1,16,1,L289},	/* 571 */


	{16,0,127,0,L290},	/* 572 */
	{16,10,127,0,L290},	/* 573 */


	{16,0,63,0,L291},	/* 574 */
	{16,10,63,0,L291},	/* 575 */


	{127,1,5,0,L292},	/* 576 */
	{84,1,127,1,L293},	/* 577 */


	{16,0,84,1,L294},	/* 578 */
	{16,10,84,1,L294},	/* 579 */
	{16,0,63,0,L295},	/* 580 */
	{16,10,63,0,L295},	/* 581 */
	{84,1,63,0,L296},	/* 582 */


	{127,1,63,0,L297},	/* 583 */


	{127,0,63,0,L298},	/* 584 */
	{16,8,8,0,L299},	/* 585 */
	{16,11,8,0,L299},	/* 586 */
	{16,8,8,8,L300},	/* 587 */
	{16,11,8,8,L300},	/* 588 */
	{16,8,16,9,L301},	/* 589 */
	{16,11,16,9,L301},	/* 590 */


	{16,8,16,8,L302},	/* 591 */
	{16,8,16,11,L302},	/* 592 */
	{16,11,16,8,L302},	/* 593 */
	{16,11,16,11,L302},	/* 594 */


	{16,8,127,8,L303},	/* 595 */
	{16,8,127,11,L303},	/* 596 */
	{16,11,127,8,L303},	/* 597 */
	{16,11,127,11,L303},	/* 598 */


	{16,8,63,8,L304},	/* 599 */
	{16,8,63,11,L304},	/* 600 */
	{16,11,63,8,L304},	/* 601 */
	{16,11,63,11,L304},	/* 602 */


	{127,8,8,0,L305},	/* 603 */
	{127,8,16,9,L305},	/* 604 */
	{127,11,8,0,L305},	/* 605 */
	{127,11,16,9,L305},	/* 606 */


	{127,8,16,8,L306},	/* 607 */
	{127,8,16,11,L306},	/* 608 */
	{127,11,16,8,L306},	/* 609 */
	{127,11,16,11,L306},	/* 610 */


	{84,8,63,8,L307},	/* 611 */
	{84,8,63,11,L307},	/* 612 */
	{84,11,63,8,L307},	/* 613 */
	{84,11,63,11,L307},	/* 614 */


	{127,8,63,8,L308},	/* 615 */
	{127,8,63,11,L308},	/* 616 */
	{127,11,63,8,L308},	/* 617 */
	{127,11,63,11,L308},	/* 618 */


/* field = ... */
	{0},
/* ci16 */
	{16,3,8,0,L309},	/* 620 */
	{16,0,8,0,L310},	/* 621 */
	{16,0,16,0,L311},	/* 622 */
	{16,0,63,0,L312},	/* 623 */
	{127,0,16,0,L313},	/* 624 */
	{84,0,63,0,L314},	/* 625 */
	{127,0,20,0,L315},	/* 626 */
	{127,0,63,0,L316},	/* 627 */


/* relationals */
	{0},
/* cc60 */
	{8,0,4,0,L317},	/* 629 */
	{16,0,4,0,L318},	/* 630 */
	{16,5,4,4,L318},	/* 631 */
	{16,10,4,0,L318},	/* 632 */
	{16,4,4,0,L319},	/* 633 */
	{127,0,4,0,L320},	/* 634 */
	{127,5,4,4,L320},	/* 635 */
	{127,10,4,0,L320},	/* 636 */
	{127,4,4,0,L321},	/* 637 */
	{63,0,4,0,L322},	/* 638 */
	{63,4,4,4,L322},	/* 639 */
	{9,0,8,0,L323},	/* 640 */
	{63,0,8,0,L324},	/* 641 */
	{16,1,16,1,L325},	/* 642 */
	{16,3,16,3,L325},	/* 643 */
	{16,10,16,10,L325},	/* 644 */
	{127,1,16,1,L326},	/* 645 */
	{127,3,16,3,L326},	/* 646 */
	{127,10,16,10,L326},	/* 647 */
	{63,0,16,1,L327},	/* 648 */
	{63,4,16,5,L327},	/* 649 */
	{127,1,84,1,L328},	/* 650 */
	{127,3,84,3,L328},	/* 651 */
	{127,10,84,10,L328},	/* 652 */
	{127,1,20,0,L329},	/* 653 */
	{63,0,84,1,L330},	/* 654 */
	{63,4,84,5,L330},	/* 655 */
	{63,0,20,0,L331},	/* 656 */
	{63,4,20,4,L331},	/* 657 */
	{63,0,63,0,L332},	/* 658 */
	{63,4,63,4,L332},	/* 659 */
	{16,8,4,0,L333},	/* 660 */
	{16,11,4,0,L333},	/* 661 */
	{16,8,8,0,L334},	/* 662 */
	{16,11,8,0,L334},	/* 663 */
	{16,8,8,8,L335},	/* 664 */
	{16,11,8,8,L335},	/* 665 */

	{8,8,16,8,L336},	/* 666 */
	{8,8,16,11,L336},	/* 667 */
	{16,8,16,9,L337},	/* 668 */
	{16,11,16,9,L337},	/* 669 */
	{16,8,16,8,L338},	/* 670 */
	{16,8,16,11,L338},	/* 671 */
	{16,11,16,8,L338},	/* 672 */
	{16,11,16,11,L338},	/* 673 */
	{127,8,4,0,L339},	/* 674 */
	{127,11,4,0,L339},	/* 675 */
	{127,8,8,0,L340},	/* 676 */
	{127,11,8,0,L340},	/* 677 */
	{127,8,16,9,L341},	/* 678 */
	{127,11,16,9,L341},	/* 679 */
	{127,8,16,8,L342},	/* 680 */
	{127,8,16,11,L342},	/* 681 */
	{127,11,16,8,L342},	/* 682 */
	{127,8,16,11,L342},	/* 683 */
	{63,8,4,0,L343},	/* 684 */
	{63,11,4,0,L343},	/* 685 */
	{63,8,8,0,L344},	/* 686 */
	{63,11,8,0,L344},	/* 687 */
	{63,8,8,8,L345},	/* 688 */
	{63,11,8,8,L345},	/* 689 */
	{63,8,16,9,L346},	/* 690 */
	{63,11,16,9,L346},	/* 691 */
	{63,8,16,8,L347},	/* 692 */
	{63,8,16,11,L347},	/* 693 */
	{63,11,16,8,L347},	/* 694 */
	{63,11,16,11,L347},	/* 695 */
	{127,8,84,8,L348},	/* 696 */
	{127,8,84,11,L348},	/* 697 */
	{127,11,84,8,L348},	/* 698 */
	{127,11,84,11,L348},	/* 699 */
	{63,8,84,8,L349},	/* 700 */
	{63,8,84,11,L349},	/* 701 */
	{63,11,84,8,L349},	/* 702 */
	{63,11,84,11,L349},	/* 703 */
	{63,8,63,8,L350},	/* 704 */
	{63,8,63,11,L350},	/* 705 */
	{63,11,63,8,L350},	/* 706 */
	{63,11,63,11,L350},	/* 707 */
/* & as in "if ((a&b) ==0)" */
	{0},
/* cc81 */
	{16,3,8,0,L351},	/* 709 */
	{16,10,8,0,L351},	/* 710 */
	{63,0,8,0,L352},	/* 711 */
	{16,0,20,0,L353},	/* 712 */
	{16,10,20,0,L353},	/* 713 */
	{127,0,16,0,L354},	/* 714 */
	{127,10,16,0,L354},	/* 715 */


	{63,0,16,1,L355},	/* 716 */


	{63,0,20,0,L356},	/* 717 */


	{63,0,63,0,L357},	/* 718 */


/* set codes right by moving the result */
	{0},
/* rest */
	{63,0,63,0,L358},	/* 720 */
	{63,4,63,4,L358},	/* 721 */


/* load */
	{0},
/* cs106 */
	{4,0,63,0,L359},	/* 723 */
	{4,4,63,0,L359},	/* 724 */
	{8,0,63,0,L360},	/* 725 */
	{16,3,63,0,L361},	/* 726 */
	{16,10,63,0,L361},	/* 727 */
	{16,1,63,0,L362},	/* 728 */
	{127,1,63,0,L363},	/* 729 */
	{8,8,63,0,L364},	/* 730 */
	{16,8,63,0,L365},	/* 731 */
	{16,11,63,0,L365},	/* 732 */
/* +1, +2, -1, -2 */
	{0},
/* cs91 */
	{63,0,5,0,L366},	/* 734 */
	{63,0,6,0,L367},	/* 735 */
/* +, -, |, &~ */
	{0},
/* cs40 */
	{16,0,8,0,L368},	/* 737 */
	{16,10,8,0,L368},	/* 738 */
	{63,0,8,0,L369},	/* 739 */
	{63,10,8,0,L369},	/* 740 */
	{16,0,16,1,L370},	/* 741 */
	{16,0,127,1,L371},	/* 742 */
	{16,0,63,0,L372},	/* 743 */
/* integer to long */
	{0},
/* cs58 */
	{8,0,63,0,L373},	/* 745 */
	{63,9,63,0,L374},	/* 746 */
	{16,1,63,0,L375},	/* 747 */
/* float to long */
	{0},
/* cs56 */
	{63,4,63,0,L376},	/* 749 */
/* setup for structure assign */
	{0},
/* ci116 */
	{63,0,20,0,L377},	/* 751 */
	{63,0,63,0,L378},	/* 752 */
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
