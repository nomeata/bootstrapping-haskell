%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[PprMach]{Pretty-printing assembly language}

We start with the @pprXXX@s with some cross-platform commonality
(e.g., @pprReg@); we conclude with the no-commonality monster,
@pprInstr@.

\begin{code}
#include "nativeGen/NCG.h"

module PprMach ( pprInstr ) where

#include "HsVersions.h"

import MachRegs		-- may differ per-platform
import MachMisc

import AbsCSyn		( MagicId )
import CLabel		( pprCLabel_asm, externallyVisibleCLabel )
import CStrings		( charToC )
import Maybes		( maybeToBool )
import OrdList		( OrdList )
import Stix		( CodeSegment(..), StixTree )
import Char		( isPrint, isDigit )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{@pprReg@: print a @Reg@}
%*									*
%************************************************************************

For x86, the way we print a register name depends
on which bit of it we care about.  Yurgh.
\begin{code}
pprReg :: IF_ARCH_i386(Size ->,) Reg -> SDoc

pprReg IF_ARCH_i386(s,) r
  = case r of
      FixedReg  i -> ppr_reg_no IF_ARCH_i386(s,) i
      MappedReg i -> ppr_reg_no IF_ARCH_i386(s,) i
      other	  -> text (show other)   -- should only happen when debugging
  where
#if alpha_TARGET_ARCH
    ppr_reg_no :: FAST_REG_NO -> SDoc
    ppr_reg_no i = ptext
      (case i of {
	ILIT( 0) -> SLIT("$0");   ILIT( 1) -> SLIT("$1");
	ILIT( 2) -> SLIT("$2");   ILIT( 3) -> SLIT("$3");
	ILIT( 4) -> SLIT("$4");   ILIT( 5) -> SLIT("$5");
	ILIT( 6) -> SLIT("$6");   ILIT( 7) -> SLIT("$7");
	ILIT( 8) -> SLIT("$8");   ILIT( 9) -> SLIT("$9");
	ILIT(10) -> SLIT("$10");  ILIT(11) -> SLIT("$11");
	ILIT(12) -> SLIT("$12");  ILIT(13) -> SLIT("$13");
	ILIT(14) -> SLIT("$14");  ILIT(15) -> SLIT("$15");
	ILIT(16) -> SLIT("$16");  ILIT(17) -> SLIT("$17");
	ILIT(18) -> SLIT("$18");  ILIT(19) -> SLIT("$19");
	ILIT(20) -> SLIT("$20");  ILIT(21) -> SLIT("$21");
	ILIT(22) -> SLIT("$22");  ILIT(23) -> SLIT("$23");
	ILIT(24) -> SLIT("$24");  ILIT(25) -> SLIT("$25");
	ILIT(26) -> SLIT("$26");  ILIT(27) -> SLIT("$27");
	ILIT(28) -> SLIT("$28");  ILIT(29) -> SLIT("$29");
	ILIT(30) -> SLIT("$30");  ILIT(31) -> SLIT("$31");
	ILIT(32) -> SLIT("$f0");  ILIT(33) -> SLIT("$f1");
	ILIT(34) -> SLIT("$f2");  ILIT(35) -> SLIT("$f3");
	ILIT(36) -> SLIT("$f4");  ILIT(37) -> SLIT("$f5");
	ILIT(38) -> SLIT("$f6");  ILIT(39) -> SLIT("$f7");
	ILIT(40) -> SLIT("$f8");  ILIT(41) -> SLIT("$f9");
	ILIT(42) -> SLIT("$f10"); ILIT(43) -> SLIT("$f11");
	ILIT(44) -> SLIT("$f12"); ILIT(45) -> SLIT("$f13");
	ILIT(46) -> SLIT("$f14"); ILIT(47) -> SLIT("$f15");
	ILIT(48) -> SLIT("$f16"); ILIT(49) -> SLIT("$f17");
	ILIT(50) -> SLIT("$f18"); ILIT(51) -> SLIT("$f19");
	ILIT(52) -> SLIT("$f20"); ILIT(53) -> SLIT("$f21");
	ILIT(54) -> SLIT("$f22"); ILIT(55) -> SLIT("$f23");
	ILIT(56) -> SLIT("$f24"); ILIT(57) -> SLIT("$f25");
	ILIT(58) -> SLIT("$f26"); ILIT(59) -> SLIT("$f27");
	ILIT(60) -> SLIT("$f28"); ILIT(61) -> SLIT("$f29");
	ILIT(62) -> SLIT("$f30"); ILIT(63) -> SLIT("$f31");
	_ -> SLIT("very naughty alpha register")
      })
#endif
#if i386_TARGET_ARCH
    ppr_reg_no :: Size -> FAST_REG_NO -> SDoc
    ppr_reg_no B i = ptext
      (case i of {
	ILIT( 0) -> SLIT("%al");  ILIT( 1) -> SLIT("%bl");
	ILIT( 2) -> SLIT("%cl");  ILIT( 3) -> SLIT("%dl");
	_ -> SLIT("very naughty I386 byte register")
      })

    {- UNUSED:
    ppr_reg_no HB i = ptext
      (case i of {
	ILIT( 0) -> SLIT("%ah");  ILIT( 1) -> SLIT("%bh");
	ILIT( 2) -> SLIT("%ch");  ILIT( 3) -> SLIT("%dh");
	_ -> SLIT("very naughty I386 high byte register")
      })
    -}

{- UNUSED:
    ppr_reg_no S i = ptext
      (case i of {
	ILIT( 0) -> SLIT("%ax");  ILIT( 1) -> SLIT("%bx");
	ILIT( 2) -> SLIT("%cx");  ILIT( 3) -> SLIT("%dx");
	ILIT( 4) -> SLIT("%si");  ILIT( 5) -> SLIT("%di");
	ILIT( 6) -> SLIT("%bp");  ILIT( 7) -> SLIT("%sp");
	_ -> SLIT("very naughty I386 word register")
      })
-}

    ppr_reg_no L i = ptext
      (case i of {
	ILIT( 0) -> SLIT("%eax");  ILIT( 1) -> SLIT("%ebx");
	ILIT( 2) -> SLIT("%ecx");  ILIT( 3) -> SLIT("%edx");
	ILIT( 4) -> SLIT("%esi");  ILIT( 5) -> SLIT("%edi");
	ILIT( 6) -> SLIT("%ebp");  ILIT( 7) -> SLIT("%esp");
	_ -> SLIT("very naughty I386 double word register")
      })

    ppr_reg_no F i = ptext
      (case i of {
	--ToDo: rm these (???)
	ILIT( 8) -> SLIT("%st(0)");  ILIT( 9) -> SLIT("%st(1)");
	ILIT(10) -> SLIT("%st(2)");  ILIT(11) -> SLIT("%st(3)");
	ILIT(12) -> SLIT("%st(4)");  ILIT(13) -> SLIT("%st(5)");
	ILIT(14) -> SLIT("%st(6)");  ILIT(15) -> SLIT("%st(7)");
	_ -> SLIT("very naughty I386 float register")
      })

    ppr_reg_no DF i = ptext
      (case i of {
	--ToDo: rm these (???)
	ILIT( 8) -> SLIT("%st(0)");  ILIT( 9) -> SLIT("%st(1)");
	ILIT(10) -> SLIT("%st(2)");  ILIT(11) -> SLIT("%st(3)");
	ILIT(12) -> SLIT("%st(4)");  ILIT(13) -> SLIT("%st(5)");
	ILIT(14) -> SLIT("%st(6)");  ILIT(15) -> SLIT("%st(7)");
	_ -> SLIT("very naughty I386 float register")
      })
#endif
#if sparc_TARGET_ARCH
    ppr_reg_no :: FAST_REG_NO -> SDoc
    ppr_reg_no i = ptext
      (case i of {
	ILIT( 0) -> SLIT("%g0");  ILIT( 1) -> SLIT("%g1");
	ILIT( 2) -> SLIT("%g2");  ILIT( 3) -> SLIT("%g3");
	ILIT( 4) -> SLIT("%g4");  ILIT( 5) -> SLIT("%g5");
	ILIT( 6) -> SLIT("%g6");  ILIT( 7) -> SLIT("%g7");
	ILIT( 8) -> SLIT("%o0");  ILIT( 9) -> SLIT("%o1");
	ILIT(10) -> SLIT("%o2");  ILIT(11) -> SLIT("%o3");
	ILIT(12) -> SLIT("%o4");  ILIT(13) -> SLIT("%o5");
	ILIT(14) -> SLIT("%o6");  ILIT(15) -> SLIT("%o7");
	ILIT(16) -> SLIT("%l0");  ILIT(17) -> SLIT("%l1");
	ILIT(18) -> SLIT("%l2");  ILIT(19) -> SLIT("%l3");
	ILIT(20) -> SLIT("%l4");  ILIT(21) -> SLIT("%l5");
	ILIT(22) -> SLIT("%l6");  ILIT(23) -> SLIT("%l7");
	ILIT(24) -> SLIT("%i0");  ILIT(25) -> SLIT("%i1");
	ILIT(26) -> SLIT("%i2");  ILIT(27) -> SLIT("%i3");
	ILIT(28) -> SLIT("%i4");  ILIT(29) -> SLIT("%i5");
	ILIT(30) -> SLIT("%i6");  ILIT(31) -> SLIT("%i7");
	ILIT(32) -> SLIT("%f0");  ILIT(33) -> SLIT("%f1");
	ILIT(34) -> SLIT("%f2");  ILIT(35) -> SLIT("%f3");
	ILIT(36) -> SLIT("%f4");  ILIT(37) -> SLIT("%f5");
	ILIT(38) -> SLIT("%f6");  ILIT(39) -> SLIT("%f7");
	ILIT(40) -> SLIT("%f8");  ILIT(41) -> SLIT("%f9");
	ILIT(42) -> SLIT("%f10"); ILIT(43) -> SLIT("%f11");
	ILIT(44) -> SLIT("%f12"); ILIT(45) -> SLIT("%f13");
	ILIT(46) -> SLIT("%f14"); ILIT(47) -> SLIT("%f15");
	ILIT(48) -> SLIT("%f16"); ILIT(49) -> SLIT("%f17");
	ILIT(50) -> SLIT("%f18"); ILIT(51) -> SLIT("%f19");
	ILIT(52) -> SLIT("%f20"); ILIT(53) -> SLIT("%f21");
	ILIT(54) -> SLIT("%f22"); ILIT(55) -> SLIT("%f23");
	ILIT(56) -> SLIT("%f24"); ILIT(57) -> SLIT("%f25");
	ILIT(58) -> SLIT("%f26"); ILIT(59) -> SLIT("%f27");
	ILIT(60) -> SLIT("%f28"); ILIT(61) -> SLIT("%f29");
	ILIT(62) -> SLIT("%f30"); ILIT(63) -> SLIT("%f31");
	_ -> SLIT("very naughty sparc register")
      })
#endif
\end{code}

%************************************************************************
%*									*
\subsection{@pprSize@: print a @Size@}
%*									*
%************************************************************************

\begin{code}
pprSize :: Size -> SDoc

pprSize x = ptext (case x of
#if alpha_TARGET_ARCH
	 B  -> SLIT("b")
	 BU -> SLIT("bu")
--	 W  -> SLIT("w") UNUSED
--	 WU -> SLIT("wu") UNUSED
--	 L  -> SLIT("l") UNUSED
	 Q  -> SLIT("q")
--	 FF -> SLIT("f") UNUSED
--	 DF -> SLIT("d") UNUSED
--	 GF -> SLIT("g") UNUSED
--	 SF -> SLIT("s") UNUSED
	 TF -> SLIT("t")
#endif
#if i386_TARGET_ARCH
	B  -> SLIT("b")
--	HB -> SLIT("b") UNUSED
--	S  -> SLIT("w") UNUSED
	L  -> SLIT("l")
	F  -> SLIT("s")
	DF -> SLIT("l")
#endif
#if sparc_TARGET_ARCH
	B   -> SLIT("sb")
	BU  -> SLIT("ub")
--	HW  -> SLIT("hw") UNUSED
--	HWU -> SLIT("uhw") UNUSED
	W   -> SLIT("")
	F   -> SLIT("")
--	D   -> SLIT("d") UNUSED
	DF  -> SLIT("d")
    )
pprStSize :: Size -> SDoc
pprStSize x = ptext (case x of
	B   -> SLIT("b")
	BU  -> SLIT("b")
--	HW  -> SLIT("hw") UNUSED
--	HWU -> SLIT("uhw") UNUSED
	W   -> SLIT("")
	F   -> SLIT("")
--	D   -> SLIT("d") UNUSED
	DF  -> SLIT("d")
#endif
    )
\end{code}

%************************************************************************
%*									*
\subsection{@pprCond@: print a @Cond@}
%*									*
%************************************************************************

\begin{code}
pprCond :: Cond -> SDoc

pprCond c = ptext (case c of {
#if alpha_TARGET_ARCH
	EQQ  -> SLIT("eq");
	LTT  -> SLIT("lt");
	LE  -> SLIT("le");
	ULT -> SLIT("ult");
	ULE -> SLIT("ule");
	NE  -> SLIT("ne");
	GTT  -> SLIT("gt");
	GE  -> SLIT("ge")
#endif
#if i386_TARGET_ARCH
	GEU	-> SLIT("ae");	LU    -> SLIT("b");
	EQQ	-> SLIT("e");	GTT    -> SLIT("g");
	GE	-> SLIT("ge");	GU    -> SLIT("a");
	LTT	-> SLIT("l");	LE    -> SLIT("le");
	LEU	-> SLIT("be");	NE    -> SLIT("ne");
	NEG	-> SLIT("s");	POS   -> SLIT("ns");
	ALWAYS	-> SLIT("mp")	-- hack
#endif
#if sparc_TARGET_ARCH
	ALWAYS	-> SLIT("");	NEVER -> SLIT("n");
	GEU	-> SLIT("geu");	LU    -> SLIT("lu");
	EQQ	-> SLIT("e");	GTT   -> SLIT("g");
	GE	-> SLIT("ge");	GU    -> SLIT("gu");
	LTT	-> SLIT("l");	LE    -> SLIT("le");
	LEU	-> SLIT("leu");	NE    -> SLIT("ne");
	NEG	-> SLIT("neg");	POS   -> SLIT("pos");
	VC	-> SLIT("vc");	VS    -> SLIT("vs")
#endif
    })
\end{code}

%************************************************************************
%*									*
\subsection{@pprImm@: print an @Imm@}
%*									*
%************************************************************************

\begin{code}
pprImm :: Imm -> SDoc

pprImm (ImmInt i)     = int i
pprImm (ImmInteger i) = integer i
pprImm (ImmCLbl l)    = pprCLabel_asm l
pprImm (ImmLit s)     = s

pprImm (ImmLab s) | underscorePrefix = (<>) (char '_') s
		  | otherwise	     = s

#if sparc_TARGET_ARCH
pprImm (LO i)
  = hcat [ pp_lo, pprImm i, rparen ]
  where
    pp_lo = text "%lo("

pprImm (HI i)
  = hcat [ pp_hi, pprImm i, rparen ]
  where
    pp_hi = text "%hi("
#endif
\end{code}

%************************************************************************
%*									*
\subsection{@pprAddr@: print an @Addr@}
%*									*
%************************************************************************

\begin{code}
pprAddr :: MachRegsAddr -> SDoc

#if alpha_TARGET_ARCH
pprAddr (AddrReg r) = parens (pprReg r)
pprAddr (AddrImm i) = pprImm i
pprAddr (AddrRegImm r1 i)
  = (<>) (pprImm i) (parens (pprReg r1))
#endif

-------------------

#if i386_TARGET_ARCH
pprAddr (ImmAddr imm off)
  = let
	pp_imm = pprImm imm
    in
    if (off == 0) then
	pp_imm
    else if (off < 0) then
	(<>) pp_imm (int off)
    else
	hcat [pp_imm, char '+', int off]

pprAddr (AddrBaseIndex base index displacement)
  = let
	pp_disp  = ppr_disp displacement
	pp_off p = (<>) pp_disp (parens p)
	pp_reg r = pprReg L r
    in
    case (base,index) of
      (Nothing, Nothing)    -> pp_disp
      (Just b,  Nothing)    -> pp_off (pp_reg b)
      (Nothing, Just (r,i)) -> pp_off (hcat [pp_reg r, comma, int i])
      (Just b,  Just (r,i)) -> pp_off (hcat [pp_reg b, comma, pp_reg r, comma, int i])
  where
    ppr_disp (ImmInt 0) = empty
    ppr_disp imm        = pprImm imm
#endif

-------------------

#if sparc_TARGET_ARCH
pprAddr (AddrRegReg r1 (FixedReg ILIT(0))) = pprReg r1

pprAddr (AddrRegReg r1 r2)
  = hcat [ pprReg r1, char '+', pprReg r2 ]

pprAddr (AddrRegImm r1 (ImmInt i))
  | i == 0 = pprReg r1
  | not (fits13Bits i) = largeOffsetError i
  | otherwise = hcat [ pprReg r1, pp_sign, int i ]
  where
    pp_sign = if i > 0 then char '+' else empty

pprAddr (AddrRegImm r1 (ImmInteger i))
  | i == 0 = pprReg r1
  | not (fits13Bits i) = largeOffsetError i
  | otherwise  = hcat [ pprReg r1, pp_sign, integer i ]
  where
    pp_sign = if i > 0 then char '+' else empty

pprAddr (AddrRegImm r1 imm)
  = hcat [ pprReg r1, char '+', pprImm imm ]
#endif
\end{code}

%************************************************************************
%*									*
\subsection{@pprInstr@: print an @Instr@}
%*									*
%************************************************************************

\begin{code}
pprInstr :: Instr -> SDoc

--pprInstr (COMMENT s) = (<>) (ptext SLIT("# "))   (ptext s)
pprInstr (COMMENT s) = empty -- nuke 'em
--alpha:  = (<>) (ptext SLIT("\t# ")) (ptext s)
--i386 :  = (<>) (ptext SLIT("# "))   (ptext s)
--sparc:  = (<>) (ptext SLIT("! "))   (ptext s)

pprInstr (SEGMENT TextSegment)
    = ptext
	 IF_ARCH_alpha(SLIT("\t.text\n\t.align 3") {-word boundary-}
	,IF_ARCH_sparc(SLIT("\t.text\n\t.align 4") {-word boundary-}
	,IF_ARCH_i386((_PK_ ".text\n\t.align 2\x2c\&0x90") {-needs per-OS variation!-}
	,)))

pprInstr (SEGMENT DataSegment)
    = ptext
	 IF_ARCH_alpha(SLIT("\t.data\n\t.align 3")
	,IF_ARCH_sparc(SLIT("\t.data\n\t.align 8") {-<8 will break double constants -}
	,IF_ARCH_i386(SLIT(".data\n\t.align 2")
	,)))

pprInstr (LABEL clab)
  = let
	pp_lab = pprCLabel_asm clab
    in
    hcat [
	if not (externallyVisibleCLabel clab) then
	    empty
	else
	    hcat [ptext
			 IF_ARCH_alpha(SLIT("\t.globl\t")
		        ,IF_ARCH_i386(SLIT(".globl ")
			,IF_ARCH_sparc(SLIT("\t.global\t")
			,)))
			, pp_lab, char '\n'],
	pp_lab,
	char ':'
    ]

pprInstr (ASCII False{-no backslash conversion-} str)
  = hcat [ ptext SLIT("\t.asciz "), char '\"', text str, char '"' ]

pprInstr (ASCII True str)
  = (<>) (text "\t.ascii \"") (asciify str 60)
  where
    asciify :: String -> Int -> SDoc

    asciify [] _ = text "\\0\""
    asciify s     n | n <= 0 = (<>) (text "\"\n\t.ascii \"") (asciify s 60)
    asciify ('\\':cs)      n = (<>) (text "\\\\") (asciify cs (n-1))
    asciify ('\"':cs)      n = (<>) (text "\\\"") (asciify cs (n-1))
    asciify (c:cs) n | isPrint c = (<>) (char c) (asciify cs (n-1))
    asciify [c]            _ = (<>) (text (charToC c)) (text ("\\0\"")){-"-}
    asciify (c:(cs@(d:_))) n
      | isDigit d = (<>) (text (charToC c)) (asciify cs 0)
      | otherwise = (<>) (text (charToC c)) (asciify cs (n-1))

pprInstr (DATA s xs)
  = vcat [(<>) (ptext pp_size) (pprImm x) | x <- xs]
  where
    pp_size = case s of
#if alpha_TARGET_ARCH
	    B  -> SLIT("\t.byte\t")
	    BU -> SLIT("\t.byte\t")
--UNUSED:   W  -> SLIT("\t.word\t")
--UNUSED:   WU -> SLIT("\t.word\t")
--UNUSED:   L  -> SLIT("\t.long\t")
	    Q  -> SLIT("\t.quad\t")
--UNUSED:   FF -> SLIT("\t.f_floating\t")
--UNUSED:   DF -> SLIT("\t.d_floating\t")
--UNUSED:   GF -> SLIT("\t.g_floating\t")
--UNUSED:   SF -> SLIT("\t.s_floating\t")
	    TF -> SLIT("\t.t_floating\t")
#endif
#if i386_TARGET_ARCH
	    B  -> SLIT("\t.byte\t")
--UNUSED:   HB -> SLIT("\t.byte\t")
--UNUSED:   S  -> SLIT("\t.word\t")
	    L  -> SLIT("\t.long\t")
	    F  -> SLIT("\t.long\t")
    	    DF -> SLIT("\t.double\t")
#endif
#if sparc_TARGET_ARCH
	    B  -> SLIT("\t.byte\t")
	    BU -> SLIT("\t.byte\t")
	    W  -> SLIT("\t.word\t")
    	    DF -> SLIT("\t.double\t")
#endif

-- fall through to rest of (machine-specific) pprInstr...
\end{code}

%************************************************************************
%*									*
\subsubsection{@pprInstr@ for an Alpha}
%*									*
%************************************************************************

\begin{code}
#if alpha_TARGET_ARCH

pprInstr (LD size reg addr)
  = hcat [
	ptext SLIT("\tld"),
	pprSize size,
	char '\t',
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDA reg addr)
  = hcat [
	ptext SLIT("\tlda\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDAH reg addr)
  = hcat [
	ptext SLIT("\tldah\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDGP reg addr)
  = hcat [
	ptext SLIT("\tldgp\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDI size reg imm)
  = hcat [
	ptext SLIT("\tldi"),
	pprSize size,
	char '\t',
	pprReg reg,
	comma,
	pprImm imm
    ]

pprInstr (ST size reg addr)
  = hcat [
	ptext SLIT("\tst"),
	pprSize size,
	char '\t',
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (CLR reg)
  = hcat [
	ptext SLIT("\tclr\t"),
	pprReg reg
    ]

pprInstr (ABS size ri reg)
  = hcat [
	ptext SLIT("\tabs"),
	pprSize size,
	char '\t',
	pprRI ri,
	comma,
	pprReg reg
    ]

pprInstr (NEG size ov ri reg)
  = hcat [
	ptext SLIT("\tneg"),
	pprSize size,
	if ov then ptext SLIT("v\t") else char '\t',
	pprRI ri,
	comma,
	pprReg reg
    ]

pprInstr (ADD size ov reg1 ri reg2)
  = hcat [
	ptext SLIT("\tadd"),
	pprSize size,
	if ov then ptext SLIT("v\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (SADD size scale reg1 ri reg2)
  = hcat [
	ptext (case scale of {{-UNUSED:L -> SLIT("\ts4");-} Q -> SLIT("\ts8")}),
	ptext SLIT("add"),
	pprSize size,
	char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (SUB size ov reg1 ri reg2)
  = hcat [
	ptext SLIT("\tsub"),
	pprSize size,
	if ov then ptext SLIT("v\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (SSUB size scale reg1 ri reg2)
  = hcat [
	ptext (case scale of {{-UNUSED:L -> SLIT("\ts4");-} Q -> SLIT("\ts8")}),
	ptext SLIT("sub"),
	pprSize size,
	char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (MUL size ov reg1 ri reg2)
  = hcat [
	ptext SLIT("\tmul"),
	pprSize size,
	if ov then ptext SLIT("v\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (DIV size uns reg1 ri reg2)
  = hcat [
	ptext SLIT("\tdiv"),
	pprSize size,
	if uns then ptext SLIT("u\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (REM size uns reg1 ri reg2)
  = hcat [
	ptext SLIT("\trem"),
	pprSize size,
	if uns then ptext SLIT("u\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (NOT ri reg)
  = hcat [
	ptext SLIT("\tnot"),
	char '\t',
	pprRI ri,
	comma,
	pprReg reg
    ]

pprInstr (AND reg1 ri reg2) = pprRegRIReg SLIT("and") reg1 ri reg2
pprInstr (ANDNOT reg1 ri reg2) = pprRegRIReg SLIT("andnot") reg1 ri reg2
pprInstr (OR reg1 ri reg2) = pprRegRIReg SLIT("or") reg1 ri reg2
pprInstr (ORNOT reg1 ri reg2) = pprRegRIReg SLIT("ornot") reg1 ri reg2
pprInstr (XOR reg1 ri reg2) = pprRegRIReg SLIT("xor") reg1 ri reg2
pprInstr (XORNOT reg1 ri reg2) = pprRegRIReg SLIT("xornot") reg1 ri reg2

pprInstr (SLL reg1 ri reg2) = pprRegRIReg SLIT("sll") reg1 ri reg2
pprInstr (SRL reg1 ri reg2) = pprRegRIReg SLIT("srl") reg1 ri reg2
pprInstr (SRA reg1 ri reg2) = pprRegRIReg SLIT("sra") reg1 ri reg2

pprInstr (ZAP reg1 ri reg2) = pprRegRIReg SLIT("zap") reg1 ri reg2
pprInstr (ZAPNOT reg1 ri reg2) = pprRegRIReg SLIT("zapnot") reg1 ri reg2

pprInstr (NOP) = ptext SLIT("\tnop")

pprInstr (CMP cond reg1 ri reg2)
  = hcat [
	ptext SLIT("\tcmp"),
	pprCond cond,
	char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (FCLR reg)
  = hcat [
	ptext SLIT("\tfclr\t"),
	pprReg reg
    ]

pprInstr (FABS reg1 reg2)
  = hcat [
	ptext SLIT("\tfabs\t"),
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (FNEG size reg1 reg2)
  = hcat [
	ptext SLIT("\tneg"),
	pprSize size,
	char '\t',
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (FADD size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("add") size reg1 reg2 reg3
pprInstr (FDIV size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("div") size reg1 reg2 reg3
pprInstr (FMUL size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("mul") size reg1 reg2 reg3
pprInstr (FSUB size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("sub") size reg1 reg2 reg3

pprInstr (CVTxy size1 size2 reg1 reg2)
  = hcat [
	ptext SLIT("\tcvt"),
	pprSize size1,
	case size2 of {Q -> ptext SLIT("qc"); _ -> pprSize size2},
	char '\t',
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (FCMP size cond reg1 reg2 reg3)
  = hcat [
	ptext SLIT("\tcmp"),
	pprSize size,
	pprCond cond,
	char '\t',
	pprReg reg1,
	comma,
	pprReg reg2,
	comma,
	pprReg reg3
    ]

pprInstr (FMOV reg1 reg2)
  = hcat [
	ptext SLIT("\tfmov\t"),
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (BI ALWAYS reg lab) = pprInstr (BR lab)

pprInstr (BI NEVER reg lab) = empty

pprInstr (BI cond reg lab)
  = hcat [
	ptext SLIT("\tb"),
	pprCond cond,
	char '\t',
	pprReg reg,
	comma,
	pprImm lab
    ]

pprInstr (BF cond reg lab)
  = hcat [
	ptext SLIT("\tfb"),
	pprCond cond,
	char '\t',
	pprReg reg,
	comma,
	pprImm lab
    ]

pprInstr (BR lab)
  = (<>) (ptext SLIT("\tbr\t")) (pprImm lab)

pprInstr (JMP reg addr hint)
  = hcat [
	ptext SLIT("\tjmp\t"),
	pprReg reg,
	comma,
	pprAddr addr,
	comma,
	int hint
    ]

pprInstr (BSR imm n)
  = (<>) (ptext SLIT("\tbsr\t")) (pprImm imm)

pprInstr (JSR reg addr n)
  = hcat [
	ptext SLIT("\tjsr\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (FUNBEGIN clab)
  = hcat [
	if (externallyVisibleCLabel clab) then
	    hcat [ptext SLIT("\t.globl\t"), pp_lab, char '\n']
	else
	    empty,
	ptext SLIT("\t.ent "),
	pp_lab,
	char '\n',
	pp_lab,
	pp_ldgp,
	pp_lab,
	pp_frame
    ]
    where
	pp_lab = pprCLabel_asm clab

	pp_ldgp  = ptext SLIT(":\n\tldgp $29,0($27)\n")
	pp_frame = ptext SLIT("..ng:\n\t.frame $30,4240,$26,0\n\t.prologue 1")

pprInstr (FUNEND clab)
  = (<>) (ptext SLIT("\t.align 4\n\t.end ")) (pprCLabel_asm clab)
\end{code}

Continue with Alpha-only printing bits and bobs:
\begin{code}
pprRI :: RI -> SDoc

pprRI (RIReg r) = pprReg r
pprRI (RIImm r) = pprImm r

pprRegRIReg :: FAST_STRING -> Reg -> RI -> Reg -> SDoc

pprRegRIReg name reg1 ri reg2
  = hcat [
 	char '\t',
	ptext name,
	char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprSizeRegRegReg :: FAST_STRING -> Size -> Reg -> Reg -> Reg -> SDoc

pprSizeRegRegReg name size reg1 reg2 reg3
  = hcat [
	char '\t',
	ptext name,
	pprSize size,
	char '\t',
	pprReg reg1,
	comma,
	pprReg reg2,
	comma,
	pprReg reg3
    ]

#endif {-alpha_TARGET_ARCH-}
\end{code}

%************************************************************************
%*									*
\subsubsection{@pprInstr@ for an I386}
%*									*
%************************************************************************

\begin{code}
#if i386_TARGET_ARCH

pprInstr v@(MOV size s@(OpReg src) d@(OpReg dst)) -- hack
  | src == dst
  =
#ifdef DEBUG
    (<>) (ptext SLIT("# warning: ")) (pprSizeOpOp SLIT("mov") size s d)
#else
    (ptext SLIT(""))
#endif
pprInstr (MOV size src dst)
  = pprSizeOpOp SLIT("mov") size src dst
pprInstr (MOVZX size src dst) = pprSizeOpOpCoerce SLIT("movzx") L size src dst
pprInstr (MOVSX size src dst) = pprSizeOpOpCoerce SLIT("movxs") L size src dst

-- here we do some patching, since the physical registers are only set late
-- in the code generation.
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(Just reg1) (Just (reg2,1)) (ImmInt 0))) dst@(OpReg reg3))
  | reg1 == reg3
  = pprSizeOpOp SLIT("add") size (OpReg reg2) dst
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(Just reg1) (Just (reg2,1)) (ImmInt 0))) dst@(OpReg reg3))
  | reg2 == reg3
  = pprSizeOpOp SLIT("add") size (OpReg reg1) dst
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(Just reg1) Nothing displ)) dst@(OpReg reg3))
  | reg1 == reg3
  = pprInstr (ADD size (OpImm displ) dst)
pprInstr (LEA size src dst) = pprSizeOpOp SLIT("lea") size src dst

pprInstr (ADD size (OpImm (ImmInt (-1))) dst)
  = pprSizeOp SLIT("dec") size dst
pprInstr (ADD size (OpImm (ImmInt 1)) dst)
  = pprSizeOp SLIT("inc") size dst
pprInstr (ADD size src dst)
  = pprSizeOpOp SLIT("add") size src dst
pprInstr (SUB size src dst) = pprSizeOpOp SLIT("sub") size src dst
pprInstr (IMUL size op1 op2) = pprSizeOpOp SLIT("imul") size op1 op2
pprInstr (IDIV size op) = pprSizeOp SLIT("idiv") size op

pprInstr (AND size src dst) = pprSizeOpOp SLIT("and") size src dst
pprInstr (OR  size src dst) = pprSizeOpOp SLIT("or")  size src dst
pprInstr (XOR size src dst) = pprSizeOpOp SLIT("xor")  size src dst
pprInstr (NOT size op) = pprSizeOp SLIT("not") size op
pprInstr (NEGI size op) = pprSizeOp SLIT("neg") size op

pprInstr (SHL size imm dst) = pprSizeByteOpOp SLIT("shl")  size imm dst
pprInstr (SAR size imm dst) = pprSizeByteOpOp SLIT("sar")  size imm dst
pprInstr (SHR size imm dst) = pprSizeByteOpOp SLIT("shr")  size imm dst

pprInstr (CMP size src dst) = pprSizeOpOp SLIT("cmp")  size src dst
pprInstr (TEST size src dst) = pprSizeOpOp SLIT("test")  size src dst
pprInstr (PUSH size op) = pprSizeOp SLIT("push") size op
pprInstr (POP size op) = pprSizeOp SLIT("pop") size op

pprInstr (NOP) = ptext SLIT("\tnop")
pprInstr (CLTD) = ptext SLIT("\tcltd")

pprInstr (SETCC cond op) = pprCondInstr SLIT("set") cond (pprOperand B op)

pprInstr (JXX cond lab) = pprCondInstr SLIT("j") cond (pprCLabel_asm lab)

pprInstr (JMP (OpImm imm)) = (<>) (ptext SLIT("\tjmp ")) (pprImm imm)
pprInstr (JMP op) = (<>) (ptext SLIT("\tjmp *")) (pprOperand L op)

pprInstr (CALL imm)
  = hcat [ ptext SLIT("\tcall "), pprImm imm ]

pprInstr SAHF = ptext SLIT("\tsahf")
pprInstr FABS = ptext SLIT("\tfabs")

pprInstr (FADD sz src@(OpAddr _))
  = hcat [ptext SLIT("\tfadd"), pprSize sz, space, pprOperand sz src]
pprInstr (FADD sz src)
  = ptext SLIT("\tfadd")
pprInstr FADDP
  = ptext SLIT("\tfaddp")
pprInstr (FMUL sz src)
  = hcat [ptext SLIT("\tfmul"), pprSize sz, space, pprOperand sz src]
pprInstr FMULP
  = ptext SLIT("\tfmulp")
pprInstr (FIADD size op) = pprSizeAddr SLIT("fiadd") size op
pprInstr FCHS = ptext SLIT("\tfchs")
pprInstr (FCOM size op) = pprSizeOp SLIT("fcom") size op
pprInstr FCOS = ptext SLIT("\tfcos")
pprInstr (FIDIV size op) = pprSizeAddr SLIT("fidiv") size op
pprInstr (FDIV sz src)
  = hcat [ptext SLIT("\tfdiv"), pprSize sz, space, pprOperand sz src]
pprInstr FDIVP
  = ptext SLIT("\tfdivp")
pprInstr (FDIVR sz src)
  = hcat [ptext SLIT("\tfdivr"), pprSize sz, space, pprOperand sz src]
pprInstr FDIVRP
  = ptext SLIT("\tfdivpr")
pprInstr (FIDIVR size op) = pprSizeAddr SLIT("fidivr") size op
pprInstr (FICOM size op) = pprSizeAddr SLIT("ficom") size op
pprInstr (FILD sz op reg) = pprSizeAddrReg SLIT("fild") sz op reg
pprInstr (FIST size op) = pprSizeAddr SLIT("fist") size op
pprInstr (FLD sz (OpImm (ImmCLbl src)))
  = hcat [ptext SLIT("\tfld"),pprSize sz,space,pprCLabel_asm src]
pprInstr (FLD sz src)
  = hcat [ptext SLIT("\tfld"),pprSize sz,space,pprOperand sz src]
pprInstr FLD1 = ptext SLIT("\tfld1")
pprInstr FLDZ = ptext SLIT("\tfldz")
pprInstr (FIMUL size op) = pprSizeAddr SLIT("fimul") size op
pprInstr FRNDINT = ptext SLIT("\tfrndint")
pprInstr FSIN = ptext SLIT("\tfsin")
pprInstr FSQRT = ptext SLIT("\tfsqrt")
pprInstr (FST sz dst)
  = hcat [ptext SLIT("\tfst"), pprSize sz, space, pprOperand sz dst]
pprInstr (FSTP sz dst)
  = hcat [ptext SLIT("\tfstp"), pprSize sz, space, pprOperand sz dst]
pprInstr (FISUB size op) = pprSizeAddr SLIT("fisub") size op
pprInstr (FSUB sz src)
  = hcat [ptext SLIT("\tfsub"), pprSize sz, space, pprOperand sz src]
pprInstr FSUBP
  = ptext SLIT("\tfsubp")
pprInstr (FSUBR size src)
  = pprSizeOp SLIT("fsubr") size src
pprInstr FSUBRP
  = ptext SLIT("\tfsubpr")
pprInstr (FISUBR size op)
  = pprSizeAddr SLIT("fisubr") size op
pprInstr FTST = ptext SLIT("\tftst")
pprInstr (FCOMP sz op)
  = hcat [ptext SLIT("\tfcomp"), pprSize sz, space, pprOperand sz op]
pprInstr FUCOMPP = ptext SLIT("\tfucompp")
pprInstr FXCH = ptext SLIT("\tfxch")
pprInstr FNSTSW = ptext SLIT("\tfnstsw %ax")
pprInstr FNOP = ptext SLIT("")
\end{code}

Continue with I386-only printing bits and bobs:
\begin{code}
pprDollImm :: Imm -> SDoc

pprDollImm i     = hcat [ ptext SLIT("$"), pprImm i]

pprOperand :: Size -> Operand -> SDoc
pprOperand s (OpReg r) = pprReg s r
pprOperand s (OpImm i) = pprDollImm i
pprOperand s (OpAddr ea) = pprAddr ea

pprSizeOp :: FAST_STRING -> Size -> Operand -> SDoc
pprSizeOp name size op1
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprOperand size op1
    ]

pprSizeOpOp :: FAST_STRING -> Size -> Operand -> Operand -> SDoc
pprSizeOpOp name size op1 op2
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprOperand size op1,
	comma,
	pprOperand size op2
    ]

pprSizeByteOpOp :: FAST_STRING -> Size -> Operand -> Operand -> SDoc
pprSizeByteOpOp name size op1 op2
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprOperand B op1,
	comma,
	pprOperand size op2
    ]

pprSizeOpReg :: FAST_STRING -> Size -> Operand -> Reg -> SDoc
pprSizeOpReg name size op1 reg
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprOperand size op1,
	comma,
	pprReg size reg
    ]

pprSizeAddr :: FAST_STRING -> Size -> MachRegsAddr -> SDoc
pprSizeAddr name size op
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprAddr op
    ]

pprSizeAddrReg :: FAST_STRING -> Size -> MachRegsAddr -> Reg -> SDoc
pprSizeAddrReg name size op dst
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprAddr op,
	comma,
	pprReg size dst
    ]

pprOpOp :: FAST_STRING -> Size -> Operand -> Operand -> SDoc
pprOpOp name size op1 op2
  = hcat [
    	char '\t',
	ptext name, space,
	pprOperand size op1,
	comma,
	pprOperand size op2
    ]

pprSizeOpOpCoerce :: FAST_STRING -> Size -> Size -> Operand -> Operand -> SDoc
pprSizeOpOpCoerce name size1 size2 op1 op2
  = hcat [ char '\t', ptext name, space,
	pprOperand size1 op1,
	comma,
	pprOperand size2 op2
    ]

pprCondInstr :: FAST_STRING -> Cond -> SDoc -> SDoc
pprCondInstr name cond arg
  = hcat [ char '\t', ptext name, pprCond cond, space, arg]

#endif {-i386_TARGET_ARCH-}
\end{code}

%************************************************************************
%*									*
\subsubsection{@pprInstr@ for a SPARC}
%*									*
%************************************************************************

\begin{code}
#if sparc_TARGET_ARCH

-- a clumsy hack for now, to handle possible double alignment problems

-- even clumsier, to allow for RegReg regs that show when doing indexed
-- reads (bytearrays).
--
pprInstr (LD DF (AddrRegReg g1 g2) reg)
  = hcat [
	ptext SLIT("\tadd\t"), pprReg g1,comma,pprReg g2,comma,pprReg g1, char '\n',
	pp_ld_lbracket, pprReg g1, pp_rbracket_comma, pprReg reg, char '\n',
	pp_ld_lbracket, pprReg g1, ptext SLIT("+4]"), comma, pprReg (fPair reg)
    ]

pprInstr (LD DF addr reg) | maybeToBool off_addr
  = hcat [
	pp_ld_lbracket,
	pprAddr addr,
	pp_rbracket_comma,
	pprReg reg,

	char '\n',
	pp_ld_lbracket,
	pprAddr addr2,
	pp_rbracket_comma,
	pprReg (fPair reg)
    ]
  where
    off_addr = addrOffset addr 4
    addr2 = case off_addr of Just x -> x

pprInstr (LD size addr reg)
  = hcat [
	ptext SLIT("\tld"),
	pprSize size,
	char '\t',
	lbrack,
	pprAddr addr,
	pp_rbracket_comma,
	pprReg reg
    ]

-- The same clumsy hack as above

pprInstr (ST DF reg (AddrRegReg g1 g2))
 = hcat [
	ptext SLIT("\tadd\t"),
		      pprReg g1,comma,pprReg g2,comma,pprReg g1, char '\n',
	ptext SLIT("\tst\t"),    
	      pprReg reg, pp_comma_lbracket, pprReg g1,
	ptext SLIT("]\n\tst\t"), 
	      pprReg (fPair reg), pp_comma_lbracket, pprReg g1, ptext SLIT("+4]")
    ]

pprInstr (ST DF reg addr) | maybeToBool off_addr 
 = hcat [
	ptext SLIT("\tst\t"),
	pprReg reg, pp_comma_lbracket,	pprAddr addr,

	ptext SLIT("]\n\tst\t"),
	pprReg (fPair reg), pp_comma_lbracket,
	pprAddr addr2, rbrack
    ]
  where
    off_addr = addrOffset addr 4
    addr2 = case off_addr of Just x -> x

-- no distinction is made between signed and unsigned bytes on stores for the
-- Sparc opcodes (at least I cannot see any, and gas is nagging me --SOF),
-- so we call a special-purpose pprSize for ST..

pprInstr (ST size reg addr)
  = hcat [
	ptext SLIT("\tst"),
	pprStSize size,
	char '\t',
	pprReg reg,
	pp_comma_lbracket,
	pprAddr addr,
	rbrack
    ]

pprInstr (ADD x cc reg1 ri reg2)
  | not x && not cc && riZero ri
  = hcat [ ptext SLIT("\tmov\t"), pprReg reg1, comma, pprReg reg2 ]
  | otherwise
  = pprRegRIReg (if x then SLIT("addx") else SLIT("add")) cc reg1 ri reg2

pprInstr (SUB x cc reg1 ri reg2)
  | not x && cc && reg2 == g0
  = hcat [ ptext SLIT("\tcmp\t"), pprReg reg1, comma, pprRI ri ]
  | not x && not cc && riZero ri
  = hcat [ ptext SLIT("\tmov\t"), pprReg reg1, comma, pprReg reg2 ]
  | otherwise
  = pprRegRIReg (if x then SLIT("subx") else SLIT("sub")) cc reg1 ri reg2

pprInstr (AND  b reg1 ri reg2) = pprRegRIReg SLIT("and")  b reg1 ri reg2
pprInstr (ANDN b reg1 ri reg2) = pprRegRIReg SLIT("andn") b reg1 ri reg2

pprInstr (OR b reg1 ri reg2)
  | not b && reg1 == g0
  = hcat [ ptext SLIT("\tmov\t"), pprRI ri, comma, pprReg reg2 ]
  | otherwise
  = pprRegRIReg SLIT("or") b reg1 ri reg2

pprInstr (ORN b reg1 ri reg2) = pprRegRIReg SLIT("orn") b reg1 ri reg2

pprInstr (XOR  b reg1 ri reg2) = pprRegRIReg SLIT("xor")  b reg1 ri reg2
pprInstr (XNOR b reg1 ri reg2) = pprRegRIReg SLIT("xnor") b reg1 ri reg2

pprInstr (SLL reg1 ri reg2) = pprRegRIReg SLIT("sll") False reg1 ri reg2
pprInstr (SRL reg1 ri reg2) = pprRegRIReg SLIT("srl") False reg1 ri reg2
pprInstr (SRA reg1 ri reg2) = pprRegRIReg SLIT("sra") False reg1 ri reg2

pprInstr (SETHI imm reg)
  = hcat [
	ptext SLIT("\tsethi\t"),
	pprImm imm,
	comma,
	pprReg reg
    ]

pprInstr NOP = ptext SLIT("\tnop")

pprInstr (FABS F reg1 reg2) = pprSizeRegReg SLIT("fabs") F reg1 reg2
pprInstr (FABS DF reg1 reg2)
  = (<>) (pprSizeRegReg SLIT("fabs") F reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg SLIT("fmov") F (fPair reg1) (fPair reg2)))

pprInstr (FADD size reg1 reg2 reg3)
  = pprSizeRegRegReg SLIT("fadd") size reg1 reg2 reg3
pprInstr (FCMP e size reg1 reg2)
  = pprSizeRegReg (if e then SLIT("fcmpe") else SLIT("fcmp")) size reg1 reg2
pprInstr (FDIV size reg1 reg2 reg3)
  = pprSizeRegRegReg SLIT("fdiv") size reg1 reg2 reg3

pprInstr (FMOV F reg1 reg2) = pprSizeRegReg SLIT("fmov") F reg1 reg2
pprInstr (FMOV DF reg1 reg2)
  = (<>) (pprSizeRegReg SLIT("fmov") F reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg SLIT("fmov") F (fPair reg1) (fPair reg2)))

pprInstr (FMUL size reg1 reg2 reg3)
  = pprSizeRegRegReg SLIT("fmul") size reg1 reg2 reg3

pprInstr (FNEG F reg1 reg2) = pprSizeRegReg SLIT("fneg") F reg1 reg2
pprInstr (FNEG DF reg1 reg2)
  = (<>) (pprSizeRegReg SLIT("fneg") F reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg SLIT("fmov") F (fPair reg1) (fPair reg2)))

pprInstr (FSQRT size reg1 reg2)     = pprSizeRegReg SLIT("fsqrt") size reg1 reg2
pprInstr (FSUB size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("fsub") size reg1 reg2 reg3
pprInstr (FxTOy size1 size2 reg1 reg2)
  = hcat [
    	ptext SLIT("\tf"),
	ptext
    	(case size1 of
    	    W  -> SLIT("ito")
    	    F  -> SLIT("sto")
    	    DF -> SLIT("dto")),
	ptext
    	(case size2 of
    	    W  -> SLIT("i\t")
    	    F  -> SLIT("s\t")
    	    DF -> SLIT("d\t")),
	pprReg reg1, comma, pprReg reg2
    ]


pprInstr (BI cond b lab)
  = hcat [
	ptext SLIT("\tb"), pprCond cond,
	if b then pp_comma_a else empty,
	char '\t',
	pprImm lab
    ]

pprInstr (BF cond b lab)
  = hcat [
	ptext SLIT("\tfb"), pprCond cond,
	if b then pp_comma_a else empty,
	char '\t',
	pprImm lab
    ]

pprInstr (JMP addr) = (<>) (ptext SLIT("\tjmp\t")) (pprAddr addr)

pprInstr (CALL imm n _)
  = hcat [ ptext SLIT("\tcall\t"), pprImm imm, comma, int n ]
\end{code}

Continue with SPARC-only printing bits and bobs:
\begin{code}
pprRI :: RI -> SDoc
pprRI (RIReg r) = pprReg r
pprRI (RIImm r) = pprImm r

pprSizeRegReg :: FAST_STRING -> Size -> Reg -> Reg -> SDoc
pprSizeRegReg name size reg1 reg2
  = hcat [
    	char '\t',
	ptext name,
    	(case size of
    	    F  -> ptext SLIT("s\t")
    	    DF -> ptext SLIT("d\t")),
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprSizeRegRegReg :: FAST_STRING -> Size -> Reg -> Reg -> Reg -> SDoc
pprSizeRegRegReg name size reg1 reg2 reg3
  = hcat [
    	char '\t',
	ptext name,
    	(case size of
    	    F  -> ptext SLIT("s\t")
    	    DF -> ptext SLIT("d\t")),
	pprReg reg1,
	comma,
	pprReg reg2,
	comma,
	pprReg reg3
    ]

pprRegRIReg :: FAST_STRING -> Bool -> Reg -> RI -> Reg -> SDoc
pprRegRIReg name b reg1 ri reg2
  = hcat [
	char '\t',
	ptext name,
	if b then ptext SLIT("cc\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprRIReg :: FAST_STRING -> Bool -> RI -> Reg -> SDoc
pprRIReg name b ri reg1
  = hcat [
	char '\t',
	ptext name,
	if b then ptext SLIT("cc\t") else char '\t',
	pprRI ri,
	comma,
	pprReg reg1
    ]

pp_ld_lbracket    = ptext SLIT("\tld\t[")
pp_rbracket_comma = text "],"
pp_comma_lbracket = text ",["
pp_comma_a	  = text ",a"

#endif {-sparc_TARGET_ARCH-}
\end{code}
