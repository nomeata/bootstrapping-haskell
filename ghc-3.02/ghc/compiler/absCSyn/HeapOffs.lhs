%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HeapOffs]{Abstract C: heap offsets}

Part of ``Abstract C.''  Heap offsets---main point: they are {\em
symbolic}---are sufficiently turgid that they get their own module.

INTERNAL MODULE: should be accessed via @AbsCSyn.hi@.

\begin{code}
module HeapOffs (
	HeapOffset,

	zeroOff, intOff, fixedHdrSize, totHdrSize, varHdrSize,
	maxOff, addOff, subOff,
	isZeroOff, possiblyEqualHeapOffset,

	pprHeapOffset,

	intOffsetIntoGoods,

#if ! OMIT_NATIVE_CODEGEN
	hpRelToInt,
#endif

	VirtualHeapOffset, HpRelOffset,
	VirtualSpAOffset, VirtualSpBOffset,
	SpARelOffset, SpBRelOffset
    ) where

#include "HsVersions.h"

#if ! OMIT_NATIVE_CODEGEN
import {-# SOURCE #-} MachMisc
#endif

import Maybes		( catMaybes )
import SMRep
import Util		( panic )
import Outputable
import GlaExts		( Int(..), Int#, (+#), negateInt#, (<=#), (>=#), (==#) )
\end{code}

%************************************************************************
%*									*
\subsection[Offsets-Heap-and-others]{Offsets, Heap and otherwise}
%*									*
%************************************************************************

\begin{code}
{-
    < fixed-hdr-size> < var-hdr-size  >
    ---------------------------------------------------------------------
    |info|    |      |   |  |  |   |  | ptrs... | nonptrs ... | slop.... |
    ---------------------------------------------------------------------
    <------------- header ------------>

    * Node, the ptr to the closure, pts at its info-ptr field
-}
data HeapOffset
  = MkHeapOffset

	FAST_INT	-- this many words...

	FAST_INT	-- PLUS: this many FixedHdrSizes

	[SMRep__Int]	-- PLUS: for each elem in this list:
			--	"Int" VarHdrSizes for rep "SMRep"
			-- *sorted* by SMRep
			-- We never have any SpecReps in here, because their
			-- 	VarHdrSize is zero

	[SMRep__Int]	-- PLUS: for each elem in this list:
			--	"Int" TotHdrSizes for rep "SMRep"
			-- *sorted* by SMRep
			-- We never have any SpecReps in here, because
			--	their TotHdrSize is just FixedHdrSize

  | MaxHeapOffset HeapOffset HeapOffset
  | SubHeapOffset HeapOffset HeapOffset
  | AddHeapOffset HeapOffset HeapOffset
  | ZeroHeapOffset

  deriving () -- but: see `eqOff` below

data SMRep__Int = SMRI_ SMRep Int#
#define SMRI(a,b) (SMRI_ a b)

type VirtualHeapOffset	= HeapOffset
type VirtualSpAOffset	= Int
type VirtualSpBOffset	= Int

type HpRelOffset	= HeapOffset
type SpARelOffset	= Int
type SpBRelOffset	= Int
\end{code}

Interface fns for HeapOffsets:
\begin{code}
zeroOff = ZeroHeapOffset

intOff IBOX(n) = MkHeapOffset n ILIT(0) [] []

fixedHdrSize = MkHeapOffset ILIT(0) ILIT(1) [] []

totHdrSize sm_rep
  = if isSpecRep sm_rep -- Tot hdr size for a spec rep is just FixedHdrSize
    then MkHeapOffset ILIT(0) ILIT(1) [] []
    else MkHeapOffset ILIT(0) ILIT(0) [] [SMRI(sm_rep, ILIT(1))]

varHdrSize sm_rep
  = if isSpecRep sm_rep
    then zeroOff
    else MkHeapOffset ILIT(0) ILIT(0) [SMRI(sm_rep, ILIT(1))] []
\end{code}

%************************************************************************
%*									*
\subsubsection[Heap-offset-arithmetic]{Heap offset arithmetic}
%*									*
%************************************************************************

\begin{code}
-- For maxOff we do our best when we have something simple to deal with
maxOff ZeroHeapOffset off2 = off2
maxOff off1 ZeroHeapOffset = off1
maxOff off1@(MkHeapOffset int_offs1 fixhdr_offs1 varhdr_offs1 tothdr_offs1)
       off2@(MkHeapOffset int_offs2 fixhdr_offs2 varhdr_offs2 tothdr_offs2)
  = if (int_offs1 _LE_ int_offs2) &&
       (real_fixed1 _LE_ real_fixed2) &&
       (all negative_or_zero difference_of_real_varhdrs)
    then
	 off2
    else
    if (int_offs2 _LE_ int_offs1) &&
       (real_fixed2 _LE_ real_fixed1) &&
       (all positive_or_zero difference_of_real_varhdrs)
    then
	 off1
    else
	 MaxHeapOffset off1 off2
  where
    -- Normalise, by realising that each tot-hdr is really a
    -- var-hdr plus a fixed-hdr
    n_tothdr1    = total_of tothdr_offs1
    real_fixed1  = fixhdr_offs1 _ADD_ n_tothdr1
    real_varhdr1 = add_HdrSizes varhdr_offs1 tothdr_offs1

    n_tothdr2    = total_of tothdr_offs2
    real_fixed2  = fixhdr_offs2 _ADD_ n_tothdr2
    real_varhdr2 = add_HdrSizes varhdr_offs2 tothdr_offs2

    -- Take the difference of the normalised var-hdrs
    difference_of_real_varhdrs
      = add_HdrSizes real_varhdr1 (map negate_HdrSize real_varhdr2)
      where
	negate_HdrSize :: SMRep__Int -> SMRep__Int
	negate_HdrSize SMRI(rep,n) = SMRI(rep, (_NEG_ n))

    positive_or_zero SMRI(rep,n) = n _GE_ ILIT(0)
    negative_or_zero SMRI(rep,n) = n _LE_ ILIT(0)

    total_of []			= ILIT(0)
    total_of (SMRI(rep,n):offs) = n _ADD_ total_of offs

maxOff other_off1 other_off2 = MaxHeapOffset other_off1 other_off2

------------------------------------------------------------------

subOff off1 ZeroHeapOffset = off1
subOff off1
       (MkHeapOffset int_offs2 fxdhdr_offs2 varhdr_offs2 tothdr_offs2)
  = addOff off1
	  (MkHeapOffset (_NEG_ int_offs2)
			(_NEG_ fxdhdr_offs2)
			(map negate_HdrSize varhdr_offs2)
			(map negate_HdrSize tothdr_offs2))
  where
    negate_HdrSize :: SMRep__Int -> SMRep__Int
    negate_HdrSize SMRI(rep,n) = SMRI(rep,(_NEG_ n))

subOff other_off1 other_off2 = SubHeapOffset other_off1 other_off2

------------------------------------------------------------------

addOff ZeroHeapOffset off2 = off2
addOff off1 ZeroHeapOffset = off1
addOff (MkHeapOffset int_offs1 fxdhdr_offs1 varhdr_offs1 tothdr_offs1)
       (MkHeapOffset int_offs2 fxdhdr_offs2 varhdr_offs2 tothdr_offs2)
  = MkHeapOffset
	(int_offs1    _ADD_ int_offs2)
	(fxdhdr_offs1 _ADD_ fxdhdr_offs2)
	(add_HdrSizes varhdr_offs1 varhdr_offs2)
	(add_HdrSizes tothdr_offs1 tothdr_offs2)

addOff other_off1 other_off2 = AddHeapOffset other_off1 other_off2

------------------------------------------------------------------
-- not exported:
--
add_HdrSizes :: [SMRep__Int] -> [SMRep__Int] -> [SMRep__Int]

add_HdrSizes [] offs2 = offs2
add_HdrSizes offs1 [] = offs1
add_HdrSizes as@(off1@(SMRI(rep1,n1)) : offs1) bs@(off2@(SMRI(rep2,n2)) : offs2)
  = if rep1 `ltSMRepHdr` rep2 then
	     off1 : (add_HdrSizes offs1 bs)
    else
    if rep2 `ltSMRepHdr` rep1 then
	     off2 : (add_HdrSizes as offs2)
    else
    let
	n1_plus_n2 = n1 _ADD_ n2
    in
    -- So they are the same rep
    if n1_plus_n2 _EQ_ ILIT(0) then
	add_HdrSizes offs1 offs2
    else
	(SMRI(rep1, n1_plus_n2)) : (add_HdrSizes offs1 offs2)
\end{code}

\begin{code}
isZeroOff :: HeapOffset -> Bool
isZeroOff ZeroHeapOffset = True
isZeroOff (MaxHeapOffset off1 off2) = isZeroOff off1 && isZeroOff off2

isZeroOff (AddHeapOffset off1 off2) = isZeroOff off1 && isZeroOff off2
	-- This assumes that AddHeapOffset only has positive arguments

isZeroOff (MkHeapOffset int_offs fxdhdr_offs varhdr_offs tothdr_offs)
  = int_offs _EQ_ ILIT(0) && fxdhdr_offs _EQ_ ILIT(0) &&
    null varhdr_offs && null tothdr_offs

isZeroOff (SubHeapOffset off1 off2) = panic "Can't say if a SubHeapOffset is zero"
\end{code}

@possiblyEqualHeapOffset@ tells if two heap offsets might be equal.
It has to be conservative, but the situation in which it is used
(@doSimultaneously@) makes it likely to give a good answer.

\begin{code}
possiblyEqualHeapOffset :: HeapOffset -> HeapOffset -> Bool
possiblyEqualHeapOffset o1 o2
 = case (o1 `subOff` o2) of

	SubHeapOffset _ _ -> True			-- Very conservative

	diff		  -> not (isZeroOff diff)	-- Won't be any SubHeapOffsets in diff
							-- NB: this claim depends on the use of
							-- heap offsets, so this defn might need
							-- to be elaborated.

\end{code}

%************************************************************************
%*									*
\subsection[HeapOffs-printing]{Printing heap offsets}
%*									*
%************************************************************************

IMPORTANT: @pprHeapOffset@ and @pprHeapOffsetPieces@ guarantee to
print either a single value, or a parenthesised value.  No need for
the caller to parenthesise.

\begin{code}
pprHeapOffset :: HeapOffset -> SDoc

pprHeapOffset ZeroHeapOffset = char '0'

pprHeapOffset (MaxHeapOffset off1 off2)
  = (<>) (ptext SLIT("STG_MAX"))
      (parens (hcat [pprHeapOffset off1, comma, pprHeapOffset off2]))

pprHeapOffset (AddHeapOffset off1 off2)
  = parens (hcat [pprHeapOffset off1, char '+',
			pprHeapOffset off2])
pprHeapOffset (SubHeapOffset off1 off2)
  = parens (hcat [pprHeapOffset off1, char '-',
			pprHeapOffset off2])

pprHeapOffset (MkHeapOffset int_offs fxdhdr_offs varhdr_offs tothdr_offs)
  = pprHeapOffsetPieces int_offs fxdhdr_offs varhdr_offs tothdr_offs
\end{code}

\begin{code}
pprHeapOffsetPieces :: FAST_INT		-- Words
		    -> FAST_INT		-- Fixed hdrs
		    -> [SMRep__Int]	-- Var hdrs
		    -> [SMRep__Int]	-- Tot hdrs
		    -> SDoc

pprHeapOffsetPieces n ILIT(0) [] [] = int IBOX(n) -- Deals with zero case too

pprHeapOffsetPieces int_offs fxdhdr_offs varhdr_offs tothdr_offs
  = let pp_int_offs =
	    if int_offs _EQ_ ILIT(0)
	    then Nothing
	    else Just (int IBOX(int_offs))

	pp_fxdhdr_offs =
	    if fxdhdr_offs _EQ_ ILIT(0) then
		Nothing
	    else if fxdhdr_offs _EQ_ ILIT(1) then
		Just (ptext SLIT("_FHS"))
	    else
		Just (hcat [char '(', ptext SLIT("_FHS*"), int IBOX(fxdhdr_offs), char ')'])

	pp_varhdr_offs = pp_hdrs (ptext SLIT("_VHS")) varhdr_offs

	pp_tothdr_offs = pp_hdrs (ptext SLIT("_HS")) tothdr_offs
    in
    case (catMaybes [pp_tothdr_offs, pp_varhdr_offs, pp_fxdhdr_offs, pp_int_offs]) of
	[]   -> char '0'
	[pp] -> pp	-- Each blob is parenthesised if necessary
	pps  -> parens (hcat (punctuate (char '+') pps))
  where
    pp_hdrs hdr_pp [] = Nothing
    pp_hdrs hdr_pp [SMRI(rep, n)] | n _EQ_ ILIT(1) = Just ((<>) (text (show rep)) hdr_pp)
    pp_hdrs hdr_pp hdrs = Just (parens (hsep (punctuate (char '+')
						(map (pp_hdr hdr_pp) hdrs))))

    pp_hdr :: SDoc -> SMRep__Int -> SDoc
    pp_hdr pp_str (SMRI(rep, n))
      = if n _EQ_ ILIT(1) then
	  (<>) (text (show rep)) pp_str
	else
	  hcat [int IBOX(n), char '*', text (show rep), pp_str]
\end{code}

%************************************************************************
%*									*
\subsection[HeapOffs-conversion]{Converting heap offsets to words}
%*									*
%************************************************************************

@intOffsetIntoGoods@ and @hpRelToInt@ convert HeapOffsets into Ints.

@intOffsetIntoGoods@ {\em tries} to convert a HeapOffset in a SPEC
closure into an Int, returning the (0-origin) index from the beginning
of the ``goods'' in the closure.  [SPECs don't have VHSs, by
definition, so the index is merely ignoring the FHS].

@hpRelToInt@ is for the native code-generator(s); it is courtesy of
Jon Hill and the DAP code generator.  We've just abstracted away some
of the implementation-dependent bits.

\begin{code}
intOffsetIntoGoods :: HeapOffset -> Maybe Int

intOffsetIntoGoods (MkHeapOffset n ILIT(1){-FHS-} [{-no VHSs-}] [{-no totHSs-}])
  = Just IBOX(n)
intOffsetIntoGoods anything_else = Nothing
\end{code}

\begin{code}
#if ! OMIT_NATIVE_CODEGEN

hpRelToInt :: HeapOffset -> Int

hpRelToInt ZeroHeapOffset = 0

hpRelToInt (MaxHeapOffset left right)
  = hpRelToInt left `max` hpRelToInt right

hpRelToInt (SubHeapOffset left right)
  = hpRelToInt left - hpRelToInt right

hpRelToInt (AddHeapOffset left right)
  = hpRelToInt left + hpRelToInt right

hpRelToInt (MkHeapOffset base fhs vhs ths)
  = let
	vhs_pieces, ths_pieces :: [Int]
	fhs_off, vhs_off, ths_off :: Int

	vhs_pieces = map (\ (SMRI(r, n)) -> vhs_size r * IBOX(n)) vhs
	ths_pieces = map (\ (SMRI(r, n)) -> (fhs_size + vhs_size r) * IBOX(n)) ths

	fhs_off = fhs_size * IBOX(fhs)
	vhs_off = sum vhs_pieces
	ths_off = sum ths_pieces
    in
    IBOX(base) + fhs_off + vhs_off + ths_off
  where
    fhs_size   = fixedHdrSizeInWords
    vhs_size r = varHdrSizeInWords r

#endif
\end{code}
