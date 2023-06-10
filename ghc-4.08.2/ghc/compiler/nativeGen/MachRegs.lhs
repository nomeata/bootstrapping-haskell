%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[MachRegs]{Machine-specific info about registers}

Also includes stuff about immediate operands, which are
often/usually quite entangled with registers.

(Immediates could be untangled from registers at some cost in tangled
modules --- the pleasure has been foregone.)

\begin{code}
#include "nativeGen/NCG.h"

module MachRegs (

        RegClass(..), regClass,
	Reg(..), isRealReg, isVirtualReg,
        allocatableRegs,

	Imm(..),
	MachRegsAddr(..),
	RegLoc(..),

	addrOffset,
	baseRegOffset,
	callerSaves,
	freeReg,
	getNewRegNCG,
	mkVReg,
	magicIdRegMaybe,
	saveLoc,
	spRel,
	stgReg,
	strImmLit

#if alpha_TARGET_ARCH
	, allArgRegs
	, fits8Bits
	, fReg
	, gp, pv, ra, sp, t9, t10, t11, t12, v0, f0, zeroh
#endif
#if i386_TARGET_ARCH
	, eax, ebx, ecx, edx, esi, esp
	, fake0, fake1, fake2, fake3, fake4, fake5
#endif
#if sparc_TARGET_ARCH
	, fits13Bits
	, fpRel, gReg, iReg, lReg, oReg, largeOffsetError
	, fp, g0, o0, f0
	
#endif
    ) where

#include "HsVersions.h"

import AbsCSyn		( MagicId(..) )
import AbsCUtils	( magicIdPrimRep )
import CLabel           ( CLabel, mkMainRegTableLabel )
import PrimOp		( PrimOp(..) )
import PrimRep		( PrimRep(..), isFloatingRep )
import Stix		( StixTree(..), StixReg(..),
                          getUniqueNat, returnNat, thenNat, NatM )
import Unique		( mkPseudoUnique2, Uniquable(..), Unique )
import Outputable
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

\begin{code}
data Imm
  = ImmInt	Int
  | ImmInteger	Integer	    -- Sigh.
  | ImmCLbl	CLabel	    -- AbstractC Label (with baggage)
  | ImmLab	Bool SDoc    -- Simple string label (underscore-able)
                             -- Bool==True ==> in a different DLL
  | ImmLit	SDoc    -- Simple string
  | ImmIndex    CLabel Int
  | ImmFloat	Rational
  | ImmDouble	Rational
  IF_ARCH_sparc(
  | LO Imm		    -- Possible restrictions...
  | HI Imm
  ,)
strImmLit s = ImmLit (text s)
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

\begin{code}
data MachRegsAddr
#if alpha_TARGET_ARCH
  = AddrImm	Imm
  | AddrReg	Reg
  | AddrRegImm	Reg Imm
#endif

#if i386_TARGET_ARCH
  = AddrBaseIndex	Base Index Displacement
  | ImmAddr		Imm Int

type Base         = Maybe Reg
type Index        = Maybe (Reg, Int)	-- Int is 2, 4 or 8
type Displacement = Imm
#endif

#if sparc_TARGET_ARCH
  = AddrRegReg	Reg Reg
  | AddrRegImm	Reg Imm
#endif

addrOffset :: MachRegsAddr -> Int -> Maybe MachRegsAddr

addrOffset addr off
  = case addr of
#if alpha_TARGET_ARCH
      _ -> panic "MachMisc.addrOffset not defined for Alpha"
#endif
#if i386_TARGET_ARCH
      ImmAddr i off0	  -> Just (ImmAddr i (off0 + off))
      AddrBaseIndex r i (ImmInt n) -> Just (AddrBaseIndex r i (ImmInt (n + off)))
      AddrBaseIndex r i (ImmInteger n)
	-> Just (AddrBaseIndex r i (ImmInt (fromInteger (n + toInteger off))))
      _ -> Nothing
#endif
#if sparc_TARGET_ARCH
      AddrRegImm r (ImmInt n)
       | fits13Bits n2 -> Just (AddrRegImm r (ImmInt n2))
       | otherwise     -> Nothing
       where n2 = n + off

      AddrRegImm r (ImmInteger n)
       | fits13Bits n2 -> Just (AddrRegImm r (ImmInt (fromInteger n2)))
       | otherwise     -> Nothing
       where n2 = n + toInteger off

      AddrRegReg r (RealReg 0)
       | fits13Bits off -> Just (AddrRegImm r (ImmInt off))
       | otherwise     -> Nothing
       
      _ -> Nothing

#endif {-sparc-}

-----------------
#if alpha_TARGET_ARCH

fits8Bits :: Integer -> Bool
fits8Bits i = i >= -256 && i < 256

#endif

#if sparc_TARGET_ARCH
{-# SPECIALIZE
    fits13Bits :: Int -> Bool
  #-}
{-# SPECIALIZE
    fits13Bits :: Integer -> Bool
  #-}

fits13Bits :: Integral a => a -> Bool
fits13Bits x = x >= -4096 && x < 4096

-----------------
largeOffsetError i
  = error ("ERROR: SPARC native-code generator cannot handle large offset ("
           ++show i++");\nprobably because of large constant data structures;" ++ 
           "\nworkaround: use -fvia-C on this module.\n")

#endif {-sparc-}
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@stgReg@: we map STG registers onto appropriate Stix Trees.  First, we
handle the two constants, @STK_STUB_closure@ and @vtbl_StdUpdFrame@.
The rest are either in real machine registers or stored as offsets
from BaseReg.

\begin{code}
data RegLoc = Save StixTree | Always StixTree
\end{code}

Trees for register save locations:
\begin{code}
saveLoc :: MagicId -> StixTree

saveLoc reg = case (stgReg reg) of {Always loc -> loc; Save loc -> loc}
\end{code}

\begin{code}
stgReg :: MagicId -> RegLoc

stgReg x
  = case (magicIdRegMaybe x) of
	Just _  -> Save   nonReg
	Nothing -> Always nonReg
  where
    offset = baseRegOffset x

    baseLoc = case (magicIdRegMaybe BaseReg) of
      Just _  -> StReg (StixMagicId BaseReg)
      Nothing -> StCLbl mkMainRegTableLabel

    nonReg = case x of
      BaseReg -> StCLbl mkMainRegTableLabel

      _ -> StInd (magicIdPrimRep x)
		 (StPrim IntAddOp [baseLoc,
			StInt (toInteger (offset*BYTES_PER_WORD))])
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@spRel@ gives us a stack relative addressing mode for volatile
temporaries and for excess call arguments.  @fpRel@, where
applicable, is the same but for the frame pointer.

\begin{code}
spRel :: Int	-- desired stack offset in words, positive or negative
      -> MachRegsAddr

spRel n
#if i386_TARGET_ARCH
  = AddrBaseIndex (Just esp) Nothing (ImmInt (n * BYTES_PER_WORD))
#else
  = AddrRegImm sp (ImmInt (n * BYTES_PER_WORD))
#endif

#if sparc_TARGET_ARCH
fpRel :: Int -> MachRegsAddr
    -- Duznae work for offsets greater than 13 bits; we just hope for
    -- the best
fpRel n
  = AddrRegImm fp (ImmInt (n * BYTES_PER_WORD))
#endif
\end{code}

%************************************************************************
%*									*
\subsection[Reg]{Real registers}
%*									*
%************************************************************************

RealRegs are machine regs which are available for allocation, in the
usual way.  We know what class they are, because that's part of the
processor's architecture.

VirtualRegs are virtual registers.  The register allocator will
eventually have to map them into RealRegs, or into spill slots.
VirtualRegs are allocated on the fly, usually to represent a single
value in the abstract assembly code (i.e. dynamic registers are
usually single assignment).  With the new register allocator, the
single assignment restriction isn't necessary to get correct code,
although a better register allocation will result if single assignment
is used -- because the allocator maps a VirtualReg into a single
RealReg, even if the VirtualReg has multiple live ranges.

Virtual regs can be of either class, so that info is attached.

\begin{code}

data RegClass 
   = RcInteger 
   | RcFloating
     deriving Eq

data Reg
   = RealReg     Int
   | VirtualRegI Unique
   | VirtualRegF Unique

mkVReg :: Unique -> PrimRep -> Reg
mkVReg u pk
   = if isFloatingRep pk then VirtualRegF u else VirtualRegI u

isVirtualReg (RealReg _)     = False
isVirtualReg (VirtualRegI _) = True
isVirtualReg (VirtualRegF _) = True
isRealReg = not . isVirtualReg

getNewRegNCG :: PrimRep -> NatM Reg
getNewRegNCG pk
   = if   isFloatingRep pk 
     then getUniqueNat `thenNat` \ u -> returnNat (VirtualRegF u)
     else getUniqueNat `thenNat` \ u -> returnNat (VirtualRegI u)

instance Eq Reg where
   (==) (RealReg i1)     (RealReg i2)     = i1 == i2
   (==) (VirtualRegI u1) (VirtualRegI u2) = u1 == u2
   (==) (VirtualRegF u1) (VirtualRegF u2) = u1 == u2
   (==) reg1             reg2             = False

instance Ord Reg where
   compare (RealReg i1)     (RealReg i2)     = compare i1 i2
   compare (RealReg _)      (VirtualRegI _)  = LT
   compare (RealReg _)      (VirtualRegF _)  = LT
   compare (VirtualRegI _)  (RealReg _)      = GT
   compare (VirtualRegI u1) (VirtualRegI u2) = compare u1 u2
   compare (VirtualRegI _)  (VirtualRegF _)  = LT
   compare (VirtualRegF _)  (RealReg _)      = GT
   compare (VirtualRegF _)  (VirtualRegI _)  = GT
   compare (VirtualRegF u1) (VirtualRegF u2) = compare u1 u2

instance Show Reg where
    showsPrec _ (RealReg i)     = showString (showReg i)
    showsPrec _ (VirtualRegI u) = showString "%vI_"  . shows u
    showsPrec _ (VirtualRegF u) = showString "%vF_"  . shows u

instance Outputable Reg where
    ppr r = text (show r)

instance Uniquable Reg where
    getUnique (RealReg i)     = mkPseudoUnique2 i
    getUnique (VirtualRegI u) = u
    getUnique (VirtualRegF u) = u
\end{code}

** Machine-specific Reg stuff: **

The Alpha has 64 registers of interest; 32 integer registers and 32 floating
point registers.  The mapping of STG registers to alpha machine registers
is defined in StgRegs.h.  We are, of course, prepared for any eventuality.
\begin{code}
#if alpha_TARGET_ARCH
fReg :: Int -> Int
fReg x = (32 + x)

v0, f0, ra, pv, gp, sp, zeroh :: Reg
v0    = realReg 0
f0    = realReg (fReg 0)
ra    = FixedReg ILIT(26)
pv    = t12
gp    = FixedReg ILIT(29)
sp    = FixedReg ILIT(30)
zeroh = FixedReg ILIT(31) -- "zero" is used in 1.3 (MonadZero method)

t9, t10, t11, t12 :: Reg
t9  = realReg 23
t10 = realReg 24
t11 = realReg 25
t12 = realReg 27
#endif
\end{code}

Intel x86 architecture:
- All registers except 7 (esp) are available for use.
- Only ebx, esi, edi and esp are available across a C call (they are callee-saves).
- Registers 0-7 have 16-bit counterparts (ax, bx etc.)
- Registers 0-3 have 8 bit counterparts (ah, bh etc.)
- Registers 8-13 are fakes; we pretend x86 has 6 conventionally-addressable
  fp registers, and 3-operand insns for them, and we translate this into
  real stack-based x86 fp code after register allocation.

\begin{code}
#if i386_TARGET_ARCH

fake0, fake1, fake2, fake3, fake4, fake5, 
       eax, ebx, ecx, edx, esp, ebp, esi, edi :: Reg
eax   = RealReg 0
ebx   = RealReg 1
ecx   = RealReg 2
edx   = RealReg 3
esi   = RealReg 4
edi   = RealReg 5
ebp   = RealReg 6
esp   = RealReg 7
fake0 = RealReg 8
fake1 = RealReg 9
fake2 = RealReg 10
fake3 = RealReg 11
fake4 = RealReg 12
fake5 = RealReg 13

regClass (RealReg i)     = if i < 8 then RcInteger else RcFloating
regClass (VirtualRegI u) = RcInteger
regClass (VirtualRegF u) = RcFloating

regNames 
   = ["%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%ebp", "%esp", 
      "%fake0", "%fake1", "%fake2", "%fake3", "%fake4", "%fake5", "%fake6"]

showReg :: Int -> String
showReg n
   = if   n >= 0 && n < 14
     then regNames !! n
     else "%unknown_x86_real_reg_" ++ show n

#endif
\end{code}

The SPARC has 64 registers of interest; 32 integer registers and 32
floating point registers.  The mapping of STG registers to SPARC
machine registers is defined in StgRegs.h.  We are, of course,
prepared for any eventuality.  When (if?) the sparc nativegen is 
ever revived, we should just treat it as if it has 16 floating
regs, and use them in pairs.  

\begin{code}
#if sparc_TARGET_ARCH

gReg,lReg,iReg,oReg,fReg :: Int -> Int
gReg x = x
oReg x = (8 + x)
lReg x = (16 + x)
iReg x = (24 + x)
fReg x = (32 + x)

-- CHECK THIS
regClass (RealReg i)     = if i < 32 then RcInteger else RcFloating
regClass (VirtualRegI u) = RcInteger
regClass (VirtualRegF u) = RcFloating

-- FIX THIS
showReg :: Int -> String
showReg n
   = if   n >= 0 && n < 64
     then "%sparc_real_reg_" ++ show n
     else "%unknown_sparc_real_reg_" ++ show n

g0, fp, sp, o0, f0 :: Reg
g0 = RealReg (gReg 0)
fp = RealReg (iReg 6)
sp = RealReg (oReg 6)
o0 = RealReg (oReg 0)
f0 = RealReg (fReg 0)

#endif
\end{code}

Redefine the literals used for machine-registers with non-numeric
names in the header files.  Gag me with a spoon, eh?
\begin{code}
#if alpha_TARGET_ARCH
#define f0 32
#define f1 33
#define f2 34
#define f3 35
#define f4 36
#define f5 37
#define f6 38
#define f7 39
#define f8 40
#define f9 41
#define f10 42
#define f11 43
#define f12 44
#define f13 45
#define f14 46
#define f15 47
#define f16 48
#define f17 49
#define f18 50
#define f19 51
#define f20 52
#define f21 53
#define f22 54
#define f23 55
#define f24 56
#define f25 57
#define f26 58
#define f27 59
#define f28 60
#define f29 61
#define f30 62
#define f31 63
#endif
#if i386_TARGET_ARCH
#define eax 0
#define ebx 1
#define ecx 2
#define edx 3
#define esi 4
#define edi 5
#define ebp 6
#define esp 7
#define fake0 8
#define fake1 9
#define fake2 10
#define fake3 11
#define fake4 12
#define fake5 13
#endif
#if sparc_TARGET_ARCH
#define g0 0
#define g1 1
#define g2 2
#define g3 3
#define g4 4
#define g5 5
#define g6 6
#define g7 7
#define o0 8
#define o1 9
#define o2 10
#define o3 11
#define o4 12
#define o5 13
#define o6 14
#define o7 15
#define l0 16
#define l1 17
#define l2 18
#define l3 19
#define l4 20
#define l5 21
#define l6 22
#define l7 23
#define i0 24
#define i1 25
#define i2 26
#define i3 27
#define i4 28
#define i5 29
#define i6 30
#define i7 31
#define f0 32
#define f1 33
#define f2 34
#define f3 35
#define f4 36
#define f5 37
#define f6 38
#define f7 39
#define f8 40
#define f9 41
#define f10 42
#define f11 43
#define f12 44
#define f13 45
#define f14 46
#define f15 47
#define f16 48
#define f17 49
#define f18 50
#define f19 51
#define f20 52
#define f21 53
#define f22 54
#define f23 55
#define f24 56
#define f25 57
#define f26 58
#define f27 59
#define f28 60
#define f29 61
#define f30 62
#define f31 63
#endif
\end{code}

\begin{code}
baseRegOffset :: MagicId -> Int

baseRegOffset (VanillaReg _ ILIT(1)) = OFFSET_R1
baseRegOffset (VanillaReg _ ILIT(2)) = OFFSET_R2
baseRegOffset (VanillaReg _ ILIT(3)) = OFFSET_R3
baseRegOffset (VanillaReg _ ILIT(4)) = OFFSET_R4
baseRegOffset (VanillaReg _ ILIT(5)) = OFFSET_R5
baseRegOffset (VanillaReg _ ILIT(6)) = OFFSET_R6
baseRegOffset (VanillaReg _ ILIT(7)) = OFFSET_R7
baseRegOffset (VanillaReg _ ILIT(8)) = OFFSET_R8
baseRegOffset (VanillaReg _ ILIT(9)) = OFFSET_R9
baseRegOffset (VanillaReg _ ILIT(10)) = OFFSET_R10
baseRegOffset (FloatReg  ILIT(1))    = OFFSET_F1
baseRegOffset (FloatReg  ILIT(2))    = OFFSET_F2
baseRegOffset (FloatReg  ILIT(3))    = OFFSET_F3
baseRegOffset (FloatReg  ILIT(4))    = OFFSET_F4
baseRegOffset (DoubleReg ILIT(1))    = OFFSET_D1
baseRegOffset (DoubleReg ILIT(2))    = OFFSET_D2
baseRegOffset Sp		     = OFFSET_Sp
baseRegOffset Su		     = OFFSET_Su
baseRegOffset SpLim		     = OFFSET_SpLim
#ifdef OFFSET_Lng1
baseRegOffset (LongReg _ ILIT(1))    = OFFSET_Lng1
#endif
#ifdef OFFSET_Lng2
baseRegOffset (LongReg _ ILIT(2))    = OFFSET_Lng2
#endif
baseRegOffset Hp		     = OFFSET_Hp
baseRegOffset HpLim		     = OFFSET_HpLim
baseRegOffset CurrentTSO	     = OFFSET_CurrentTSO
baseRegOffset CurrentNursery	     = OFFSET_CurrentNursery
#ifdef DEBUG
baseRegOffset BaseReg		     = panic "baseRegOffset:BaseReg"
baseRegOffset CurCostCentre	     = panic "baseRegOffset:CurCostCentre"
baseRegOffset VoidReg		     = panic "baseRegOffset:VoidReg"
#endif
\end{code}

\begin{code}
callerSaves :: MagicId -> Bool

#ifdef CALLER_SAVES_Base
callerSaves BaseReg			= True
#endif
#ifdef CALLER_SAVES_R1
callerSaves (VanillaReg _ ILIT(1))	= True
#endif
#ifdef CALLER_SAVES_R2
callerSaves (VanillaReg _ ILIT(2))    	= True
#endif
#ifdef CALLER_SAVES_R3
callerSaves (VanillaReg _ ILIT(3))    	= True
#endif
#ifdef CALLER_SAVES_R4
callerSaves (VanillaReg _ ILIT(4))	= True
#endif
#ifdef CALLER_SAVES_R5
callerSaves (VanillaReg _ ILIT(5))	= True
#endif
#ifdef CALLER_SAVES_R6
callerSaves (VanillaReg _ ILIT(6))	= True
#endif
#ifdef CALLER_SAVES_R7
callerSaves (VanillaReg _ ILIT(7))	= True
#endif
#ifdef CALLER_SAVES_R8
callerSaves (VanillaReg _ ILIT(8))	= True
#endif
#ifdef CALLER_SAVES_F1
callerSaves (FloatReg ILIT(1))		= True
#endif
#ifdef CALLER_SAVES_F2
callerSaves (FloatReg ILIT(2))		= True
#endif
#ifdef CALLER_SAVES_F3
callerSaves (FloatReg ILIT(3))		= True
#endif
#ifdef CALLER_SAVES_F4
callerSaves (FloatReg ILIT(4))		= True
#endif
#ifdef CALLER_SAVES_D1
callerSaves (DoubleReg ILIT(1))		= True
#endif
#ifdef CALLER_SAVES_D2
callerSaves (DoubleReg ILIT(2))		= True
#endif
#ifdef CALLER_SAVES_L1
callerSaves (LongReg _ ILIT(1))		= True
#endif
#ifdef CALLER_SAVES_Sp
callerSaves Sp				= True
#endif
#ifdef CALLER_SAVES_Su
callerSaves Su				= True
#endif
#ifdef CALLER_SAVES_SpLim
callerSaves SpLim			= True
#endif
#ifdef CALLER_SAVES_Hp
callerSaves Hp				= True
#endif
#ifdef CALLER_SAVES_HpLim
callerSaves HpLim			= True
#endif
#ifdef CALLER_SAVES_CurrentTSO
callerSaves CurrentTSO			= True
#endif
#ifdef CALLER_SAVES_CurrentNursery
callerSaves CurrentNursery		= True
#endif
callerSaves _				= False
\end{code}

\begin{code}
magicIdRegMaybe :: MagicId -> Maybe Reg

#ifdef REG_Base
magicIdRegMaybe BaseReg			= Just (RealReg REG_Base)
#endif
#ifdef REG_R1
magicIdRegMaybe (VanillaReg _ ILIT(1)) 	= Just (RealReg REG_R1)
#endif 
#ifdef REG_R2 
magicIdRegMaybe (VanillaReg _ ILIT(2)) 	= Just (RealReg REG_R2)
#endif 
#ifdef REG_R3 
magicIdRegMaybe (VanillaReg _ ILIT(3)) 	= Just (RealReg REG_R3)
#endif 
#ifdef REG_R4 
magicIdRegMaybe (VanillaReg _ ILIT(4)) 	= Just (RealReg REG_R4)
#endif 
#ifdef REG_R5 
magicIdRegMaybe (VanillaReg _ ILIT(5)) 	= Just (RealReg REG_R5)
#endif 
#ifdef REG_R6 
magicIdRegMaybe (VanillaReg _ ILIT(6)) 	= Just (RealReg REG_R6)
#endif 
#ifdef REG_R7 
magicIdRegMaybe (VanillaReg _ ILIT(7)) 	= Just (RealReg REG_R7)
#endif 
#ifdef REG_R8 
magicIdRegMaybe (VanillaReg _ ILIT(8)) 	= Just (RealReg REG_R8)
#endif
#ifdef REG_R9 
magicIdRegMaybe (VanillaReg _ ILIT(9)) 	= Just (RealReg REG_R9)
#endif
#ifdef REG_R10 
magicIdRegMaybe (VanillaReg _ ILIT(10))	= Just (RealReg REG_R10)
#endif
#ifdef REG_F1
magicIdRegMaybe (FloatReg ILIT(1))	= Just (RealReg REG_F1)
#endif				 	
#ifdef REG_F2			 	
magicIdRegMaybe (FloatReg ILIT(2))	= Just (RealReg REG_F2)
#endif				 	
#ifdef REG_F3			 	
magicIdRegMaybe (FloatReg ILIT(3))	= Just (RealReg REG_F3)
#endif				 	
#ifdef REG_F4			 	
magicIdRegMaybe (FloatReg ILIT(4))	= Just (RealReg REG_F4)
#endif				 	
#ifdef REG_D1			 	
magicIdRegMaybe (DoubleReg ILIT(1))	= Just (RealReg REG_D1)
#endif				 	
#ifdef REG_D2			 	
magicIdRegMaybe (DoubleReg ILIT(2))	= Just (RealReg REG_D2)
#endif
#ifdef REG_Sp	    
magicIdRegMaybe Sp		   	= Just (RealReg REG_Sp)
#endif
#ifdef REG_Lng1			 	
magicIdRegMaybe (LongReg _ ILIT(1))	= Just (RealReg REG_Lng1)
#endif				 	
#ifdef REG_Lng2			 	
magicIdRegMaybe (LongReg _ ILIT(2))	= Just (RealReg REG_Lng2)
#endif
#ifdef REG_Su	    			
magicIdRegMaybe Su		   	= Just (RealReg REG_Su)
#endif	    				
#ifdef REG_SpLim	    			
magicIdRegMaybe SpLim		   	= Just (RealReg REG_SpLim)
#endif	    				
#ifdef REG_Hp	   			
magicIdRegMaybe Hp		   	= Just (RealReg REG_Hp)
#endif	    				
#ifdef REG_HpLim      			
magicIdRegMaybe HpLim		   	= Just (RealReg REG_HpLim)
#endif	    				
#ifdef REG_CurrentTSO      			
magicIdRegMaybe CurrentTSO	   	= Just (RealReg REG_CurrentTSO)
#endif	    				
#ifdef REG_CurrentNursery      			
magicIdRegMaybe CurrentNursery	   	= Just (RealReg REG_CurrentNursery)
#endif	    				
magicIdRegMaybe _		   	= Nothing
\end{code}

\begin{code}
-------------------------------
#if 0
freeRegs :: [Reg]
freeRegs
  = freeMappedRegs IF_ARCH_alpha( [0..63],
		   IF_ARCH_i386(  [0..13],
		   IF_ARCH_sparc( [0..63],)))
#endif
-- allMachRegs is the complete set of machine regs.
allMachRegNos :: [Int]
allMachRegNos
   = IF_ARCH_alpha( [0..63],
     IF_ARCH_i386(  [0..13],
     IF_ARCH_sparc( [0..63],)))
-- allocatableRegs is allMachRegNos with the fixed-use regs removed.
allocatableRegs :: [Reg]
allocatableRegs
   = let isFree (RealReg (I# i)) = _IS_TRUE_(freeReg i)
     in  filter isFree (map RealReg allMachRegNos)


-------------------------------
#if 0
callClobberedRegs :: [Reg]
callClobberedRegs
  = freeMappedRegs
#if alpha_TARGET_ARCH
    [0, 1, 2, 3, 4, 5, 6, 7, 8,
     16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
     fReg 0, fReg 1, fReg 10, fReg 11, fReg 12, fReg 13, fReg 14, fReg 15,
     fReg 16, fReg 17, fReg 18, fReg 19, fReg 20, fReg 21, fReg 22, fReg 23,
     fReg 24, fReg 25, fReg 26, fReg 27, fReg 28, fReg 29, fReg 30]
#endif {- alpha_TARGET_ARCH -}
#if i386_TARGET_ARCH
    [{-none-}]
#endif {- i386_TARGET_ARCH -}
#if sparc_TARGET_ARCH
    ( oReg 7 :
      [oReg i | i <- [0..5]] ++
      [gReg i | i <- [1..7]] ++
      [fReg i | i <- [0..31]] )
#endif {- sparc_TARGET_ARCH -}
#endif

-------------------------------
#if 0
argRegs :: Int -> [Reg]

argRegs 0 = []
#if i386_TARGET_ARCH
argRegs _ = panic "MachRegs.argRegs: doesn't work on I386"
#else
#if alpha_TARGET_ARCH
argRegs 1 = freeMappedRegs [16, fReg 16]
argRegs 2 = freeMappedRegs [16, 17, fReg 16, fReg 17]
argRegs 3 = freeMappedRegs [16, 17, 18, fReg 16, fReg 17, fReg 18]
argRegs 4 = freeMappedRegs [16, 17, 18, 19, fReg 16, fReg 17, fReg 18, fReg 19]
argRegs 5 = freeMappedRegs [16, 17, 18, 19, 20, fReg 16, fReg 17, fReg 18, fReg 19, fReg 20]
argRegs 6 = freeMappedRegs [16, 17, 18, 19, 20, 21, fReg 16, fReg 17, fReg 18, fReg 19, fReg 20, fReg 21]
#endif {- alpha_TARGET_ARCH -}
#if sparc_TARGET_ARCH
argRegs 1 = freeMappedRegs (map oReg [0])
argRegs 2 = freeMappedRegs (map oReg [0,1])
argRegs 3 = freeMappedRegs (map oReg [0,1,2])
argRegs 4 = freeMappedRegs (map oReg [0,1,2,3])
argRegs 5 = freeMappedRegs (map oReg [0,1,2,3,4])
argRegs 6 = freeMappedRegs (map oReg [0,1,2,3,4,5])
#endif {- sparc_TARGET_ARCH -}
argRegs _ = panic "MachRegs.argRegs: don't know about >6 arguments!"
#endif {- i386_TARGET_ARCH -}
#endif

-------------------------------

#if 0
#if alpha_TARGET_ARCH
allArgRegs :: [(Reg, Reg)]

allArgRegs = [(realReg i, realReg (fReg i)) | i <- [16..21]]
#endif {- alpha_TARGET_ARCH -}

#if sparc_TARGET_ARCH
allArgRegs :: [Reg]

allArgRegs = map realReg [oReg i | i <- [0..5]]
#endif {- sparc_TARGET_ARCH -}
#endif
\end{code}

\begin{code}
freeReg :: FAST_INT -> FAST_BOOL

#if alpha_TARGET_ARCH
freeReg ILIT(26) = _FALSE_  -- return address (ra)
freeReg ILIT(28) = _FALSE_  -- reserved for the assembler (at)
freeReg ILIT(29) = _FALSE_  -- global pointer (gp)
freeReg ILIT(30) = _FALSE_  -- stack pointer (sp)
freeReg ILIT(31) = _FALSE_  -- always zero (zeroh)
freeReg ILIT(63) = _FALSE_  -- always zero (f31)
#endif

#if i386_TARGET_ARCH
freeReg ILIT(esp) = _FALSE_  --	%esp is the C stack pointer
#endif

#if sparc_TARGET_ARCH
freeReg ILIT(g0) = _FALSE_  --	%g0 is always 0.
freeReg ILIT(g5) = _FALSE_  --	%g5 is reserved (ABI).
freeReg ILIT(g6) = _FALSE_  --	%g6 is reserved (ABI).
freeReg ILIT(g7) = _FALSE_  --	%g7 is reserved (ABI).
freeReg ILIT(i6) = _FALSE_  --	%i6 is our frame pointer.
freeReg ILIT(o6) = _FALSE_  --	%o6 is our stack pointer.
#endif

#ifdef REG_Base
freeReg ILIT(REG_Base) = _FALSE_
#endif
#ifdef REG_R1
freeReg ILIT(REG_R1)   = _FALSE_
#endif	
#ifdef REG_R2  
freeReg ILIT(REG_R2)   = _FALSE_
#endif	
#ifdef REG_R3  
freeReg ILIT(REG_R3)   = _FALSE_
#endif	
#ifdef REG_R4  
freeReg ILIT(REG_R4)   = _FALSE_
#endif	
#ifdef REG_R5  
freeReg ILIT(REG_R5)   = _FALSE_
#endif	
#ifdef REG_R6  
freeReg ILIT(REG_R6)   = _FALSE_
#endif	
#ifdef REG_R7  
freeReg ILIT(REG_R7)   = _FALSE_
#endif	
#ifdef REG_R8  
freeReg ILIT(REG_R8)   = _FALSE_
#endif
#ifdef REG_F1
freeReg ILIT(REG_F1) = _FALSE_
#endif
#ifdef REG_F2
freeReg ILIT(REG_F2) = _FALSE_
#endif
#ifdef REG_F3
freeReg ILIT(REG_F3) = _FALSE_
#endif
#ifdef REG_F4
freeReg ILIT(REG_F4) = _FALSE_
#endif
#ifdef REG_D1
freeReg ILIT(REG_D1) = _FALSE_
#endif
#ifdef REG_D2
freeReg ILIT(REG_D2) = _FALSE_
#endif
#ifdef REG_Sp 
freeReg ILIT(REG_Sp)   = _FALSE_
#endif 
#ifdef REG_Su
freeReg ILIT(REG_Su)   = _FALSE_
#endif 
#ifdef REG_SpLim 
freeReg ILIT(REG_SpLim) = _FALSE_
#endif 
#ifdef REG_Hp 
freeReg ILIT(REG_Hp)   = _FALSE_
#endif
#ifdef REG_HpLim
freeReg ILIT(REG_HpLim) = _FALSE_
#endif
freeReg n
  -- we hang onto two double regs for dedicated
  -- use; this is not necessary on Alphas and
  -- may not be on other non-SPARCs.
#ifdef REG_D1
  | n _EQ_ (ILIT(REG_D1) _ADD_ ILIT(1)) = _FALSE_
#endif
#ifdef REG_D2
  | n _EQ_ (ILIT(REG_D2) _ADD_ ILIT(1)) = _FALSE_
#endif
  | otherwise = _TRUE_
\end{code}
