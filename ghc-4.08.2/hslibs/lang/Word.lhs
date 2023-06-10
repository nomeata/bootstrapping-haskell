%
% (c) The AQUA Project, Glasgow University, 1997
%
\section[Word]{Module @Word@}

GHC implementation of the standard Hugs/GHC @Word@
interface, types and operations over unsigned, sized
quantities.

\begin{code}
#include "MachDeps.h"

module Word 
	( Word8
	, Word16
	, Word32
	, Word64
	-- instances: Eq, Ord, Num, Bounded, Real, Integral, Ix, Enum, Read,
	-- Show, Bits, CCallable, CReturnable (last two are GHC specific.)

        -- SUP: deprecated in the new FFI, subsumed by fromIntegral
	, intToWord8      -- :: Int     -> Word8
	, intToWord16     -- :: Int     -> Word16
	, intToWord32     -- :: Int     -> Word32
	, intToWord64     -- :: Int     -> Word64

	, integerToWord8  -- :: Integer -> Word8
	, integerToWord16 -- :: Integer -> Word16
	, integerToWord32 -- :: Integer -> Word32
	, integerToWord64 -- :: Integer -> Word64

	, word8ToInt      -- :: Word8   -> Int
        , word8ToInteger  -- :: Word8   -> Integer
	, word8ToWord16   -- :: Word8   -> Word16
	, word8ToWord32   -- :: Word8   -> Word32
	, word8ToWord64   -- :: Word8   -> Word64

	, word16ToInt     -- :: Word16  -> Int
        , word16ToInteger -- :: Word16  -> Integer
	, word16ToWord8   -- :: Word16  -> Word8
	, word16ToWord32  -- :: Word16  -> Word32
	, word16ToWord64  -- :: Word16  -> Word64

	, word32ToInt     -- :: Word32  -> Int
        , word32ToInteger -- :: Word32  -> Integer
	, word32ToWord8   -- :: Word32  -> Word8
	, word32ToWord16  -- :: Word32  -> Word16
	, word32ToWord64  -- :: Word32  -> Word64

	, word64ToInt     -- :: Word64  -> Int
        , word64ToInteger -- :: Word64  -> Integer
	, word64ToWord8   -- :: Word64  -> Word8
	, word64ToWord16  -- :: Word64  -> Word16
	, word64ToWord32  -- :: Word64  -> Word32

#ifndef __HUGS__
	-- NB! GHC SPECIFIC:
 	, wordToWord8     -- :: Word    -> Word8
	, wordToWord16    -- :: Word    -> Word16
	, wordToWord32    -- :: Word    -> Word32
	, wordToWord64    -- :: Word    -> Word64

	, word8ToWord     -- :: Word8   -> Word
	, word16ToWord    -- :: Word16  -> Word
	, word32ToWord    -- :: Word32  -> Word
	, word64ToWord    -- :: Word64  -> Word
#endif

	-- The "official" place to get these from is Addr.
        -- SUP: deprecated in the new FFI, subsumed by the Storable class
	, indexWord8OffAddr
	, indexWord16OffAddr
	, indexWord32OffAddr
	, indexWord64OffAddr
	
	, readWord8OffAddr
	, readWord16OffAddr
	, readWord32OffAddr
	, readWord64OffAddr
	
	, writeWord8OffAddr
	, writeWord16OffAddr
	, writeWord32OffAddr
	, writeWord64OffAddr
	
        -- SUP: deprecated in the new FFI, subsumed by the Storable class
	, sizeofWord8
	, sizeofWord16
	, sizeofWord32
	, sizeofWord64

	-- The "official" place to get these from is Foreign
        -- SUP: deprecated in the new FFI, subsumed by the Storable class
        -- + foreignObjToAddr
#ifndef __PARALLEL_HASKELL__
#ifndef __HUGS__
	, indexWord8OffForeignObj
	, indexWord16OffForeignObj
	, indexWord32OffForeignObj
	, indexWord64OffForeignObj
	
	, readWord8OffForeignObj
	, readWord16OffForeignObj
	, readWord32OffForeignObj
	, readWord64OffForeignObj
	
	, writeWord8OffForeignObj
	, writeWord16OffForeignObj
	, writeWord32OffForeignObj
	, writeWord64OffForeignObj
#endif
#endif

#ifndef __HUGS__
	-- non-standard, GHC specific
	, wordToInt
#endif
	) where


#ifndef __HUGS__
import PrelBase
import CCall
import PrelIOBase	( IO(..), ForeignObj(..) )
import PrelAddr
import PrelNum		( Num(fromInt), Integer(..) )
import PrelEnum		( boundedEnumFrom, boundedEnumFromThen )
import PrelReal		( Integral(toInt) )
#else
import PrelPrim		( Word
			, primIntToWord, primWordToInt
			, primPlusWord, primMinusWord, primTimesWord
			, primNegateWord, primMaxWord
			, primAndWord, primOrWord, primXorWord
			, primShiftLWord, primShiftRLWord
			, absReal, signumReal
			, numericEnumFrom, numericEnumFromThen
			, numericEnumFromTo, numericEnumFromThenTo
			)
#endif
import Ix
import Bits
import Ratio
import Numeric		(readDec, showInt)

-----------------------------------------------------------------------------
-- The "official" coercion functions
-----------------------------------------------------------------------------

intToWord8      :: Int     -> Word8
intToWord16     :: Int     -> Word16
intToWord32     :: Int     -> Word32
intToWord64     :: Int     -> Word64

integerToWord8  :: Integer -> Word8
integerToWord16 :: Integer -> Word16
integerToWord32 :: Integer -> Word32
integerToWord64 :: Integer -> Word64

word8ToInt      :: Word8   -> Int
word8ToInteger  :: Word8   -> Integer
word8ToWord16   :: Word8   -> Word16
word8ToWord32   :: Word8   -> Word32
word8ToWord64   :: Word8   -> Word64

word16ToInt     :: Word16  -> Int
word16ToInteger :: Word16  -> Integer
word16ToWord8   :: Word16  -> Word8
word16ToWord32  :: Word16  -> Word32
word16ToWord64  :: Word16  -> Word64

word32ToInt     :: Word32  -> Int
word32ToInteger :: Word32  -> Integer
word32ToWord8   :: Word32  -> Word8
word32ToWord16  :: Word32  -> Word16
word32ToWord64  :: Word32  -> Word64

word64ToInt     :: Word64  -> Int
word64ToInteger :: Word64  -> Integer
word64ToWord8   :: Word64  -> Word8
word64ToWord16  :: Word64  -> Word16
word64ToWord32  :: Word64  -> Word32

#ifndef __HUGS__
wordToWord8     :: Word    -> Word8
wordToWord16    :: Word    -> Word16
wordToWord32    :: Word    -> Word32
wordToWord64    :: Word    -> Word64

word8ToWord     :: Word8   -> Word
word16ToWord    :: Word16  -> Word
word32ToWord    :: Word32  -> Word
word64ToWord    :: Word64  -> Word
#endif

intToWord8      = word32ToWord8   . intToWord32
intToWord16     = word32ToWord16  . intToWord32

integerToWord8  = fromInteger
integerToWord16 = fromInteger

word8ToInt      = word32ToInt     . word8ToWord32
word8ToInteger  = word32ToInteger . word8ToWord32

word16ToInt     = word32ToInt     . word16ToWord32
word16ToInteger = word32ToInteger . word16ToWord32

#ifndef __HUGS__
intToWord32 (I# x)   = W32# ((int2Word# x) `and#` (case (maxBound::Word32) of W32# x# -> x#))
--intToWord32 (I# x)   = W32# (int2Word# x)
word32ToInt (W32# x) = I#   (word2Int# x)

word32ToInteger (W32# x) = word2Integer x
integerToWord32 = fromInteger

#else // __HUGS__

intToWord32 = W32 . primIntToWord
word32ToInt = primWordToInt . unW32

word32ToInteger x = (toInteger (word32ToInt x) + twoToPower32) `rem` twoToPower32
integerToWord32 = fromInteger
#endif

-----------------------------------------------------------------------------
-- The following rules for fromIntegral remove the need to export specialized
-- conversion functions.
-----------------------------------------------------------------------------

{-# RULES
   "fromIntegral/Int->Word8"        fromIntegral = intToWord8;
   "fromIntegral/Int->Word16"       fromIntegral = intToWord16;
   "fromIntegral/Int->Word32"       fromIntegral = intToWord32;
   "fromIntegral/Int->Word64"       fromIntegral = intToWord64;

   "fromIntegral/Integer->Word8"    fromIntegral = integerToWord8;
   "fromIntegral/Integer->Word16"   fromIntegral = integerToWord16;
   "fromIntegral/Integer->Word32"   fromIntegral = integerToWord32;
   "fromIntegral/Integer->Word64"   fromIntegral = integerToWord64;

   "fromIntegral/Word8->Int"        fromIntegral = word8ToInt;
   "fromIntegral/Word8->Integer"    fromIntegral = word8ToInteger;
   "fromIntegral/Word8->Word16"     fromIntegral = word8ToWord16;
   "fromIntegral/Word8->Word32"     fromIntegral = word8ToWord32;
   "fromIntegral/Word8->Word64"     fromIntegral = word8ToWord64;

   "fromIntegral/Word16->Int"       fromIntegral = word16ToInt;
   "fromIntegral/Word16->Integer"   fromIntegral = word16ToInteger;
   "fromIntegral/Word16->Word8"     fromIntegral = word16ToWord8;
   "fromIntegral/Word16->Word32"    fromIntegral = word16ToWord32;
   "fromIntegral/Word16->Word64"    fromIntegral = word16ToWord64;

   "fromIntegral/Word32->Int"       fromIntegral = word32ToInt;
   "fromIntegral/Word32->Integer"   fromIntegral = word32ToInteger;
   "fromIntegral/Word32->Word8"     fromIntegral = word32ToWord8;
   "fromIntegral/Word32->Word16"    fromIntegral = word32ToWord16;
   "fromIntegral/Word32->Word64"    fromIntegral = word32ToWord64;

   "fromIntegral/Word64->Int"       fromIntegral = word64ToInt;
   "fromIntegral/Word64->Integer"   fromIntegral = word64ToInteger;
   "fromIntegral/Word64->Word8"     fromIntegral = word64ToWord8;
   "fromIntegral/Word64->Word16"    fromIntegral = word64ToWord16;
   "fromIntegral/Word64->Word32"    fromIntegral = word64ToWord32
 #-}

\end{code}

\subsection[Word8]{The @Word8@ interface}


The byte type @Word8@ is represented in the Haskell
heap by boxing up a 32-bit quantity, @Word#@. An invariant
for this representation is that the higher 24 bits are
*always* zeroed out. A consequence of this is that
operations that could possibly overflow have to mask
out the top three bytes before building the resulting @Word8@.

\begin{code}
#ifndef __HUGS__
data Word8  = W8# Word#
#else
newtype Word8  = W8 { unW8 :: Word } deriving (Eq, Ord)
mkW8 = wordToWord8
#endif

#ifndef __HUGS__

instance CCallable Word8
instance CReturnable Word8

word8ToWord32 (W8#  x) = W32# x
word8ToWord16 (W8#  x) = W16# x
word32ToWord8 (W32# x) = W8# (wordToWord8# x)

-- mask out upper three bytes.
intToWord8# :: Int# -> Word#
intToWord8# i# = (int2Word# i#) `and#` (int2Word# 0xff#)

wordToWord8# :: Word# -> Word#
wordToWord8# w# = w# `and#` (int2Word# 0xff#)

instance Eq  Word8     where 
  (W8# x) == (W8# y) = x `eqWord#` y
  (W8# x) /= (W8# y) = x `neWord#` y

instance Ord Word8     where 
  compare (W8# x#) (W8# y#) = compareWord# x# y#
  (<)  (W8# x) (W8# y)      = x `ltWord#` y
  (<=) (W8# x) (W8# y)      = x `leWord#` y
  (>=) (W8# x) (W8# y)      = x `geWord#` y
  (>)  (W8# x) (W8# y)      = x `gtWord#` y
  max x@(W8# x#) y@(W8# y#) = 
     case (compareWord# x# y#) of { LT -> y ; EQ -> x ; GT -> x }
  min x@(W8# x#) y@(W8# y#) =
     case (compareWord# x# y#) of { LT -> x ; EQ -> x ; GT -> y }

-- Helper function, used by Ord Word* instances.
compareWord# :: Word# -> Word# -> Ordering
compareWord# x# y# 
 | x# `ltWord#` y# = LT
 | x# `eqWord#` y# = EQ
 | otherwise       = GT
#else // __HUGS__

word8ToWord32 (W8  x) = W32 x
word8ToWord16 (W8  x) = W16 x
word32ToWord8 (W32 x) = mkW8 x
wordToWord8 w = W8 (w `primAndWord` (primIntToWord 0xff))

#endif

#ifndef __HUGS__
instance Num Word8 where
  (W8# x) + (W8# y) = 
      W8# (intToWord8# (word2Int# x +# word2Int# y))
  (W8# x) - (W8# y) = 
      W8# (intToWord8# (word2Int# x -# word2Int# y))
  (W8# x) * (W8# y) = 
      W8# (intToWord8# (word2Int# x *# word2Int# y))
  negate w@(W8# x)  = 
     if x' ==# 0# 
      then w
      else W8# (int2Word# (0x100# -# x'))
     where
      x' = word2Int# x
  abs x         = x
  signum        = signumReal
  fromInteger (S# i#)    = W8# (wordToWord8# (int2Word# i#))
  fromInteger (J# s# d#) = W8# (wordToWord8# (integer2Word# s# d#))
  fromInt       = intToWord8
#else

instance Num Word8 where
    (+) x y       = binop8 primPlusWord x y
    (-) x y       = binop8 primMinusWord x y
    negate        = mkW8 . primNegateWord . unW8
    (*) x y       = binop8 primTimesWord x y
    abs           = absReal
    signum        = signumReal
    fromInteger   = intToWord8 . toInt	-- overflow issues?
    fromInt       = intToWord8

binop8 op x y = mkW8 (unW8 x `op` unW8 y)
#endif

instance Bounded Word8 where
  minBound = 0
  maxBound = 0xff

instance Real Word8 where
  toRational x = toInteger x % 1

#ifndef __HUGS__
-- Note: no need to mask results here 
-- as they cannot overflow.
instance Integral Word8 where
  div  x@(W8# x#)  (W8# y#) 
    | y# `neWord#` (int2Word# 0#) = W8# (x# `quotWord#` y#)
    | otherwise                   = divZeroError "div{Word8}" x

  quot x@(W8# x#)  (W8# y#)   
    | y# `neWord#` (int2Word# 0#) = W8# (x# `quotWord#` y#)
    | otherwise                   = divZeroError "quot{Word8}" x

  rem  x@(W8# x#)  (W8# y#)
    | y# `neWord#` (int2Word# 0#) = W8# (x# `remWord#` y#)
    | otherwise                   = divZeroError "rem{Word8}" x

  mod  x@(W8# x#)  (W8# y#)
    | y# `neWord#` (int2Word# 0#) = W8# (x# `remWord#` y#)
    | otherwise                   = divZeroError "mod{Word8}" x

  quotRem (W8# x) (W8# y) = (W8# (x `quotWord#` y), W8# (x `remWord#` y))
  divMod  (W8# x) (W8# y) = (W8# (x `quotWord#` y), W8# (x `remWord#` y))

  toInteger (W8# x)       = word2Integer x
  toInt x                 = word8ToInt x
#else // __HUGS__
instance Integral Word8 where
    div  x y 
      | y /= 0         = quotWord8 x y
      | otherwise      = divZeroError "div{Word8}" x

    quot x y
      | y /= 0         = quotWord8 x y
      | otherwise      = divZeroError "quot{Word8}" x

    rem	 x y
      | y /= 0         = remWord8 x y
      | otherwise      = divZeroError "rem{Word8}" x

    mod  x y
      | y /= 0         = remWord8 x y
      | otherwise      = divZeroError "mod{Word8}" x

    quotRem a b        = (a `quot` b, a `rem` b)
    divMod x y         = quotRem x y
    toInteger          = word8ToInteger 
    toInt              = word8ToInt

remWord8, quotWord8 :: Word8 -> Word8 -> Word8
remWord8 w1 w2 
	= integerToWord8 (word8ToInteger w1 `rem` word8ToInteger w2)
quotWord8 w1 w2 
	= integerToWord8 (word8ToInteger w1 `quot` word8ToInteger w2)
#endif

instance Ix Word8 where
    range (m,n)          = [m..n]
    index b@(m,_) i
	   | inRange b i = word8ToInt (i-m)
	   | otherwise   = indexError i b "Word8"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word8 where
    succ w	    
      | w == maxBound = succError "Word8"
      | otherwise     = w+1
    pred w	    
      | w == minBound = predError "Word8"
      | otherwise     = w-1

#ifndef __HUGS__
    toEnum   i@(I# i#)  
      | i >= toInt (minBound::Word8) && i <= toInt (maxBound::Word8) 
      = W8# (intToWord8# i#)
      | otherwise
      = toEnumError "Word8" i (minBound::Word8,maxBound::Word8)

    fromEnum  (W8# w) = I# (word2Int# w)
#else // __HUGS__
    toEnum   x
      | x >= 0    = intToWord8 x
      | otherwise
      = toEnumError "Word8" x (minBound::Word8,maxBound::Word8)
    fromEnum   x
      | x <= intToWord8 (maxBound::Int)
      = word8ToInt x
      | otherwise
      = fromEnumError "Word8" x
#endif

    enumFrom          = boundedEnumFrom
    enumFromThen      = boundedEnumFromThen

instance Read Word8 where
    readsPrec _ = readDec

instance Show Word8 where
    showsPrec _ = showInt

#ifndef __HUGS__
--
-- Word8s are represented by an (unboxed) 32-bit Word.
-- The invariant is that the upper 24 bits are always zeroed out.
--
instance Bits Word8 where
  (W8# x)  .&.  (W8# y)    = W8# (x `and#` y)
  (W8# x)  .|.  (W8# y)    = W8# (x `or#` y)
  (W8# x) `xor` (W8# y)    = W8# (x `xor#` y)
  complement (W8# x)       = W8# (x `xor#` int2Word# 0xff#)
  shift (W8# x#) i@(I# i#)
	| i > 0     = W8# (wordToWord8# (shiftL# x# i#))
	| otherwise = W8# (wordToWord8# (shiftRL# x# (negateInt# i#)))
  w@(W8# x)  `rotate` (I# i)
        | i ==# 0#    = w
	| i ># 0#     = W8# ((wordToWord8# (shiftL# x i')) `or#`
	                     (shiftRL# (x `and#` 
			                (int2Word# (0x100# -# pow2# i2)))
			               i2))
	| otherwise = rotate w (I# (8# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 7#)
           i2 = 8# -# i'

  bit (I# i#)
	| i# >=# 0# && i# <=# 7# = W8# (wordToWord8# (shiftL# (int2Word# 1#) i#))
	| otherwise = 0 -- We'll be overbearing, for now..

  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i

  testBit (W8# x#) (I# i#)
    | i# <# 8# && i# >=# 0# = (word2Int# (x# `and#` (shiftL# (int2Word# 1#) i#))) /=# 0#
    | otherwise             = False -- for now, this is really an error.

  bitSize  _    = 8
  isSigned _    = False
#else // __HUGS__
instance Bits Word8 where
  x .&. y       = binop8 primAndWord x y
  x .|. y       = binop8 primOrWord x y
  x `xor` y     = binop8 primXorWord x y
  complement    = xor ((-1) :: Word8)  
  x `shift` i   | i == 0 = x
		| i > 0  && i < 8 
			 = mkW8 (primShiftLWord (unW8 x) 
						  (primIntToWord i))
		| i < 0  && i > -8
			 = mkW8 (primShiftRLWord (unW8 x)
						   (primIntToWord (-i)))
		| otherwise = 0
  x `rotate` i  | i == 0 = x
  		| i > 0  && i < 8
			  = (x `shift` i) .|. (x `shift` (i - 8))
		| i < 0  && i > -8 
			 = x `rotate` (8 + i)
		| otherwise = x `rotate` (i `mod` 8)
  bit           = shift 0x1
  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i   = (x .&. bit i) /= 0
  bitSize  _    = 8
  isSigned _    = False
#endif

#ifndef __HUGS__
pow2# :: Int# -> Int#
pow2# x# = word2Int# (shiftL# (int2Word# 1#) x#)

word2Integer :: Word# -> Integer
word2Integer w = case word2Integer# w of
			(# s, d #) -> J# s d

pow2_64# :: Int# -> Int64#
pow2_64# x# = word64ToInt64# (shiftL64# (wordToWord64# (int2Word# 1#)) x#)

#endif
sizeofWord8 :: Word32
sizeofWord8 = 1

\end{code}

\subsection[Word16]{The @Word16@ interface}

The double byte type @Word16@ is represented in the Haskell
heap by boxing up a machine word, @Word#@. An invariant
for this representation is that only the lower 16 bits are
`active', any bits above are {\em always} zeroed out.
A consequence of this is that operations that could possibly
overflow have to mask out anything above the lower two bytes
before putting together the resulting @Word16@.

\begin{code}
#ifndef __HUGS__
data Word16 = W16# Word#
#else
newtype Word16  = W16 { unW16 :: Word } deriving (Eq, Ord)
mkW16 = wordToWord16
#endif

#ifndef __HUGS__
instance CCallable Word16
instance CReturnable Word16

word16ToWord8  (W16# x) = W8#  (wordToWord8#  x)
word16ToWord32 (W16# x) = W32# x

word32ToWord16 (W32# x) = W16# (wordToWord16# x)

-- mask out upper 16 bits.
intToWord16# :: Int# -> Word#
intToWord16# i# = ((int2Word# i#) `and#` (int2Word# 0xffff#))

wordToWord16# :: Word# -> Word#
wordToWord16# w# = w# `and#` (int2Word# 0xffff#)

instance Eq  Word16    where 
  (W16# x) == (W16# y) = x `eqWord#` y
  (W16# x) /= (W16# y) = x `neWord#` y

instance Ord Word16     where
  compare (W16# x#) (W16# y#) = compareWord# x# y#
  (<)  (W16# x) (W16# y)      = x `ltWord#` y
  (<=) (W16# x) (W16# y)      = x `leWord#` y
  (>=) (W16# x) (W16# y)      = x `geWord#` y
  (>)  (W16# x) (W16# y)      = x `gtWord#` y
  max x@(W16# x#) y@(W16# y#) = 
     case (compareWord# x# y#) of { LT -> y ; EQ -> x ; GT -> x }
  min x@(W16# x#) y@(W16# y#) =
     case (compareWord# x# y#) of { LT -> x ; EQ -> x ; GT -> y }

#else // __HUGS__

word16ToWord32 (W16 x) = W32 x
word16ToWord8  (W16 x) = mkW8 x
word32ToWord16 (W32 x) = mkW16 x
wordToWord16 w = W16 (w `primAndWord` (primIntToWord 0xffff))

#endif


#ifndef __HUGS__
instance Num Word16 where
  (W16# x) + (W16# y) = 
       W16# (intToWord16# (word2Int# x +# word2Int# y))
  (W16# x) - (W16# y) = 
       W16# (intToWord16# (word2Int# x -# word2Int# y))
  (W16# x) * (W16# y) = 
       W16# (intToWord16# (word2Int# x *# word2Int# y))
  negate w@(W16# x)  = 
       if x' ==# 0# 
        then w
        else W16# (int2Word# (0x10000# -# x'))
       where
        x' = word2Int# x
  abs x         = x
  signum        = signumReal
  fromInteger (S# i#)    = W16# (wordToWord16# (int2Word# i#))
  fromInteger (J# s# d#) = W16# (wordToWord16# (integer2Word# s# d#))
  fromInt       = intToWord16
#else

instance Num Word16 where
    (+) x y       = binop16 primPlusWord x y
    (-) x y       = binop16 primMinusWord x y
    negate        = mkW16 . primNegateWord . unW16
    (*) x y       = binop16 primTimesWord x y
    abs           = absReal
    signum        = signumReal
    fromInteger   = intToWord16 . toInt	-- overflow issues?
    fromInt       = intToWord16

binop16 op x y = mkW16 (unW16 x `op` unW16 y)
#endif

instance Bounded Word16 where
  minBound = 0
  maxBound = 0xffff

instance Real Word16 where
  toRational x = toInteger x % 1

#ifndef __HUGS__
instance Integral Word16 where
  div  x@(W16# x#)  (W16# y#)
   | y# `neWord#` (int2Word# 0#) = W16# (x# `quotWord#` y#)
   | otherwise                   = divZeroError "div{Word16}" x

  quot x@(W16# x#) (W16# y#)
   | y# `neWord#`(int2Word# 0#)  = W16# (x# `quotWord#` y#)
   | otherwise                   = divZeroError "quot{Word16}" x

  rem  x@(W16# x#) (W16# y#)
   | y# `neWord#` (int2Word# 0#) = W16# (x# `remWord#` y#)
   | otherwise                   = divZeroError "rem{Word16}" x

  mod  x@(W16# x#)  (W16# y#)
   | y# `neWord#` (int2Word# 0#) = W16# (x# `remWord#` y#)
   | otherwise		         = divZeroError "mod{Word16}" x

  quotRem (W16# x) (W16# y) = (W16# (x `quotWord#` y), W16# (x `remWord#` y))
  divMod  (W16# x) (W16# y) = (W16# (x `quotWord#` y), W16# (x `remWord#` y))

  toInteger (W16# x)        = word2Integer x
  toInt x                   = word16ToInt x
#else // __HUGS__
instance Integral Word16 where
    div  x y 
      | y /= 0         = quotWord16 x y
      | otherwise      = divZeroError "div{Word16}" x

    quot x y
      | y /= 0         = quotWord16 x y
      | otherwise      = divZeroError "quot{Word16}" x

    rem	 x y
      | y /= 0         = remWord16 x y
      | otherwise      = divZeroError "rem{Word16}" x

    mod  x y
      | y /= 0         = remWord16 x y
      | otherwise      = divZeroError "mod{Word16}" x

    quotRem a b        = (a `quot` b, a `rem` b)
    divMod x y         = quotRem x y
    toInteger          = word16ToInteger 
    toInt              = word16ToInt

remWord16, quotWord16 :: Word16 -> Word16 -> Word16
remWord16 w1 w2 
	= integerToWord16 (word16ToInteger w1 `rem` word16ToInteger w2)
quotWord16 w1 w2 
	= integerToWord16 (word16ToInteger w1 `quot` word16ToInteger w2)
#endif

instance Ix Word16 where
  range (m,n)          = [m..n]
  index b@(m,_) i
         | inRange b i = word16ToInt (i - m)
         | otherwise   = indexError i b "Word16"
  inRange (m,n) i      = m <= i && i <= n

instance Enum Word16 where
    succ w	    
      | w == maxBound = succError "Word16"
      | otherwise     = w+1
    pred w	    
      | w == minBound = predError "Word16"
      | otherwise     = w-1

#ifndef __HUGS__
    toEnum   i@(I# i#)  
      | i >= toInt (minBound::Word16) && i <= toInt (maxBound::Word16)
      = W16# (intToWord16# i#)
      | otherwise
      = toEnumError "Word16" i (minBound::Word16,maxBound::Word16)

    fromEnum  (W16# w) = I# (word2Int# w)
#else // __HUGS__
    toEnum   x
      | x >= 0    = intToWord16 x
      | otherwise
      = toEnumError "Word16" x (minBound::Word16,maxBound::Word16)
    fromEnum   x
      | x <= intToWord16 (maxBound::Int)
      = word16ToInt x
      | otherwise
      = fromEnumError "Word16" x
#endif
    enumFrom     = boundedEnumFrom
    enumFromThen = boundedEnumFromThen

instance Read Word16 where
  readsPrec _ = readDec

instance Show Word16 where
  showsPrec _ = showInt

#ifndef __HUGS__
instance Bits Word16 where
  (W16# x)  .&.  (W16# y)  = W16# (x `and#` y)
  (W16# x)  .|.  (W16# y)  = W16# (x `or#` y)
  (W16# x) `xor` (W16# y)  = W16# (x `xor#` y)
  complement (W16# x)      = W16# (x `xor#` int2Word# 0xffff#)
  shift (W16# x#) i@(I# i#)
	| i > 0     = W16# (wordToWord16# (shiftL# x# i#))
	| otherwise = W16# (shiftRL# x# (negateInt# i#))
  w@(W16# x)  `rotate` (I# i)
        | i ==# 0#    = w
	| i ># 0#     = W16# ((wordToWord16# (shiftL# x i')) `or#`
	                      (shiftRL# (x `and#` 
			                 (int2Word# (0x10000# -# pow2# i2)))
			                i2))
	| otherwise = rotate w (I# (16# +# i'))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 15#)
           i2 = 16# -# i'
  bit (I# i#)
	| i# >=# 0# && i# <=# 15# = W16# (shiftL# (int2Word# 1#) i#)
	| otherwise = 0 -- We'll be overbearing, for now..

  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i

  testBit (W16# x#) (I# i#)
    | i# <# 16# && i# >=# 0# = (word2Int# (x# `and#` (shiftL# (int2Word# 1#) i#))) /=# 0#
    | otherwise             = False -- for now, this is really an error.

  bitSize  _    = 16
  isSigned _    = False
#else // __HUGS__
instance Bits Word16 where
  x .&. y       = binop16 primAndWord x y
  x .|. y       = binop16 primOrWord x y
  x `xor` y     = binop16 primXorWord x y
  complement    = xor ((-1) :: Word16)  
  x `shift` i   | i == 0 = x
		| i > 0  && i < 16 
			 = mkW16 (primShiftLWord (unW16 x) 
						  (primIntToWord i))
		| i < 0  && i > -16
			 = mkW16 (primShiftRLWord (unW16 x)
						   (primIntToWord (-i)))
		| otherwise = 0
  x `rotate` i  | i == 0 = x
  		| i > 0  && i < 16
			  = (x `shift` i) .|. (x `shift` (i - 16))
		| i < 0  && i > -16 
			 = x `rotate` (16 + i)
		| otherwise = x `rotate` (i `mod` 16)
  bit           = shift 0x1
  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i   = (x .&. bit i) /= 0
  bitSize  _    = 16
  isSigned _    = False
#endif

sizeofWord16 :: Word32
sizeofWord16 = 2

\end{code}


\subsection[Word32]{The @Word32@ interface}

The quad byte type @Word32@ is represented in the Haskell
heap by boxing up a machine word, @Word#@. An invariant
for this representation is that any bits above the lower
32 are {\em always} zeroed out. A consequence of this is that
operations that could possibly overflow have to mask
the result before building the resulting @Word16@.

\begin{code}
#ifndef __HUGS__
data Word32 = W32# Word#
#else
newtype Word32 = W32 { unW32 :: Word } deriving (Eq,Ord)
#endif

#ifndef __HUGS__
instance CCallable Word32
instance CReturnable Word32

instance Eq  Word32    where 
  (W32# x) == (W32# y) = x `eqWord#` y
  (W32# x) /= (W32# y) = x `neWord#` y

instance Ord Word32    where
  compare (W32# x#) (W32# y#) = compareWord# x# y#
  (<)  (W32# x) (W32# y)      = x `ltWord#` y
  (<=) (W32# x) (W32# y)      = x `leWord#` y
  (>=) (W32# x) (W32# y)      = x `geWord#` y
  (>)  (W32# x) (W32# y)      = x `gtWord#` y
  max x@(W32# x#) y@(W32# y#) = 
     case (compareWord# x# y#) of { LT -> y ; EQ -> x ; GT -> x }
  min x@(W32# x#) y@(W32# y#) =
     case (compareWord# x# y#) of { LT -> x ; EQ -> x ; GT -> y }
#endif

#ifndef __HUGS__
instance Num Word32 where
  (W32# x) + (W32# y) = 
       W32# (intToWord32# (word2Int# x +# word2Int# y))
  (W32# x) - (W32# y) =
       W32# (intToWord32# (word2Int# x -# word2Int# y))
  (W32# x) * (W32# y) = 
       W32# (intToWord32# (word2Int# x *# word2Int# y))
#if WORD_SIZE_IN_BYTES == 8
  negate w@(W32# x)  = 
      if x' ==# 0#
       then w
       else W32# (intToWord32# (0x100000000# -# x'))
       where
        x' = word2Int# x
#else
  negate (W32# x)  = W32# (intToWord32# (negateInt# (word2Int# x)))
#endif
  abs x           = x
  signum          = signumReal
  fromInteger (S# i#)    = W32# (intToWord32# i#)
  fromInteger (J# s# d#) = W32# (wordToWord32# (integer2Word# s# d#))
  fromInt (I# x)  = W32# (intToWord32# x)
    -- ToDo: restrict fromInt{eger} range.

intToWord32#  :: Int#  -> Word#
wordToWord32# :: Word# -> Word#

#if WORD_SIZE_IN_BYTES == 8
intToWord32#  i#  = (int2Word# i#) `and#` (int2Word# 0xffffffff#)
wordToWord32# w#  = w# `and#` (int2Word# 0xffffffff#)
wordToWord64# w#  = w#
#else
intToWord32#  i# = int2Word# i#
wordToWord32# w# = w#
#endif
#else // __HUGS__
instance Num Word32 where
    (+) x y       = binop32 primPlusWord x y
    (-) x y       = binop32 primMinusWord x y
    negate        = W32 . primNegateWord . unW32
    (*) x y       = binop32 primTimesWord x y
    abs           = absReal
    signum        = signumReal
    fromInteger   = intToWord32 . toInt	-- overflow issues?
    fromInt       = intToWord32

-- a couple of utils
binop32 op x y = W32 (unW32 x `op` unW32 y)
twoToPower32 = 4294967296 -- 0x100000000
#endif

instance Bounded Word32 where
    minBound = 0
#if WORD_SIZE_IN_BYTES == 8
    maxBound = 0xffffffff
#else
    maxBound = minBound - 1
#endif

instance Real Word32 where
    toRational x = toInteger x % 1

instance Integral Word32 where
    div  x y 
      | y /= 0         = quotWord32 x y
      | otherwise      = divZeroError "div{Word32}" x

    quot x y
      | y /= 0         = quotWord32 x y
      | otherwise      = divZeroError "quot{Word32}" x

    rem	 x y
      | y /= 0         = remWord32 x y
      | otherwise      = divZeroError "rem{Word32}" x

    mod  x y
      | y /= 0         = remWord32 x y
      | otherwise      = divZeroError "mod{Word32}" x

    quotRem a b        = (a `quot` b, a `rem` b)
    divMod x y         = quotRem x y
#ifndef __HUGS__
    toInteger (W32# x) = word2Integer x
    toInt     (W32# x) = I# (word2Int# x)
#else 
    toInteger          = word32ToInteger 
    toInt              = word32ToInt
#endif


#ifndef __HUGS__
{-# INLINE quotWord32 #-}
{-# INLINE remWord32  #-}
remWord32, quotWord32 :: Word32 -> Word32 -> Word32
(W32# x) `quotWord32` (W32# y) = W32# (x `quotWord#` y)
(W32# x) `remWord32`  (W32# y) = W32# (x `remWord#`  y)
#else // __HUGS__
remWord32, quotWord32 :: Word32 -> Word32 -> Word32
remWord32 w1 w2 
	= integerToWord32 (word32ToInteger w1 `rem` word32ToInteger w2)
quotWord32 w1 w2 
	= integerToWord32 (word32ToInteger w1 `quot` word32ToInteger w2)
#endif



instance Ix Word32 where
    range (m,n)          = [m..n]
    index b@(m,_) i
	   | inRange b i = word32ToInt (i - m)
	   | otherwise   = indexError i b "Word32"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word32 where
    succ w	    
      | w == maxBound = succError "Word32"
      | otherwise     = w+1
    pred w	    
      | w == minBound = predError "Word32"
      | otherwise     = w-1

     -- the toEnum/fromEnum will fail if the mapping isn't legal,
     -- use the intTo* & *ToInt coercion functions to 'bypass' these range checks.
    toEnum   x
      | x >= 0    = intToWord32 x
      | otherwise
      = toEnumError "Word32" x (minBound::Word32,maxBound::Word32)

    fromEnum   x
      | x <= intToWord32 (maxBound::Int)
      = word32ToInt x
      | otherwise
      = fromEnumError "Word32" x 

    enumFrom w           = [w .. maxBound]
    enumFromTo   w1 w2
       | w1 <= w2        = eftt32 True{-increasing-} w1 diff_f last
       | otherwise	 = []
	where
         last = (> w2)
	 diff_f x = x + 1 
	  
    enumFromThen w1 w2   = [w1,w2 .. last]
       where
	 last :: Word32
	 last
	  | w1 <=w2   = maxBound
	  | otherwise = minBound

    enumFromThenTo w1 w2 wend  = eftt32 increasing w1 step_f last
     where
       increasing = w1 <= w2
       diff1 = w2 - w1
       diff2 = w1 - w2
       
       last
        | increasing = (> wend)
	| otherwise  = (< wend)

       step_f 
        | increasing = \ x -> x + diff1
        | otherwise  = \ x -> x - diff2

eftt32 :: Bool -> Word32 -> (Word32 -> Word32) -> (Word32-> Bool) -> [Word32]
eftt32 increasing init stepper done = go init
  where
    go now
     | done now                    = []
     | increasing     && now > nxt = [now] -- oflow
     | not increasing && now < nxt = [now] -- uflow
     | otherwise                   = now : go nxt
     where
      nxt = stepper now 

instance Read Word32 where
    readsPrec _ = readDec

instance Show Word32 where
    showsPrec _ = showInt

#ifndef __HUGS__
instance Bits Word32 where
  (W32# x)  .&.  (W32# y)  = W32# (x `and#` y)
  (W32# x)  .|.  (W32# y)  = W32# (x `or#` y)
  (W32# x) `xor` (W32# y)  = W32# (x `xor#` y)
  complement (W32# x)      = W32# (x `xor#` mb#) where (W32# mb#) = maxBound
  shift (W32# x) i@(I# i#)
	| i > 0     = W32# (wordToWord32# (shiftL# x i#))
	| otherwise = W32# (shiftRL# x (negateInt# i#))
  w@(W32# x)  `rotate` (I# i)
        | i ==# 0#    = w
	| i ># 0#     = W32# ((wordToWord32# (shiftL# x i')) `or#`
	                      (shiftRL# (x `and#` 
			                (int2Word# (word2Int# maxBound# -# pow2# i2 +# 1#)))
			             i2))
	| otherwise = rotate w (I# (32# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 31#)
           i2 = 32# -# i'
           (W32# maxBound#) = maxBound

  bit (I# i#)
	| i# >=# 0# && i# <=# 31# = W32# (shiftL# (int2Word# 1#) i#)
	| otherwise = 0 -- We'll be overbearing, for now..

  setBit x i        = x .|. bit i
  clearBit x i      = x .&. complement (bit i)
  complementBit x i = x `xor` bit i

  testBit (W32# x#) (I# i#)
    | i# <# 32# && i# >=# 0# = (word2Int# (x# `and#` (shiftL# (int2Word# 1#) i#))) /=# 0#
    | otherwise             = False -- for now, this is really an error.
  bitSize  _        = 32
  isSigned _        = False

#else // __HUGS__
instance Bits Word32 where
  x .&. y       = binop32 primAndWord x y
  x .|. y       = binop32 primOrWord x y
  x `xor` y     = binop32 primXorWord x y
  complement    = xor ((-1) :: Word32)  
  x `shift` i   | i == 0 = x
		| i > 0  && i < 32 
			 = W32 (primShiftLWord (unW32 x) 
						  (primIntToWord i))
		| i < 0  && i > -32
			 = W32 (primShiftRLWord (unW32 x)
						   (primIntToWord (-i)))
		| otherwise = 0
  x `rotate` i  | i == 0 = x
  		| i > 0  && i < 32
			  = (x `shift` i) .|. (x `shift` (i - 32))
		| i < 0  && i > -32 
			 = x `rotate` (32 + i)
		| otherwise = x `rotate` (i `mod` 32)
  bit           = shift 0x1
  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i   = (x .&. bit i) /= 0
  bitSize  _    = 32
  isSigned _    = False
#endif

sizeofWord32 :: Word32
sizeofWord32 = 4

\end{code}


\subsection[Word64]{The @Word64@ interface}

\begin{code}
#ifndef __HUGS__
#if WORD_SIZE_IN_BYTES == 8
--data Word64 = W64# Word#

word32ToWord64 (W32 w#) = W64# w#

word8ToWord64 (W8# w#) = W64# w#
word64ToWord8 (W64# w#) = W8# (w# `and#` (int2Word# 0xff#))

word16ToWord64 (W16# w#) = W64# w#
word64ToWord16 (W64# w#) = W16# (w# `and#` (int2Word# 0xffff#))

wordToWord32# :: Word# -> Word#
wordToWord32# w# = w# `and#` (case (maxBound::Word32) of W# x# -> x#)

word64ToWord32 :: Word64 -> Word32
word64ToWord32 (W64# w#) = W32# (wordToWord32# w#)

wordToWord64# w# = w#
word64ToWord# w# = w#

instance Eq  Word64     where 
  (W64# x) == (W64# y) = x `eqWord#` y
  (W64# x) /= (W64# y) = x `neWord#` y

instance Ord Word64     where 
  compare (W64# x#) (W64# y#) = compareWord# x# y#
  (<)  (W64# x) (W64# y)      = x `ltWord#` y
  (<=) (W64# x) (W64# y)      = x `leWord#` y
  (>=) (W64# x) (W64# y)      = x `geWord#` y
  (>)  (W64# x) (W64# y)      = x `gtWord#` y
  max x@(W64# x#) y@(W64# y#) = 
     case (compareWord# x# y#) of { LT -> y ; EQ -> x ; GT -> x }
  min x@(W64# x#) y@(W64# y#) =
     case (compareWord# x# y#) of { LT -> x ; EQ -> x ; GT -> y }

instance Num Word64 where
  (W64# x) + (W64# y) = 
      W64# (intToWord64# (word2Int# x +# word2Int# y))
  (W64# x) - (W64# y) = 
      W64# (intToWord64# (word2Int# x -# word2Int# y))
  (W64# x) * (W64# y) = 
      W64# (intToWord64# (word2Int# x *# word2Int# y))
  negate w@(W64# x)  = 
     if x' ==# 0# 
      then w
      else W64# (int2Word# (0x100# -# x'))
     where
      x' = word2Int# x
  abs x         = x
  signum        = signumReal
  fromInteger (S# i#)    = W64# (int2Word# i#)
  fromInteger (J# s# d#) = W64# (integer2Word# s# d#)
  fromInt       = intToWord64

-- Note: no need to mask results here 
-- as they cannot overflow.
instance Integral Word64 where
  div  x@(W64# x#)  (W64# y#)
    | y# `neWord#` (int2Word# 0#)  = W64# (x# `quotWord#` y#)
    | otherwise                    = divZeroError "div{Word64}" x

  quot x@(W64# x#)  (W64# y#)
    | y# `neWord#` (int2Word# 0#)  = W64# (x# `quotWord#` y#)
    | otherwise                    = divZeroError "quot{Word64}" x

  rem  x@(W64# x#)  (W64# y#)
    | y# `neWord#` (int2Word# 0#)  = W64# (x# `remWord#` y#)
    | otherwise                    = divZeroError "rem{Word64}" x

  mod  (W64# x)  (W64# y)   
    | y# `neWord#` (int2Word# 0#)  = W64# (x `remWord#` y)
    | otherwise                    = divZeroError "mod{Word64}" x

  quotRem (W64# x) (W64# y) = (W64# (x `quotWord#` y), W64# (x `remWord#` y))
  divMod  (W64# x) (W64# y) = (W64# (x `quotWord#` y), W64# (x `remWord#` y))

  toInteger (W64# x)        = word2Integer# x
  toInt x                   = word64ToInt x


instance Bits Word64 where
  (W64# x)  .&.  (W64# y)    = W64# (x `and#` y)
  (W64# x)  .|.  (W64# y)    = W64# (x `or#` y)
  (W64# x) `xor` (W64# y)    = W64# (x `xor#` y)
  complement (W64# x)        = W64# (x `xor#` (case (maxBound::Word64) of W64# x# -> x#))
  shift (W64# x#) i@(I# i#)
	| i > 0     = W64# (shiftL# x# i#)
	| otherwise = W64# (shiftRL# x# (negateInt# i#))

  w@(W64# x)  `rotate` (I# i)
        | i ==# 0#    = w
	| i ># 0#     = W64# (shiftL# x i') `or#`
	                      (shiftRL# (x `and#` 
			                (int2Word# (word2Int# maxBound# -# pow2# i2 +# 1#)))
			             i2))
	| otherwise = rotate w (I# (64# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 63#)
           i2 = 64# -# i'
           (W64# maxBound#) = maxBound

  bit (I# i#)
	| i# >=# 0# && i# <=# 63# = W64# (shiftL# (int2Word# 1#) i#)
	| otherwise = 0 -- We'll be overbearing, for now..

  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i

  testBit (W64# x#) (I# i#)
    | i# <# 64# && i# >=# 0# = (word2Int# (x# `and#` (shiftL# (int2Word# 1#) i#))) /=# 0#
    | otherwise              = False -- for now, this is really an error.

  bitSize  _    = 64
  isSigned _    = False

#else
--defined in PrelCCall: data Word64 = W64 Word64# deriving (Eq, Ord, Bounded)

-- for completeness sake
word32ToWord64 (W32# w#) = W64# (wordToWord64# w#)
word64ToWord32 (W64# w#) = W32# (word64ToWord# w#)

word8ToWord64 (W8# w#) = W64# (wordToWord64# w#)
word64ToWord8 (W64# w#) = W8# ((word64ToWord# w#) `and#` (int2Word# 0xff#))

word16ToWord64 (W16# w#) = W64# (wordToWord64# w#)
word64ToWord16 (W64# w#) = W16# ((word64ToWord# w#) `and#` (int2Word# 0xffff#))

word64ToInteger (W64# w#) = 
  case word64ToInteger# w# of
    (# s#, p# #) -> J# s# p#
word64ToInt w = 
   case w `quotRem` 0x100000000 of 
     (_,l) -> toInt (word64ToWord32 l)

intToWord64# :: Int# -> Word64#
intToWord64# i# = wordToWord64# (int2Word# i#)

intToWord64 (I# i#) = W64# (intToWord64# i#)

integerToWord64 (S# i#)    = W64# (intToWord64# i#)
integerToWord64 (J# s# d#) = W64# (integerToWord64# s# d#)

instance Eq  Word64     where 
  (W64# x) == (W64# y) = x `eqWord64#` y
  (W64# x) /= (W64# y) = not (x `eqWord64#` y)

instance Ord Word64     where 
  compare (W64# x#) (W64# y#) = compareWord64# x# y#
  (<)  (W64# x) (W64# y)      = x `ltWord64#` y
  (<=) (W64# x) (W64# y)      = x `leWord64#` y
  (>=) (W64# x) (W64# y)      = x `geWord64#` y
  (>)  (W64# x) (W64# y)      = x `gtWord64#` y
  max x@(W64# x#) y@(W64# y#) = 
     case (compareWord64# x# y#) of { LT -> y ; EQ -> x ; GT -> x }
  min x@(W64# x#) y@(W64# y#) =
     case (compareWord64# x# y#) of { LT -> x ; EQ -> x ; GT -> y }

instance Num Word64 where
  (W64# x) + (W64# y) = 
      W64# (int64ToWord64# (word64ToInt64# x `plusInt64#` word64ToInt64# y))
  (W64# x) - (W64# y) = 
      W64# (int64ToWord64# (word64ToInt64# x `minusInt64#` word64ToInt64# y))
  (W64# x) * (W64# y) = 
      W64# (int64ToWord64# (word64ToInt64# x `timesInt64#` word64ToInt64# y))
  negate w
     | w == 0     = w
     | otherwise  = maxBound - w

  abs x         = x
  signum        = signumReal
  fromInteger i = integerToWord64 i
  fromInt       = intToWord64

-- Note: no need to mask results here  as they cannot overflow.
-- ToDo: protect against div by zero.
instance Integral Word64 where
  div  (W64# x)  (W64# y)   = W64# (x `quotWord64#` y)
  quot (W64# x)  (W64# y)   = W64# (x `quotWord64#` y)
  rem  (W64# x)  (W64# y)   = W64# (x `remWord64#` y)
  mod  (W64# x)  (W64# y)   = W64# (x `remWord64#` y)
  quotRem (W64# x) (W64# y) = (W64# (x `quotWord64#` y), W64# (x `remWord64#` y))
  divMod  (W64# x) (W64# y) = (W64# (x `quotWord64#` y), W64# (x `remWord64#` y))
  toInteger w64             = word64ToInteger w64
  toInt x                   = word64ToInt x


instance Bits Word64 where
  (W64# x)  .&.  (W64# y)    = W64# (x `and64#` y)
  (W64# x)  .|.  (W64# y)    = W64# (x `or64#` y)
  (W64# x) `xor` (W64# y)    = W64# (x `xor64#` y)
  complement (W64# x)        = W64# (x `xor64#` (case (maxBound::Word64) of W64# x# -> x#))
  shift (W64# x#) i@(I# i#)
	| i > 0     = W64# (shiftL64# x# i#)
	| otherwise = W64# (shiftRL64# x# (negateInt# i#))

  w@(W64# x)  `rotate` (I# i)
        | i ==# 0#    = w
	| i ># 0#     = W64# ((shiftL64# x i') `or64#`
	                      (shiftRL64# (x `and64#` 
			                   (int64ToWord64# ((word64ToInt64# maxBound#) `minusInt64#` 
						           (pow2_64# i2 `plusInt64#` (intToInt64# 1#))))))
			             i2)
	| otherwise = rotate w (I# (64# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 63#)
           i2 = 64# -# i'
           (W64# maxBound#) = maxBound

  bit (I# i#)
	| i# >=# 0# && i# <=# 63# = W64# (shiftL64# (wordToWord64# (int2Word# 1#)) i#)
	| otherwise = 0 -- We'll be overbearing, for now..

  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i

  testBit (W64# x#) (I# i#)
    | i# <# 64# && i# >=# 0# = (word2Int# (word64ToWord# (x# `and64#` (shiftL64# (wordToWord64# (int2Word# 1#)) i#)))) /=# 0#
    | otherwise              = False -- for now, this is really an error.

  bitSize  _    = 64
  isSigned _    = False

compareWord64# :: Word64# -> Word64# -> Ordering
compareWord64# i# j# 
 | i# `ltWord64#` j# = LT
 | i# `eqWord64#` j# = EQ
 | otherwise	     = GT

-- Word64# primop wrappers:

ltWord64# :: Word64# -> Word64# -> Bool
ltWord64# x# y# = stg_ltWord64 x# y# /= 0

leWord64# :: Word64# -> Word64# -> Bool
leWord64# x# y# = stg_leWord64 x# y# /= 0

eqWord64# :: Word64# -> Word64# -> Bool
eqWord64# x# y# = stg_eqWord64 x# y# /= 0
      
neWord64# :: Word64# -> Word64# -> Bool
neWord64# x# y# = stg_neWord64 x# y# /= 0
      
geWord64# :: Word64# -> Word64# -> Bool
geWord64# x# y# = stg_geWord64 x# y# /= 0
      
gtWord64# :: Word64# -> Word64# -> Bool
gtWord64# x# y# = stg_gtWord64 x# y# /= 0

plusInt64# :: Int64# -> Int64# -> Int64#
plusInt64# a# b# = case stg_plusInt64 a# b# of { I64# i# -> i# }

minusInt64# :: Int64# -> Int64# -> Int64#
minusInt64# a# b# = case stg_minusInt64 a# b# of { I64# i# -> i# }

timesInt64# :: Int64# -> Int64# -> Int64#
timesInt64# a# b# = case stg_timesInt64 a# b# of { I64# i# -> i# }

quotWord64# :: Word64# -> Word64# -> Word64#
quotWord64# a# b# = case stg_quotWord64 a# b# of { W64# w# -> w# }

remWord64# :: Word64# -> Word64# -> Word64#
remWord64# a# b# = case stg_remWord64 a# b# of { W64# w# -> w# }

negateInt64# :: Int64# -> Int64#
negateInt64# a# = case stg_negateInt64 a# of { I64# i# -> i# }

and64# :: Word64# -> Word64# -> Word64#
and64# a# b# = case stg_and64 a# b# of { W64# w# -> w# }

or64# :: Word64# -> Word64# -> Word64#
or64# a# b# = case stg_or64 a# b# of { W64# w# -> w# }

xor64# :: Word64# -> Word64# -> Word64#
xor64# a# b# = case stg_xor64 a# b# of { W64# w# -> w# }

not64# :: Word64# -> Word64#
not64# a# = case stg_not64 a# of { W64# w# -> w# }

shiftL64# :: Word64# -> Int# -> Word64#
shiftL64# a# b# = case stg_shiftL64 a# b# of { W64# w# -> w# }

shiftRL64# :: Word64# -> Int# -> Word64#
shiftRL64# a# b# = case stg_shiftRL64 a# b# of { W64# w# -> w# }

word64ToWord# :: Word64# -> Word#
word64ToWord# w64# = case stg_word64ToWord w64# of { W# w# -> w# }
      
wordToWord64# :: Word# -> Word64#
wordToWord64# w# = case stg_wordToWord64 w# of { W64# w64# -> w64# }

word64ToInt64# :: Word64# -> Int64#
word64ToInt64# w64# = case stg_word64ToInt64 w64# of { I64# i# -> i# }

int64ToWord64# :: Int64# -> Word64#
int64ToWord64# i64# = case stg_int64ToWord64 i64# of { W64# w# -> w# }

intToInt64# :: Int# -> Int64#
intToInt64# i# = case stg_intToInt64 i# of { I64# i64# -> i64# }
      
foreign import "stg_intToInt64" unsafe stg_intToInt64 :: Int# -> Int64
foreign import "stg_int64ToWord64" unsafe stg_int64ToWord64 :: Int64# -> Word64
foreign import "stg_word64ToInt64" unsafe stg_word64ToInt64 :: Word64# -> Int64
foreign import "stg_wordToWord64" unsafe stg_wordToWord64 :: Word# -> Word64
foreign import "stg_word64ToWord" unsafe stg_word64ToWord :: Word64# -> Word
foreign import "stg_shiftRL64" unsafe stg_shiftRL64 :: Word64# -> Int# -> Word64
foreign import "stg_shiftL64" unsafe stg_shiftL64 :: Word64# -> Int# -> Word64
foreign import "stg_not64" unsafe stg_not64 :: Word64# -> Word64
foreign import "stg_xor64" unsafe stg_xor64 :: Word64# -> Word64# -> Word64
foreign import "stg_or64" unsafe stg_or64 :: Word64# -> Word64# -> Word64
foreign import "stg_and64" unsafe stg_and64 :: Word64# -> Word64# -> Word64
foreign import "stg_negateInt64" unsafe stg_negateInt64 :: Int64# -> Int64
foreign import "stg_remWord64" unsafe stg_remWord64 :: Word64# -> Word64# -> Word64
foreign import "stg_quotWord64" unsafe stg_quotWord64 :: Word64# -> Word64# -> Word64
foreign import "stg_timesInt64" unsafe stg_timesInt64 :: Int64# -> Int64# -> Int64
foreign import "stg_minusInt64" unsafe stg_minusInt64 :: Int64# -> Int64# -> Int64
foreign import "stg_plusInt64" unsafe stg_plusInt64 :: Int64# -> Int64# -> Int64
foreign import "stg_gtWord64" unsafe stg_gtWord64 :: Word64# -> Word64# -> Int
foreign import "stg_geWord64" unsafe stg_geWord64 :: Word64# -> Word64# -> Int
foreign import "stg_neWord64" unsafe stg_neWord64 :: Word64# -> Word64# -> Int
foreign import "stg_eqWord64" unsafe stg_eqWord64 :: Word64# -> Word64# -> Int
foreign import "stg_leWord64" unsafe stg_leWord64 :: Word64# -> Word64# -> Int
foreign import "stg_ltWord64" unsafe stg_ltWord64 :: Word64# -> Word64# -> Int

#endif
#else // __HUGS__
data Word64 = W64
	{ hi :: !Word32
	, lo :: !Word32
	} deriving (Eq, Ord)

to64 = integerToWord64
from64 = word64ToInteger
binop64 op x y = integerToWord64 (from64 x `op` from64 y)

word64ToInteger W64{lo=lo,hi=hi} 
	= toInteger lo + twoToPower32 * toInteger hi 
integerToWord64 x = case x `quotRem` twoToPower32 of 
                 (h,l) -> W64{lo=fromInteger l, hi=fromInteger h}


instance Num Word64 where
    (+) x y       = binop64 (+) x y
    (-) x y       = binop64 (-) x y
    negate        = to64 . negate. from64
    (*) x y       = binop64 (*) x y
    abs           = absReal
    signum        = signumReal
    fromInteger   = integerToWord64
    fromInt       = fromInteger . toInteger

instance Integral Word64 where
  x `div` y     = fromInteger (toInteger x `div` toInteger y)
  x `quot` y    = fromInteger (toInteger x `quot` toInteger y)
  x `rem` y     = fromInteger (toInteger x `rem` toInteger y)
  x `mod` y     = fromInteger (toInteger x `mod` toInteger y)
  x `quotRem` y = (x `quot` y,x `rem` y)
  divMod        = quotRem
  even          = even      . toInt
  toInteger x   = word64ToInteger x

instance Bits Word64 where
  x .&. y       = W64 { lo = lo x .&. lo y
		      , hi = hi x .&. hi y }
  x .|. y       = W64 { lo = lo x .|. lo y
		      , hi = hi x .|. hi y }
  x `xor` y     = W64 { lo = lo x `xor` lo y
		      , hi = hi x `xor` hi y }
  complement    = xor (-1)
  x `shift` i   = to64 val
	where val    | i == 0 = from64 x
		     | i > 0  && i < 64   = from64 x * (2 ^ i)
	             | i < 0  && i > -64 = from64 x `div` (2 ^ (-i))
		     | otherwise         = 0
  x `rotate` i  | i == 0 = x
  		| i > 0  && i < 64
			  = (x `shift` i) .|. (x `shift` (i - 64))
		| i < 0  && i > -64 
			 = x `rotate` (64 + i)
		| otherwise = x `rotate` (i `mod` 64)

  bit i         = to64 (2 ^ i)
  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i   = (x .&. bit i) /= 0
  bitSize  _    = 64
  isSigned _    = False
#endif

instance Enum Word64 where
    succ w	    
      | w == maxBound = succError "Word64"
      | otherwise     = w+1
    pred w	    
      | w == minBound = predError "Word64"
      | otherwise     = w-1

    toEnum i
      | i >= 0    = intToWord64 i
      | otherwise 
      = toEnumError "Word64" i (minBound::Word64,maxBound::Word64)

    fromEnum w
      | w <= intToWord64 (maxBound::Int)
      = word64ToInt w
      | otherwise
      = fromEnumError "Word64" w

    enumFrom e1        = map integerToWord64 [word64ToInteger e1 .. word64ToInteger maxBound]
    enumFromTo e1 e2   = map integerToWord64 [word64ToInteger e1 .. word64ToInteger e2]
    enumFromThen e1 e2 = map integerToWord64 [word64ToInteger e1, word64ToInteger e2 .. word64ToInteger last]
		       where 
			  last :: Word64
		          last 
			   | e2 < e1   = minBound
			   | otherwise = maxBound

    enumFromThenTo e1 e2 e3 = map integerToWord64 [word64ToInteger e1, word64ToInteger e2 .. word64ToInteger e3]

instance Show Word64 where
  showsPrec p x = showsPrec p (word64ToInteger x)

instance Read Word64 where
  readsPrec _ s = [ (integerToWord64 x,r) | (x,r) <- readDec s ]

instance Ix Word64 where
    range (m,n)          = [m..n]
    index b@(m,_) i
	   | inRange b i = word64ToInt (i-m)
	   | otherwise   = indexError i b "Word64"
    inRange (m,n) i      = m <= i && i <= n

instance Bounded Word64 where
  minBound = 0
  maxBound = minBound - 1

instance Real Word64 where
  toRational x = toInteger x % 1

sizeofWord64 :: Word32
sizeofWord64 = 8
\end{code}


#ifndef __HUGS__

The Hugs-GHC extension libraries provide functions for going between
Int and the various (un)signed ints. Here we provide the same for
the GHC specific Word type:

\begin{code}
word8ToWord  (W8#  w#) = W# w#
wordToWord8  (W#   w#)  = W8# (w# `and#` (case (maxBound::Word8) of W8#   x# -> x#))

word16ToWord (W16# w#) = W# w#
wordToWord16 (W#   w#) = W16# (w# `and#` (case (maxBound::Word16) of W16# x# -> x#))

word32ToWord (W32# w#) = W# w#
wordToWord32 (W#   w#) = W32# (w# `and#` (case (maxBound::Word32) of W32# x# -> x#))

wordToWord64 (W#   w#) = W64# (wordToWord64# w#)
-- lossy on 32-bit platforms, but provided nontheless.
word64ToWord (W64# w#) = W#   (word64ToWord# w#)
\end{code}


--End of exported definitions

The remainder of this file consists of definitions which are only
used in the implementation.

\begin{code}
signumReal :: (Ord a, Num a) => a -> a
signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1
\end{code}

NOTE: the index is in units of the size of the type, *not* bytes.

\begin{code}
indexWord8OffAddr  :: Addr -> Int -> Word8
indexWord8OffAddr (A# a#) (I# i#) = intToWord8 (I# (ord# (indexCharOffAddr# a# i#)))

indexWord16OffAddr :: Addr -> Int -> Word16
indexWord16OffAddr a i =
#ifdef WORDS_BIGENDIAN
  intToWord16 ( word8ToInt l + (word8ToInt maxBound) * word8ToInt h)
#else
  intToWord16 ( word8ToInt h + (word8ToInt maxBound) * word8ToInt l)
#endif
 where
   byte_idx = i * 2
   l = indexWord8OffAddr a byte_idx
   h = indexWord8OffAddr a (byte_idx+1)

indexWord32OffAddr :: Addr -> Int -> Word32
indexWord32OffAddr (A# a#) i = wordToWord32 (W# (indexWordOffAddr# a# i'#))
 where
   -- adjust index to be in Word units, not Word32 ones.
  (I# i'#) 
#if WORD_SIZE_IN_BYTES==8
   = i `div` 2
#else
   = i
#endif

indexWord64OffAddr :: Addr -> Int -> Word64
indexWord64OffAddr (A# a#) (I# i#)
#if WORD_SIZE_IN_BYTES==8
 = W64# (indexWordOffAddr# a# i#)
#else
 = W64# (indexWord64OffAddr# a# i#)
#endif

#ifndef __PARALLEL_HASKELL__

indexWord8OffForeignObj  :: ForeignObj -> Int -> Word8
indexWord8OffForeignObj (ForeignObj fo#) (I# i#) = intToWord8 (I# (ord# (indexCharOffForeignObj# fo# i#)))

indexWord16OffForeignObj :: ForeignObj -> Int -> Word16
indexWord16OffForeignObj fo i =
#ifdef WORDS_BIGENDIAN
  intToWord16 ( word8ToInt l + (word8ToInt maxBound) * word8ToInt h)
#else
  intToWord16 ( word8ToInt h + (word8ToInt maxBound) * word8ToInt l)
#endif
 where
   byte_idx = i * 2
   l = indexWord8OffForeignObj fo byte_idx
   h = indexWord8OffForeignObj fo (byte_idx+1)

indexWord32OffForeignObj :: ForeignObj -> Int -> Word32
indexWord32OffForeignObj (ForeignObj fo#) i = wordToWord32 (W# (indexWordOffForeignObj# fo# i'#))
 where
   -- adjust index to be in Word units, not Word32 ones.
  (I# i'#) 
#if WORD_SIZE_IN_BYTES==8
   = i `div` 2
#else
   = i
#endif

indexWord64OffForeignObj :: ForeignObj -> Int -> Word64
indexWord64OffForeignObj (ForeignObj fo#) (I# i#)
#if WORD_SIZE_IN_BYTES==8
 = W64# (indexWordOffForeignObj# fo# i#)
#else
 = W64# (indexWord64OffForeignObj# fo# i#)
#endif
#endif

\end{code}

Read words out of mutable memory:

\begin{code}
readWord8OffAddr :: Addr -> Int -> IO Word8
readWord8OffAddr a i = _casm_ `` %r=((StgWord8*)%0)[(StgInt)%1]; '' a i

readWord16OffAddr  :: Addr -> Int -> IO Word16
readWord16OffAddr a i = _casm_ `` %r=((StgWord16*)%0)[(StgInt)%1]; '' a i

readWord32OffAddr  :: Addr -> Int -> IO Word32
readWord32OffAddr a i = _casm_ `` %r=((StgWord32*)%0)[(StgInt)%1]; '' a i

readWord64OffAddr  :: Addr -> Int -> IO Word64
#if WORD_SIZE_IN_BYTES==8
readWord64OffAddr a i = _casm_ `` %r=((StgWord*)%0)[(StgInt)%1]; '' a i
#else
readWord64OffAddr a i = _casm_ `` %r=((StgWord64*)%0)[(StgInt)%1]; '' a i
#endif

#ifndef __PARALLEL_HASKELL__
readWord8OffForeignObj :: ForeignObj -> Int -> IO Word8
readWord8OffForeignObj fo i = _casm_ `` %r=((StgWord8*)%0)[(StgInt)%1]; '' fo i

readWord16OffForeignObj  :: ForeignObj -> Int -> IO Word16
readWord16OffForeignObj fo i = _casm_ `` %r=((StgWord16*)%0)[(StgInt)%1]; '' fo i

readWord32OffForeignObj  :: ForeignObj -> Int -> IO Word32
readWord32OffForeignObj fo i = _casm_ `` %r=((StgWord32*)%0)[(StgInt)%1]; '' fo i

readWord64OffForeignObj  :: ForeignObj -> Int -> IO Word64
#if WORD_SIZE_IN_BYTES==8
readWord64OffForeignObj fo i = _casm_ `` %r=((StgWord*)%0)[(StgInt)%1]; '' fo i
#else
readWord64OffForeignObj fo i = _casm_ `` %r=((StgWord64*)%0)[(StgInt)%1]; '' fo i
#endif

#endif 

\end{code}

Note: we provide primops for the writing via Addrs since that's used
in the IO implementation (a place where we *really* do care about cycles.)

\begin{code}
writeWord8OffAddr  :: Addr -> Int -> Word8  -> IO ()
writeWord8OffAddr (A# a#) (I# i#) (W8# w#) = IO $ \ s# ->
      case (writeCharOffAddr# a# i# (chr# (word2Int# w#)) s#) of s2# -> (# s2#, () #)

writeWord16OffAddr :: Addr -> Int -> Word16 -> IO ()
writeWord16OffAddr a i e = _casm_ `` (((StgWord16*)%0)[(StgInt)%1])=(StgWord16)%2; '' a i e

writeWord32OffAddr :: Addr -> Int -> Word32 -> IO ()
writeWord32OffAddr (A# a#) i (W32# w#) = IO $ \ s# ->
      case (writeWordOffAddr#  a# i'# w# s#) of s2# -> (# s2#, () #)
 where
   -- adjust index to be in Word units, not Word32 ones.
  (I# i'#) 
#if WORD_SIZE_IN_BYTES==8
   = i `div` 2
#else
   = i
#endif

writeWord64OffAddr :: Addr -> Int -> Word64 -> IO ()
#if WORD_SIZE_IN_BYTES==8
writeWord64OffAddr (A# a#) (I# i#) (W64# w#) = IO $ \ s# ->
      case (writeWordOffAddr#  a# i# w# s#) of s2# -> (# s2#, () #)
#else
writeWord64OffAddr (A# a#) (I# i#) (W64# w#) = IO $ \ s# ->
      case (writeWord64OffAddr#  a# i# w# s#) of s2# -> (# s2#, () #)
#endif

#ifndef __PARALLEL_HASKELL__

writeWord8OffForeignObj  :: ForeignObj -> Int -> Word8  -> IO ()
writeWord8OffForeignObj fo i w = _casm_ `` (((StgWord8*)%0)[(StgInt)%1])=(StgWord8)%2; '' fo i w

writeWord16OffForeignObj :: ForeignObj -> Int -> Word16 -> IO ()
writeWord16OffForeignObj fo i w = _casm_ `` (((StgWord16*)%0)[(StgInt)%1])=(StgWord16)%2; '' fo i w

writeWord32OffForeignObj :: ForeignObj -> Int -> Word32 -> IO ()
writeWord32OffForeignObj fo i w = _casm_ `` (((StgWord32*)%0)[(StgInt)%1])=(StgWord32)%2; '' fo i' w
 where
   -- adjust index to be in Word units, not Word32 ones.
  i' 
#if WORD_SIZE_IN_BYTES==8
   = i `div` 2
#else
   = i
#endif

writeWord64OffForeignObj :: ForeignObj -> Int -> Word64 -> IO ()
# if WORD_SIZE_IN_BYTES==8
writeWord64OffForeignObj fo i e = _casm_ `` (((StgWord*)%0)[(StgInt)%1])=(StgWord)%2; '' fo i e
# else
writeWord64OffForeignObj fo i e = _casm_ `` (((StgWord64*)%0)[(StgInt)%1])=(StgWord64)%2; '' fo i e
# endif

#endif

\end{code}

#endif

Utils for generating friendly error messages.

\begin{code}
{-# NOINLINE indexError #-}
indexError :: (Show a) => a -> (a,a) -> String -> b
indexError i rng tp
  = error (showString "Ix{" . showString tp . showString "}.index: Index " .
           showParen True (showsPrec 0 i) .
	   showString " out of range " $
	   showParen True (showsPrec 0 rng) "")

toEnumError :: (Show a,Show b) => String -> a -> (b,b) -> c
toEnumError inst_ty tag bnds
  = error ("Enum.toEnum{" ++ inst_ty ++ "}: tag " ++
           (showParen True (showsPrec 0 tag) $
	     " is outside of bounds " ++
	     show bnds))

fromEnumError :: (Show a,Show b) => String -> a -> b
fromEnumError inst_ty tag
  = error ("Enum.fromEnum{" ++ inst_ty ++ "}: value " ++
           (showParen True (showsPrec 0 tag) $
	     " is outside of Int's bounds " ++
	     show (minBound::Int,maxBound::Int)))

succError :: String -> a
succError inst_ty
  = error ("Enum.succ{" ++ inst_ty ++ "}: tried to take `succ' of maxBound")

predError :: String -> a
predError inst_ty
  = error ("Enum.pred{" ++ inst_ty ++ "}: tried to take `pred' of minBound")

divZeroError :: (Show a) => String -> a -> b
divZeroError meth v 
  = error ("Integral." ++ meth ++ ": divide by 0 (" ++ show v ++ " / 0)")

\end{code}


\begin{code}
#ifdef __HUGS__
-- Here is the Hugs versions of some functions.

writeWord8OffAddr :: Addr -> Int -> Word8 -> IO ()
writeWord8OffAddr = error "TODO: writeWord8OffAddr"
readWord8OffAddr :: Addr -> Int -> IO Word8
readWord8OffAddr = error "TODO: readWord8OffAddr"
indexWord8OffAddr :: Addr -> Int -> Word8
indexWord8OffAddr = error "TODO: indexWord8OffAddr"

writeWord16OffAddr :: Addr -> Int -> Word16 -> IO ()
writeWord16OffAddr = error "TODO: writeWord16OffAddr"
readWord16OffAddr :: Addr -> Int -> IO Word16
readWord16OffAddr = error "TODO: readWord16OffAddr"
indexWord16OffAddr :: Addr -> Int -> Word16
indexWord16OffAddr = error "TODO: indexWord16OffAddr"

writeWord32OffAddr :: Addr -> Int -> Word32 -> IO ()
writeWord32OffAddr = error "TODO: writeWord32OffAddr"
readWord32OffAddr :: Addr -> Int -> IO Word32
readWord32OffAddr = error "TODO: readWord32OffAddr"
indexWord32OffAddr :: Addr -> Int -> Word32
indexWord32OffAddr = error "TODO: indexWord32OffAddr"

writeWord64OffAddr :: Addr -> Int -> Word64 -> IO ()
writeWord64OffAddr = error "TODO: writeWord64OffAddr"
readWord64OffAddr :: Addr -> Int -> IO Word64
readWord64OffAddr = error "TODO: readWord64OffAddr"
indexWord64OffAddr :: Addr -> Int -> Word64
indexWord64OffAddr = error "TODO: indexWord64OffAddr"

intToWord64 = error "TODO: intToWord64"
word64ToInt = error "TODO: word64ToInt"

word64ToWord32 = error "TODO: word64ToWord32"
word64ToWord16 = error "TODO: word64ToWord16"
word64ToWord8 = error "TODO: word64ToWord8"

word32ToWord64 = error "TODO: word32ToWord64"
word16ToWord64 = error "TODO: word16ToWord64"
word8ToWord64 = error "TODO: word64ToWord64"
#endif
\end{code}


