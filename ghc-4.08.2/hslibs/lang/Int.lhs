%
% (c) The AQUA Project, Glasgow University, 1997-1999
%
\section[Int]{Module @Int@}

This code is largely copied from the Hugs library of the same name,
suitably hammered to use unboxed types.

\begin{code}
#include "MachDeps.h"

module Int
	( Int8
	, Int16
	, Int32
	, Int64
	-- instances: Eq, Ord, Num, Bounded, Real, Integral, Ix, Enum, Read,
	-- Show, Bits, CCallable, CReturnable (last two are GHC specific.)

        -- SUP: deprecated in the new FFI, subsumed by fromIntegral
	, intToInt8      -- :: Int     -> Int8
	, intToInt16     -- :: Int     -> Int16
	, intToInt32     -- :: Int     -> Int32
	, intToInt64     -- :: Int     -> Int64

        , integerToInt8  -- :: Integer -> Int8
        , integerToInt16 -- :: Integer -> Int16
        , integerToInt32 -- :: Integer -> Int32
        , integerToInt64 -- :: Integer -> Int64

	, int8ToInt      -- :: Int8    -> Int
        , int8ToInteger  -- :: Int8    -> Integer
        , int8ToInt16    -- :: Int8    -> Int16
        , int8ToInt32    -- :: Int8    -> Int32
        , int8ToInt64    -- :: Int8    -> Int64

	, int16ToInt     -- :: Int16   -> Int
        , int16ToInteger -- :: Int16   -> Integer
        , int16ToInt8    -- :: Int16   -> Int8
        , int16ToInt32   -- :: Int16   -> Int32
        , int16ToInt64   -- :: Int16   -> Int64

	, int32ToInt     -- :: Int32   -> Int
        , int32ToInteger -- :: Int32   -> Integer
        , int32ToInt8    -- :: Int32   -> Int8
        , int32ToInt16   -- :: Int32   -> Int16
        , int32ToInt64   -- :: Int32   -> Int64

	, int64ToInt     -- :: Int64   -> Int
        , int64ToInteger -- :: Int64   -> Integer
        , int64ToInt8    -- :: Int64   -> Int8
        , int64ToInt16   -- :: Int64   -> Int16
        , int64ToInt32   -- :: Int64   -> Int32

#ifndef __HUGS__
	-- The "official" place to get these from is Addr, importing
	-- them from Int is a non-standard thing to do.
        -- SUP: deprecated in the new FFI, subsumed by the Storable class
	, indexInt8OffAddr
	, indexInt16OffAddr
	, indexInt32OffAddr
	, indexInt64OffAddr
	
	, readInt8OffAddr
	, readInt16OffAddr
	, readInt32OffAddr
	, readInt64OffAddr
	
	, writeInt8OffAddr
	, writeInt16OffAddr
	, writeInt32OffAddr
	, writeInt64OffAddr
#endif
	
        -- SUP: deprecated in the new FFI, subsumed by the Storable class
	, sizeofInt8
	, sizeofInt16
	, sizeofInt32
	, sizeofInt64
	
	-- The "official" place to get these from is Foreign
        -- SUP: deprecated in the new FFI, subsumed by the Storable class
        -- + foreignObjToAddr
#ifndef __PARALLEL_HASKELL__
#ifndef __HUGS__
	, indexInt8OffForeignObj
	, indexInt16OffForeignObj
	, indexInt32OffForeignObj
	, indexInt64OffForeignObj

	, readInt8OffForeignObj
	, readInt16OffForeignObj
	, readInt32OffForeignObj
	, readInt64OffForeignObj

	, writeInt8OffForeignObj
	, writeInt16OffForeignObj
	, writeInt32OffForeignObj
	, writeInt64OffForeignObj
#endif
#endif
	
	-- The non-standard fromInt and toInt methods
	, Num( fromInt ), Integral( toInt )

#ifndef __HUGS__
	-- non-standard, GHC specific
	, intToWord
#endif

	) where

#ifndef __HUGS__
import PrelBase
import CCall
import PrelIOBase	( ForeignObj(..) )
import PrelAddr		( Int64(..), Word64(..), Addr(..), Word(..), intToWord )
import PrelNum		( Num(fromInt), Integer(..) )
import PrelReal		( Integral(toInt) )
#else
import PrelPrim		( Word
			, absReal, signumReal
			, primAndInt, primOrInt, primXorInt
			, primShiftLInt, primShiftRAInt
			, primIntToWord
			)
import Word
#endif
import Ix
import Bits
import Ratio		( (%) )
import Numeric		( readDec )
import Word		( Word32 )
\end{code}

\begin{code}
-----------------------------------------------------------------------------
-- The "official" coercion functions
-----------------------------------------------------------------------------

intToInt8      :: Int     -> Int8
intToInt16     :: Int     -> Int16
intToInt32     :: Int     -> Int32
intToInt64     :: Int     -> Int64

integerToInt8  :: Integer -> Int8
integerToInt16 :: Integer -> Int16
integerToInt32 :: Integer -> Int32
integerToInt64 :: Integer -> Int64

int8ToInt      :: Int8    -> Int
int8ToInteger  :: Int8    -> Integer
int8ToInt16    :: Int8    -> Int16
int8ToInt32    :: Int8    -> Int32
int8ToInt64    :: Int8    -> Int64

int16ToInt     :: Int16   -> Int
int16ToInteger :: Int16   -> Integer
int16ToInt8    :: Int16   -> Int8
int16ToInt32   :: Int16   -> Int32
int16ToInt64   :: Int16   -> Int64

int32ToInt     :: Int32   -> Int
int32ToInteger :: Int32   -> Integer
int32ToInt8    :: Int32   -> Int8
int32ToInt16   :: Int32   -> Int16
int32ToInt64   :: Int32   -> Int64

int64ToInt     :: Int64   -> Int
int64ToInteger :: Int64   -> Integer
int64ToInt8    :: Int64   -> Int8
int64ToInt16   :: Int64   -> Int16
int64ToInt32   :: Int64   -> Int32

integerToInt8  = fromInteger
integerToInt16 = fromInteger
integerToInt32 = fromInteger

#ifndef __HUGS__

int8ToInt16  (I8#  x) = I16# x
int8ToInt32  (I8#  x) = I32# x

int16ToInt8  (I16# x) = I8#  x
int16ToInt32 (I16# x) = I32# x

int32ToInt8  (I32# x) = I8#  x
int32ToInt16 (I32# x) = I16# x

#else

int8ToInt16  (I8  x) = I16 x
int8ToInt32  (I8  x) = I32 x

int16ToInt8  (I16 x) = I8  x
int16ToInt32 (I16 x) = I32 x

int32ToInt8  (I32 x) = I8  x
int32ToInt16 (I32 x) = I16 x

#endif

int8ToInteger  = toInteger
int8ToInt64    = int32ToInt64 . int8ToInt32

int16ToInteger = toInteger
int16ToInt64   = int32ToInt64 . int16ToInt32

int32ToInteger = toInteger

int64ToInt8    = int32ToInt8  . int64ToInt32
int64ToInt16   = int32ToInt16 . int64ToInt32

-----------------------------------------------------------------------------
-- The following rules for fromIntegral remove the need to export specialized
-- conversion functions.
-----------------------------------------------------------------------------

{-# RULES
   "fromIntegral/Int->Int8"         fromIntegral = intToInt8;
   "fromIntegral/Int->Int16"        fromIntegral = intToInt16;
   "fromIntegral/Int->Int32"        fromIntegral = intToInt32;
   "fromIntegral/Int->Int64"        fromIntegral = intToInt64;

   "fromIntegral/Integer->Int8"     fromIntegral = integerToInt8;
   "fromIntegral/Integer->Int16"    fromIntegral = integerToInt16;
   "fromIntegral/Integer->Int32"    fromIntegral = integerToInt32;
   "fromIntegral/Integer->Int64"    fromIntegral = integerToInt64;

   "fromIntegral/Int8->Int"         fromIntegral = int8ToInt;
   "fromIntegral/Int8->Integer"     fromIntegral = int8ToInteger;
   "fromIntegral/Int8->Int16"       fromIntegral = int8ToInt16;
   "fromIntegral/Int8->Int32"       fromIntegral = int8ToInt32;
   "fromIntegral/Int8->Int64"       fromIntegral = int8ToInt64;

   "fromIntegral/Int16->Int"        fromIntegral = int16ToInt;
   "fromIntegral/Int16->Integer"    fromIntegral = int16ToInteger;
   "fromIntegral/Int16->Int8"       fromIntegral = int16ToInt8;
   "fromIntegral/Int16->Int32"      fromIntegral = int16ToInt32;
   "fromIntegral/Int16->Int64"      fromIntegral = int16ToInt64;

   "fromIntegral/Int32->Int"        fromIntegral = int32ToInt;
   "fromIntegral/Int32->Integer"    fromIntegral = int32ToInteger;
   "fromIntegral/Int32->Int8"       fromIntegral = int32ToInt8;
   "fromIntegral/Int32->Int16"      fromIntegral = int32ToInt16;
   "fromIntegral/Int32->Int64"      fromIntegral = int32ToInt64;

   "fromIntegral/Int64->Int"        fromIntegral = int64ToInt;
   "fromIntegral/Int64->Integer"    fromIntegral = int64ToInteger;
   "fromIntegral/Int64->Int8"       fromIntegral = int64ToInt8;
   "fromIntegral/Int64->Int16"      fromIntegral = int64ToInt16;
   "fromIntegral/Int64->Int32"      fromIntegral = int64ToInt32
 #-}

\end{code}

\subsection[Int8]{The @Int8@ interface}

\begin{code}
#ifndef __HUGS__
data Int8 = I8# Int#
#else
newtype Int8 = I8 { unI8 :: Int } deriving (Eq, Ord)
mkI8 = intToInt8
#endif

#ifndef __HUGS__
instance CCallable Int8
instance CReturnable Int8

int8ToInt (I8# x)  = I# (i8ToInt# x)

i8ToInt# :: Int# -> Int#
i8ToInt# x = if x' <=# 0x7f# then x' else x' -# 0x100#
   where x' = word2Int# (int2Word# x `and#` int2Word# 0xff#)

-- This doesn't perform any bounds checking on the value it is passed,
-- nor its sign, i.e., show (intToInt8 511) => "-1"
intToInt8 (I# x) = I8# (intToInt8# x)

intToInt8# :: Int# -> Int#
intToInt8# i# = word2Int# ((int2Word# i#) `and#` int2Word# 0xff#)

instance Eq  Int8     where 
  (I8# x#) == (I8# y#) = x# ==# y#
  (I8# x#) /= (I8# y#) = x# /=# y#

instance Ord Int8 where 
  compare (I8# x#) (I8# y#) = compareInt# (i8ToInt# x#) (i8ToInt# y#)

compareInt# :: Int# -> Int# -> Ordering
compareInt# x# y#
 | x# <#  y# = LT
 | x# ==# y# = EQ
 | otherwise = GT

instance Num Int8 where
  (I8# x#) + (I8# y#) = I8# (intToInt8# (x# +# y#))
  (I8# x#) - (I8# y#) = I8# (intToInt8# (x# -# y#))
  (I8# x#) * (I8# y#) = I8# (intToInt8# (x# *# y#))
  negate i@(I8# x#) = 
     if x# ==# 0#
      then i
      else I8# (0x100# -# x#)

  abs           = absReal
  signum        = signumReal
  fromInteger (S# i#)    = I8# (intToInt8# i#)
  fromInteger (J# s# d#) = I8# (intToInt8# (integer2Int# s# d#))
  fromInt       = intToInt8
#else
int8ToInt (I8 i) = i
intToInt8 x = if x' <= 0x7f then I8 x' else I8 (x' - 0x100)
   where x' = x `primAndInt` 0xff

instance Num Int8 where
    (+) x y       = binop8 (+) x y
    (-) x y       = binop8 (-) x y
    negate        = mkI8 . negate . unI8
    (*) x y       = binop8 (*) x y
    abs           = absReal
    signum        = signumReal
    fromInteger   = intToInt8 . toInt
    fromInt       = intToInt8

binop8 op x y = mkI8 (unI8 x `op` unI8 y)
#endif

instance Bounded Int8 where
    minBound = 0x80
    maxBound = 0x7f 

instance Real Int8 where
    toRational x = toInteger x % 1

instance Integral Int8 where
    div x y
       | x > 0 && y < 0 = quotInt8 (x-y-1) y
       | x < 0 && y > 0	= quotInt8 (x-y+1) y
       | otherwise      = quotInt8 x y
#ifndef __HUGS__
    quot x@(I8# _) y@(I8# y#)
       | y# /=# 0# = x `quotInt8` y
       | otherwise = divZeroError "quot{Int8}" x
    rem x@(I8# _) y@(I8# y#)
       | y# /=# 0#  = x `remInt8` y
       | otherwise  = divZeroError "rem{Int8}" x
#else
    quot x y
       | y /= 0 = x `quotInt8` y
       | otherwise  = divZeroError "quot{Int8}" x
    rem x y
       | y /= 0  = x `remInt8` y
       | otherwise  = divZeroError "rem{Int8}" x
#endif
    mod x y
       | x > 0 && y < 0 || x < 0 && y > 0 = if r/=0 then r+y else 0
       | otherwise = r
	where r = remInt8 x y

#ifndef __HUGS__
    a@(I8# _) `quotRem` b@(I8# _) = (a `quotInt8` b, a `remInt8` b)
#else
    a `quotRem` b = (a `quot` b, a `rem` b)
#endif
    toInteger i8  = toInteger (int8ToInt i8)
    toInt     i8  = int8ToInt i8


remInt8, quotInt8 :: Int8 -> Int8 -> Int8
#ifndef __HUGS__
remInt8  (I8# x) (I8# y) = I8# (intToInt8# ((i8ToInt# x) `remInt#`  (i8ToInt# y)))
quotInt8 (I8# x) (I8# y) = I8# (intToInt8# ((i8ToInt# x) `quotInt#` (i8ToInt# y)))
#else
remInt8 = binop8 rem 
quotInt8 = binop8 quot
#endif

instance Ix Int8 where
    range (m,n)          = [m..n]
    index b@(m,_) i
	      | inRange b i = int8ToInt (i - m)
	      | otherwise   = indexError i b "Int8"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int8 where
    succ i
      | i == maxBound = succError "Int8"
      | otherwise     = i+1
    pred i
      | i == minBound = predError "Int8"
      | otherwise     = i-1

    toEnum x
      | x >= toInt (minBound::Int8) && x <= toInt (maxBound::Int8) 
      = intToInt8 x
      | otherwise
      = toEnumError "Int8" x (minBound::Int8,maxBound::Int8)

    fromEnum           = int8ToInt
    enumFrom e1        = map toEnum [fromEnum e1 .. fromEnum (maxBound::Int8)]
    enumFromThen e1 e2 = 
             map toEnum [fromEnum e1, fromEnum e2 .. fromEnum (last::Int8)]
		where 
		   last 
		     | e2 < e1   = minBound
		     | otherwise = maxBound

instance Read Int8 where
    readsPrec p s = [ (intToInt8 x,r) | (x,r) <- readsPrec p s ]

instance Show Int8 where
    showsPrec p i8 = showsPrec p (int8ToInt i8)

#ifndef __HUGS__
binop8 :: (Int32 -> Int32 -> a) -> (Int8 -> Int8 -> a)
binop8 op x y = int8ToInt32 x `op` int8ToInt32 y

instance Bits Int8 where
  (I8# x) .&. (I8# y) = I8# (word2Int# ((int2Word# x) `and#` (int2Word# y)))
  (I8# x) .|. (I8# y) = I8# (word2Int# ((int2Word# x) `or#`  (int2Word# y)))
  (I8# x) `xor` (I8# y) = I8# (word2Int# ((int2Word# x) `xor#` (int2Word# y)))
  complement (I8# x)    = I8# (word2Int# ((int2Word# x) `xor#` (int2Word# 0xff#)))
  shift (I8# x) i@(I# i#)
	| i > 0     = I8# (intToInt8# (iShiftL# (i8ToInt# x)  i#))
	| otherwise = I8# (intToInt8# (iShiftRA# (i8ToInt# x) (negateInt# i#)))
  i8@(I8# x)  `rotate` (I# i)
        | i ==# 0#    = i8
	| i ># 0#     = 
	     I8# (intToInt8# ( word2Int#  (
	             (int2Word# (iShiftL# (i8ToInt# x) i'))
		             `or#`
                     (int2Word# (iShiftRA# (word2Int# (
		                                (int2Word# x) `and#` 
			                        (int2Word# (0x100# -# pow2# i2))))
			                  i2)))))
	| otherwise = rotate i8 (I# (8# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 7#)
           i2 = 8# -# i'
  bit i         = shift 1 i
  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i   = (x .&. bit i) /= 0
  bitSize  _    = 8
  isSigned _    = True

pow2# :: Int# -> Int#
pow2# x# = iShiftL# 1# x#

pow2_64# :: Int# -> Int64#
pow2_64# x# = word64ToInt64# (shiftL64# (wordToWord64# (int2Word# 1#)) x#)
#else

instance Bits Int8 where
  (.&.) x y    	= binop8 primAndInt x y
  (.|.) x y    	= binop8 primOrInt x y
  xor x y      	= binop8 primXorInt x y
  complement  	= xor ((-1) :: Int8) 
  x `shift` i   | i == 0 = x
		| i > 0  && i < 8 
			 = mkI8 (primShiftLInt (unI8 x) i)
		| i < 0  && i > -8
			 = mkI8 (primShiftRAInt (unI8 x) (-1))
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
sizeofInt8 :: Word32
sizeofInt8 = 1
\end{code}


\subsection[Int16]{The @Int16@ interface}

\begin{code}
#ifndef __HUGS__
data Int16  = I16# Int#
#else
newtype Int16 = I16 { unI16 :: Int } deriving (Eq, Ord)
mkI16 = intToInt16
#endif

#ifndef __HUGS__
instance CCallable Int16
instance CReturnable Int16

int16ToInt  (I16# x) = I# (i16ToInt# x)

i16ToInt# :: Int# -> Int#
i16ToInt# x = if x' <=# 0x7fff# then x' else x' -# 0x10000#
   where x' = word2Int# (int2Word# x `and#` int2Word# 0xffff#)

-- This doesn't perform any bounds checking on the value it is passed,
-- nor its sign, i.e., show (intToInt8 131071) => "-1"
intToInt16 (I# x) = I16# (intToInt16# x)

intToInt16# :: Int# -> Int#
intToInt16# i# = word2Int# ((int2Word# i#) `and#` int2Word# 0xffff#)

instance Eq  Int16     where
  (I16# x#) == (I16# y#) = x# ==# y#
  (I16# x#) /= (I16# y#) = x# /=# y#

instance Ord Int16 where
  compare (I16# x#) (I16# y#) = compareInt# (i16ToInt# x#) (i16ToInt# y#)

instance Num Int16 where
  (I16# x#) + (I16# y#) = I16# (intToInt16# (x# +# y#))
  (I16# x#) - (I16# y#) = I16# (intToInt16# (x# -# y#))
  (I16# x#) * (I16# y#) = I16# (intToInt16# (x# *# y#))
  negate i@(I16# x#) = 
     if x# ==# 0#
      then i
      else I16# (0x10000# -# x#)
  abs           = absReal
  signum        = signumReal
  fromInteger (S# i#)    = I16# (intToInt16# i#)
  fromInteger (J# s# d#) = I16# (intToInt16# (integer2Int# s# d#))
  fromInt       = intToInt16
#else
int16ToInt (I16 i) = i
intToInt16 x = if x' <= 0x7fff then I16 x' else I16 (x' - 0x10000)
   where x' = x `primAndInt` 0xffff

instance Num Int16 where
    (+) x y       = binop16 (+) x y
    (-) x y       = binop16 (-) x y
    negate        = mkI16 . negate . unI16
    (*) x y       = binop16 (*) x y
    abs           = absReal
    signum        = signumReal
    fromInteger   = intToInt16 . toInt
    fromInt       = intToInt16

binop16 op x y = mkI16 (unI16 x `op` unI16 y)
#endif

instance Bounded Int16 where
    minBound = 0x8000
    maxBound = 0x7fff 

instance Real Int16 where
    toRational x = toInteger x % 1

instance Integral Int16 where
    div x y
       | x > 0 && y < 0	= quotInt16 (x-y-1) y
       | x < 0 && y > 0	= quotInt16 (x-y+1) y
       | otherwise	= quotInt16 x y
#ifndef __HUGS__
    quot x@(I16# _) y@(I16# y#)
       | y# /=# 0#      = x `quotInt16` y
       | otherwise      = divZeroError "quot{Int16}" x
    rem x@(I16# _) y@(I16# y#)
       | y# /=# 0#      = x `remInt16` y
       | otherwise      = divZeroError "rem{Int16}" x
#else
    quot x y
       | y /= 0 = x `quotInt16` y
       | otherwise  = divZeroError "quot{Int16}" x
    rem x y
       | y /= 0  = x `remInt16` y
       | otherwise  = divZeroError "rem{Int16}" x
#endif
    mod x y
       | x > 0 && y < 0 || x < 0 && y > 0 = if r/=0 then r+y else 0
       | otherwise			  = r
	where r = remInt16 x y

#ifndef __HUGS__
    a@(I16# _) `quotRem` b@(I16# _) = (a `quotInt16` b, a `remInt16` b)
#else
    a `quotRem` b = (a `quot` b, a `rem` b)
#endif
    toInteger i16  = toInteger (int16ToInt i16)
    toInt     i16  = int16ToInt i16

#ifndef __HUGS__
remInt16, quotInt16 :: Int16 -> Int16 -> Int16
remInt16  (I16# x) (I16# y) = I16# (intToInt16# ((i16ToInt# x) `remInt#` (i16ToInt# y)))
quotInt16 (I16# x) (I16# y) = I16# (intToInt16# ((i16ToInt# x) `quotInt#` (i16ToInt# y)))
#else
remInt16 = binop16 rem 
quotInt16 = binop16 quot
#endif

instance Ix Int16 where
    range (m,n)          = [m..n]
    index b@(m,_) i
	      | inRange b i = int16ToInt (i - m)
	      | otherwise   = indexError i b "Int16"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int16 where
    succ i
      | i == maxBound = succError "Int16"
      | otherwise     = i+1

    pred i
      | i == minBound = predError "Int16"
      | otherwise     = i-1

    toEnum x
      | x >= toInt (minBound::Int16) && x <= toInt (maxBound::Int16) 
      = intToInt16 x
      | otherwise
      = toEnumError "Int16" x (minBound::Int16, maxBound::Int16)

    fromEnum         = int16ToInt

    enumFrom e1        = map toEnum [fromEnum e1 .. fromEnum (maxBound::Int16)]
    enumFromThen e1 e2 = map toEnum [fromEnum e1, fromEnum e2 .. fromEnum (last::Int16)]
			  where last 
			          | e2 < e1   = minBound
				  | otherwise = maxBound

instance Read Int16 where
    readsPrec p s = [ (intToInt16 x,r) | (x,r) <- readsPrec p s ]

instance Show Int16 where
    showsPrec p i16 = showsPrec p (int16ToInt i16)

#ifndef __HUGS__

binop16 :: (Int32 -> Int32 -> a) -> (Int16 -> Int16 -> a)
binop16 op x y = int16ToInt32 x `op` int16ToInt32 y

instance Bits Int16 where
  (I16# x) .&. (I16# y) = I16# (word2Int# ((int2Word# x) `and#` (int2Word# y)))
  (I16# x) .|. (I16# y) = I16# (word2Int# ((int2Word# x) `or#`  (int2Word# y)))
  (I16# x) `xor` (I16# y) = I16# (word2Int# ((int2Word# x) `xor#`  (int2Word# y)))
  complement (I16# x)    = I16# (word2Int# ((int2Word# x) `xor#` (int2Word# 0xffff#)))
  shift (I16# x) i@(I# i#)
	| i > 0     = I16# (intToInt16# (iShiftL# (i16ToInt# x)  i#))
	| otherwise = I16# (intToInt16# (iShiftRA# (i16ToInt# x) (negateInt# i#)))
  i16@(I16# x)  `rotate` (I# i)
        | i ==# 0#    = i16
	| i ># 0#     = 
	     I16# (intToInt16# (word2Int# (
	            (int2Word# (iShiftL# (i16ToInt# x) i')) 
		             `or#`
                    (int2Word# (iShiftRA# ( word2Int# (
		                    (int2Word# x) `and#` (int2Word# (0x100# -# pow2# i2))))
			                  i2)))))
	| otherwise = rotate i16 (I# (16# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 15#)
           i2 = 16# -# i'
  bit i             = shift 1 i
  setBit x i        = x .|. bit i
  clearBit x i      = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i       = (x .&. bit i) /= 0
  bitSize  _        = 16
  isSigned _        = True
#else
instance Bits Int16 where
  (.&.) x y    	= binop16 primAndInt x y
  (.|.) x y    	= binop16 primOrInt x y
  xor x y      	= binop16 primXorInt x y
  complement  	= xor ((-1) :: Int16) 
  x `shift` i   | i == 0 = x
		| i > 0  && i < 16 
			 = mkI16 (primShiftLInt (unI16 x) i)
		| i < 0  && i > -16
			 = mkI16 (primShiftRAInt (unI16 x) (-1))
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
sizeofInt16 :: Word32
sizeofInt16 = 2
\end{code}

%
%
\subsection[Int32]{The @Int32@ interface}
%
%

\begin{code}
#ifndef __HUGS__
data Int32  = I32# Int#
#else
newtype Int32 = I32 { unI32 :: Int } deriving (Eq, Ord)
#endif

#ifndef __HUGS__
instance CCallable Int32
instance CReturnable Int32

int32ToInt  (I32# x) = I# (i32ToInt# x)

i32ToInt# :: Int# -> Int#
#if WORD_SIZE_IN_BYTES > 4
i32ToInt# x = if x' <=# 0x7fffffff# then x' else x' -# 0x100000000#
   where x' = word2Int# (int2Word# x `and#` int2Word# 0xffffffff#)
#else
i32ToInt# x = x
#endif

intToInt32 (I# x) = I32# (intToInt32# x)

intToInt32# :: Int# -> Int#
#if WORD_SIZE_IN_BYTES > 4
intToInt32# i# = word2Int# ((int2Word# i#) `and#` int2Word# 0xffffffff#)
#else
intToInt32# i# = i#
#endif

instance Eq  Int32     where
  (I32# x#) == (I32# y#) = x# ==# y#
  (I32# x#) /= (I32# y#) = x# /=# y#

instance Ord Int32    where
  compare (I32# x#) (I32# y#) = compareInt# (i32ToInt# x#) (i32ToInt# y#)

instance Num Int32 where
  (I32# x#) + (I32# y#) = I32# (intToInt32# (x# +# y#))
  (I32# x#) - (I32# y#) = I32# (intToInt32# (x# -# y#))
  (I32# x#) * (I32# y#) = I32# (intToInt32# (x# *# y#))
#if WORD_SIZE_IN_BYTES > 4
  negate i@(I32# x)  = 
      if x ==# 0#
       then i
       else I32# (intToInt32# (0x100000000# -# x'))
#else
  negate (I32# x)  = I32# (negateInt# x)
#endif
  abs           = absReal
  signum        = signumReal
  fromInteger (S# i#)    = I32# (intToInt32# i#)
  fromInteger (J# s# d#) = I32# (intToInt32# (integer2Int# s# d#))
  fromInt       = intToInt32


#else // __HUGS__
int32ToInt = unI32
intToInt32 = I32

instance Num Int32 where
    (+) x y       = binop32 (+) x y
    (-) x y       = binop32 (-) x y
    negate        = I32 . negate . unI32
    (*) x y       = binop32 (*) x y
    abs           = absReal
    signum        = signumReal
    fromInteger   = intToInt32 . toInt
    fromInt       = intToInt32

binop32 op x y = I32 (unI32 x `op` unI32 y)
#endif

instance Bounded Int32 where 
    minBound = fromInt minBound
    maxBound = fromInt maxBound

instance Real Int32 where
    toRational x = toInteger x % 1

instance Integral Int32 where
    div x y
       | x > 0 && y < 0	= quotInt32 (x-y-1) y
       | x < 0 && y > 0	= quotInt32 (x-y+1) y
       | otherwise      = quotInt32 x y
#ifndef __HUGS__
    quot x@(I32# _) y@(I32# y#)
       | y# /=# 0#  = x `quotInt32` y
       | otherwise  = divZeroError "quot{Int32}" x
    rem x@(I32# _) y@(I32# y#)
       | y# /=# 0#  = x `remInt32` y
       | otherwise  = divZeroError "rem{Int32}" x
#else
    quot x y
       | y /= 0 = x `quotInt32` y
       | otherwise  = divZeroError "quot{Int32}" x
    rem x y
       | y /= 0  = x `remInt32` y
       | otherwise  = divZeroError "rem{Int32}" x
#endif
    mod x y
       | x > 0 && y < 0 || x < 0 && y > 0 = if r/=0 then r+y else 0
       | otherwise			  = r
	where r = remInt32 x y

#ifndef __HUGS__
    a@(I32# _) `quotRem` b@(I32# _) = (a `quotInt32` b, a `remInt32` b)
#else
    a `quotRem` b = (a `quot` b, a `rem` b)
#endif
    toInteger i32  = toInteger (int32ToInt i32)
    toInt     i32  = int32ToInt i32

remInt32, quotInt32 :: Int32 -> Int32 -> Int32
#ifndef __HUGS__
remInt32  (I32# x) (I32# y) = I32# (intToInt32# ((i32ToInt# x) `remInt#`  (i32ToInt# y)))
quotInt32 (I32# x) (I32# y) = I32# (intToInt32# ((i32ToInt# x) `quotInt#` (i32ToInt# y)))
#else
remInt32 = binop32 rem 
quotInt32 = binop32 quot
#endif

instance Ix Int32 where
    range (m,n)          = [m..n]
    index b@(m,_) i
	      | inRange b i = int32ToInt (i - m)
	      | otherwise   = indexError i b "Int32"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int32 where
    succ i
      | i == maxBound = succError "Int32"
      | otherwise     = i+1

    pred i
      | i == minBound = predError "Int32"
      | otherwise     = i-1

    toEnum x
        -- with Int having the same range as Int32, the following test
	-- shouldn't fail. However, having it here 
      | x >= toInt (minBound::Int32) && x <= toInt (maxBound::Int32) 
      = intToInt32 x
      | otherwise
      = toEnumError "Int32" x (minBound::Int32, maxBound::Int32)

    fromEnum           = int32ToInt

    enumFrom e1        = map toEnum [fromEnum e1 .. fromEnum (maxBound::Int32)]
    enumFromThen e1 e2 = map toEnum [fromEnum e1, fromEnum e2 .. fromEnum (last::Int32)]
			  where 
			    last
			     | e2 < e1   = minBound
			     | otherwise = maxBound


instance Read Int32 where
    readsPrec p s = [ (intToInt32 x,r) | (x,r) <- readsPrec p s ]

instance Show Int32 where
    showsPrec p i32 = showsPrec p (int32ToInt i32)

#ifndef __HUGS__

instance Bits Int32 where
  (I32# x) .&. (I32# y)   = I32# (word2Int# ((int2Word# x) `and#` (int2Word# y)))
  (I32# x) .|. (I32# y)   = I32# (word2Int# ((int2Word# x) `or#`  (int2Word# y)))
  (I32# x) `xor` (I32# y) = I32# (word2Int# ((int2Word# x) `xor#` (int2Word# y)))
#if WORD_SIZE_IN_BYTES > 4
  complement (I32# x)     = I32# (word2Int# ((int2Word# x) `xor#` (int2Word# 0xffffffff#)))
#else
  complement (I32# x)     = I32# (word2Int# ((int2Word# x) `xor#` (int2Word# (negateInt# 1#))))
#endif
  shift (I32# x) i@(I# i#)
	| i > 0     = I32# (intToInt32# (iShiftL# (i32ToInt# x)  i#))
	| otherwise = I32# (intToInt32# (iShiftRA# (i32ToInt# x) (negateInt# i#)))
  i32@(I32# x)  `rotate` (I# i)
        | i ==# 0#    = i32
	| i ># 0#     = 
             -- ( (x<<i') | ((x&(0x100000000-2^i2))>>i2)
	     I32# (intToInt32# ( word2Int# (
	            (int2Word# (iShiftL# (i32ToInt# x) i')) 
		          `or#`
                    (int2Word# (iShiftRA# (word2Int# (
		                              (int2Word# x) 
					          `and#` 
			                       (int2Word# (maxBound# -# pow2# i2 +# 1#))))
			                  i2)))))
	| otherwise = rotate i32 (I# (32# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 31#)
           i2 = 32# -# i'
           (I32# maxBound#) = maxBound
  bit i        	= shift 1 i
  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i   = (x .&. bit i) /= 0
  bitSize  _    = 32
  isSigned _    = True
#else
instance Bits Int32 where
  (.&.) x y    	= binop32 primAndInt x y
  (.|.) x y    	= binop32 primOrInt x y
  xor x y      	= binop32 primXorInt x y
  complement  	= xor ((-1) :: Int32) 
  x `shift` i   | i == 0 = x
		| i > 0  && i < 32 
			 = I32 (primShiftLInt (unI32 x) i)
		| i < 0  && i > -32
			 = I32 (primShiftRAInt (unI32 x) (-1))
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
sizeofInt32 :: Word32
sizeofInt32 = 4

\end{code}

\subsection[Int64]{The @Int64@ interface}


\begin{code}
#ifndef __HUGS__
#if WORD_SIZE_IN_BYTES == 8
--data Int64 = I64# Int#

int32ToInt64 (I32# i#) = I64# i#

intToInt32# :: Int# -> Int#
intToInt32# i# = word2Int# ((int2Word# i#) `and#` (case (maxBound::Word32) of W# x# -> x#))

int64ToInt32 (I64# i#) = I32# (intToInt32# w#)

instance Eq  Int64     where 
  (I64# x) == (I64# y) = x `eqInt#` y
  (I64# x) /= (I64# y) = x `neInt#` y

instance Ord Int32    where
  compare (I64# x#) (I64# y#) = compareInt# x# y#

instance Num Int64 where
  (I64# x) + (I64# y) = I64# (x +# y)
  (I64# x) - (I64# y) = I64# (x -# y)
  (I64# x) * (I64# y) = I64# (x *# y)
  negate w@(I64# x)   = I64# (negateInt# x)
  abs x               = absReal
  signum              = signumReal
  fromInteger (S# i#)    = I64# i#
  fromInteger (J# s# d#) = I64# (integer2Int# s# d#)
  fromInt       = intToInt64

instance Bounded Int64 where
  minBound = integerToInt64 (-0x8000000000000000)
  maxBound = integerToInt64 0x7fffffffffffffff

instance Integral Int64 where
    div x y
      | x > 0 && y < 0	= quotInt64 (x-y-1) y
      | x < 0 && y > 0	= quotInt64 (x-y+1) y
      | otherwise       = quotInt64 x y

    quot x@(I64# _) y@(I64# y#)
       | y# /=# 0# = x `quotInt64` y
       | otherwise = divZeroError "quot{Int64}" x

    rem x@(I64# _) y@(I64# y#)
       | y# /=# 0# = x `remInt64` y
       | otherwise = divZeroError "rem{Int64}" x

    mod x y
       | x > 0 && y < 0 || x < 0 && y > 0 = if r/=0 then r+y else 0
       | otherwise = r
	where r = remInt64 x y

    a@(I64# _) `quotRem` b@(I64# _) = (a `quotInt64` b, a `remInt64` b)
    toInteger (I64# i#) = toInteger (I# i#)
    toInt     (I64# i#) = I# i#

instance Bits Int64 where
  (I64# x) .&. (I64# y)   = I64# (word2Int# ((int2Word# x) `and#` (int2Word# y)))
  (I64# x) .|. (I64# y)   = I64# (word2Int# ((int2Word# x) `or#`  (int2Word# y)))
  (I64# x) `xor` (I64# y) = I64# (word2Int# ((int2Word# x) `xor#` (int2Word# y)))
  complement (I64# x)     = I64# (negateInt# x)
  shift (I64# x) i@(I# i#)
	| i > 0     = I64# (iShiftL# x  i#)
	| otherwise = I64# (iShiftRA# x (negateInt# i#))
  i64@(I64# x)  `rotate` (I# i)
        | i ==# 0#    = i64
	| i ># 0#     = 
             -- ( (x<<i') | ((x&(0x10000000000000000-2^i2))>>i2) )
	     I64# (word2Int# (
	            (int2Word# (iShiftL# x i')) 
		          `or#`
                    (int2Word# (iShiftRA# (word2Int# (
		                              (int2Word# x) 
					          `and#` 
			                       (int2Word# (maxBound# -# pow2# i2 +# 1#))))
			                  i2))))
	| otherwise = rotate i64 (I# (64# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 63#)
           i2 = 64# -# i'
           (I64# maxBound#) = maxBound
  bit i        	= shift 1 i
  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i   = (x .&. bit i) /= 0
  bitSize  _    = 64
  isSigned _    = True

remInt64  (I64# x) (I64# y) = I64# (x `remInt#` y)
quotInt64 (I64# x) (I64# y) = I64# (x `quotInt#` y)

int64ToInteger (I64# i#) = toInteger (I# i#)
integerToInt64 i = case fromInteger i of { I# i# -> I64# i# }

intToInt64 (I# i#) = I64# i#
int64ToInt (I64# i#) = I# i#

#else
--assume: support for long-longs
--data Int64 = I64 Int64# deriving (Eq, Ord, Bounded)

int32ToInt64 (I32# i#) = I64# (intToInt64# i#)
int64ToInt32 (I64# i#) = I32# (int64ToInt# i#)

int64ToInteger (I64# x#) = 
   case int64ToInteger# x# of
     (# s#, p# #) -> J# s# p#

integerToInt64 (S# i#) = I64# (intToInt64# i#)
integerToInt64 (J# s# d#) = I64# (integerToInt64# s# d#)

instance Eq  Int64     where 
  (I64# x) == (I64# y) = x `eqInt64#` y
  (I64# x) /= (I64# y) = x `neInt64#` y

instance Ord Int64     where 
  compare (I64# x) (I64# y)   = compareInt64# x y
  (<)  (I64# x) (I64# y)      = x `ltInt64#` y
  (<=) (I64# x) (I64# y)      = x `leInt64#` y
  (>=) (I64# x) (I64# y)      = x `geInt64#` y
  (>)  (I64# x) (I64# y)      = x `gtInt64#` y
  max x@(I64# x#) y@(I64# y#) = 
     case (compareInt64# x# y#) of { LT -> y ; EQ -> x ; GT -> x }
  min x@(I64# x#) y@(I64# y#) =
     case (compareInt64# x# y#) of { LT -> x ; EQ -> x ; GT -> y }

instance Num Int64 where
  (I64# x) + (I64# y) = I64# (x `plusInt64#`  y)
  (I64# x) - (I64# y) = I64# (x `minusInt64#` y)
  (I64# x) * (I64# y) = I64# (x `timesInt64#` y)
  negate (I64# x)     = I64# (negateInt64# x)
  abs x               = absReal x
  signum              = signumReal
  fromInteger i       = integerToInt64 i
  fromInt     i       = intToInt64 i

compareInt64# :: Int64# -> Int64# -> Ordering
compareInt64# i# j# 
 | i# `ltInt64#` j# = LT
 | i# `eqInt64#` j# = EQ
 | otherwise	    = GT

instance Bounded Int64 where
  minBound = integerToInt64 (-0x8000000000000000)
  maxBound = integerToInt64 0x7fffffffffffffff

instance Integral Int64 where
    div x y
      | x > 0 && y < 0	= quotInt64 (x-y-1) y
      | x < 0 && y > 0	= quotInt64 (x-y+1) y
      | otherwise       = quotInt64 x y

    quot x@(I64# _) y@(I64# y#)
       | y# `neInt64#` (intToInt64# 0#) = x `quotInt64` y
       | otherwise = divZeroError "quot{Int64}" x

    rem x@(I64# _) y@(I64# y#)
       | y# `neInt64#` (intToInt64# 0#) = x `remInt64` y
       | otherwise = divZeroError "rem{Int64}" x

    mod x y
       | x > 0 && y < 0 || x < 0 && y > 0 = if r/=0 then r+y else 0
       | otherwise = r
	where r = remInt64 x y

    a@(I64# _) `quotRem` b@(I64# _) = (a `quotInt64` b, a `remInt64` b)
    toInteger i         = int64ToInteger i
    toInt     i         = int64ToInt i

instance Bits Int64 where
  (I64# x) .&. (I64# y)   = I64# (word64ToInt64# ((int64ToWord64# x) `and64#` (int64ToWord64# y)))
  (I64# x) .|. (I64# y)   = I64# (word64ToInt64# ((int64ToWord64# x) `or64#`  (int64ToWord64# y)))
  (I64# x) `xor` (I64# y) = I64# (word64ToInt64# ((int64ToWord64# x) `xor64#` (int64ToWord64# y)))
  complement (I64# x)     = I64# (negateInt64# x)
  shift (I64# x) i@(I# i#)
	| i > 0     = I64# (iShiftL64# x  i#)
	| otherwise = I64# (iShiftRA64# x (negateInt# i#))
  i64@(I64# x)  `rotate` (I# i)
        | i ==# 0#    = i64
	| i ># 0#     = 
             -- ( (x<<i') | ((x&(0x10000000000000000-2^i2))>>i2) )
	     I64# (word64ToInt64# (
	            (int64ToWord64# (iShiftL64# x i'))		          `or64#`
                    (int64ToWord64# (iShiftRA64# (word64ToInt64# ((int64ToWord64# x)     `and64#` 
			                         (int64ToWord64# (maxBound# `minusInt64#` (pow2_64# i2 `plusInt64#` (intToInt64# 1#))))))
			                        i2))))
	| otherwise = rotate i64 (I# (64# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 63#)
           i2 = 64# -# i'
           (I64# maxBound#) = maxBound
  bit i        	= shift 1 i
  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i   = (x .&. bit i) /= 0
  bitSize  _    = 64
  isSigned _    = True

remInt64, quotInt64 :: Int64 -> Int64 -> Int64
remInt64  (I64# x) (I64# y) = I64# (x `remInt64#` y)
quotInt64 (I64# x) (I64# y) = I64# (x `quotInt64#` y)

intToInt64 (I# i#) = I64# (intToInt64# i#)
int64ToInt (I64# i#) = I# (int64ToInt# i#)

-- Word64# primop wrappers:

ltInt64# :: Int64# -> Int64# -> Bool
ltInt64# x# y# = stg_ltInt64 x# y# /= 0
      
leInt64# :: Int64# -> Int64# -> Bool
leInt64# x# y# = stg_leInt64 x# y# /= 0

eqInt64# :: Int64# -> Int64# -> Bool
eqInt64# x# y# = stg_eqInt64 x# y# /= 0

neInt64# :: Int64# -> Int64# -> Bool
neInt64# x# y# = stg_neInt64 x# y# /= 0

geInt64# :: Int64# -> Int64# -> Bool
geInt64# x# y# = stg_geInt64 x# y# /= 0

gtInt64# :: Int64# -> Int64# -> Bool
gtInt64# x# y# = stg_gtInt64 x# y# /= 0

plusInt64# :: Int64# -> Int64# -> Int64#
plusInt64# a# b# = case stg_plusInt64 a# b# of { I64# i# -> i# }

minusInt64# :: Int64# -> Int64# -> Int64#
minusInt64# a# b# = case stg_minusInt64 a# b# of { I64# i# -> i# }

timesInt64# :: Int64# -> Int64# -> Int64#
timesInt64# a# b# = case stg_timesInt64 a# b# of { I64# i# -> i# }

quotInt64# :: Int64# -> Int64# -> Int64#
quotInt64# a# b# = case stg_quotInt64 a# b# of { I64# i# -> i# }

remInt64# :: Int64# -> Int64# -> Int64#
remInt64# a# b# = case stg_remInt64 a# b# of { I64# i# -> i# }

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

iShiftL64# :: Int64# -> Int# -> Int64#
iShiftL64# a# b# = case stg_iShiftL64 a# b# of { I64# i# -> i# }

iShiftRL64# :: Int64# -> Int# -> Int64#
iShiftRL64# a# b# = case stg_iShiftRL64 a# b# of { I64# i# -> i# }

iShiftRA64# :: Int64# -> Int# -> Int64#
iShiftRA64# a# b# = case stg_iShiftRA64 a# b# of { I64# i# -> i# }

shiftRL64# :: Word64# -> Int# -> Word64#
shiftRL64# a# b# = case stg_shiftRL64 a# b# of { W64# w# -> w# }

int64ToInt# :: Int64# -> Int#
int64ToInt# i64# = case stg_int64ToInt i64# of { I# i# -> i# }

wordToWord64# :: Word# -> Word64#
wordToWord64# w# = case stg_wordToWord64 w# of { W64# w64# -> w64# }

word64ToInt64# :: Word64# -> Int64#
word64ToInt64# w# = case stg_word64ToInt64 w# of { I64# i# -> i# }

int64ToWord64# :: Int64# -> Word64#
int64ToWord64# i# = case stg_int64ToWord64 i# of { W64# w# -> w# }

intToInt64# :: Int# -> Int64#
intToInt64# i# = case stg_intToInt64 i# of { I64# i64# -> i64# }

foreign import "stg_intToInt64" unsafe stg_intToInt64 :: Int# -> Int64
foreign import "stg_int64ToWord64" unsafe stg_int64ToWord64 :: Int64# -> Word64
foreign import "stg_word64ToInt64" unsafe stg_word64ToInt64 :: Word64# -> Int64
foreign import "stg_wordToWord64" unsafe stg_wordToWord64 :: Word# -> Word64
foreign import "stg_int64ToInt" unsafe stg_int64ToInt :: Int64# -> Int
foreign import "stg_shiftRL64" unsafe stg_shiftRL64 :: Word64# -> Int# -> Word64
foreign import "stg_iShiftRA64" unsafe stg_iShiftRA64 :: Int64# -> Int# -> Int64
foreign import "stg_iShiftRL64" unsafe stg_iShiftRL64 :: Int64# -> Int# -> Int64
foreign import "stg_iShiftL64" unsafe stg_iShiftL64 :: Int64# -> Int# -> Int64
foreign import "stg_shiftL64" unsafe stg_shiftL64 :: Word64# -> Int# -> Word64
foreign import "stg_not64" unsafe stg_not64 :: Word64# -> Word64
foreign import "stg_xor64" unsafe stg_xor64 :: Word64# -> Word64# -> Word64
foreign import "stg_or64" unsafe stg_or64 :: Word64# -> Word64# -> Word64
foreign import "stg_and64" unsafe stg_and64 :: Word64# -> Word64# -> Word64
foreign import "stg_negateInt64" unsafe stg_negateInt64 :: Int64# -> Int64
foreign import "stg_remInt64" unsafe stg_remInt64 :: Int64# -> Int64# -> Int64
foreign import "stg_quotInt64" unsafe stg_quotInt64 :: Int64# -> Int64# -> Int64
foreign import "stg_timesInt64" unsafe stg_timesInt64 :: Int64# -> Int64# -> Int64
foreign import "stg_minusInt64" unsafe stg_minusInt64 :: Int64# -> Int64# -> Int64
foreign import "stg_plusInt64" unsafe stg_plusInt64 :: Int64# -> Int64# -> Int64
foreign import "stg_gtInt64" unsafe stg_gtInt64 :: Int64# -> Int64# -> Int
foreign import "stg_geInt64" unsafe stg_geInt64 :: Int64# -> Int64# -> Int
foreign import "stg_neInt64" unsafe stg_neInt64 :: Int64# -> Int64# -> Int
foreign import "stg_eqInt64" unsafe stg_eqInt64 :: Int64# -> Int64# -> Int
foreign import "stg_leInt64" unsafe stg_leInt64 :: Int64# -> Int64# -> Int
foreign import "stg_ltInt64" unsafe stg_ltInt64 :: Int64# -> Int64# -> Int

#endif
#else // __HUGS__
data Int64 = I64
	{ hi :: !Int32
	, lo :: !Word32
	} deriving (Eq, Ord)

to64 = integerToInt64
from64 = int64ToInteger
binop64 op x y = integerToInt64 (from64 x `op` from64 y)

instance Num Int64 where
    (+) x y       = binop64 (+) x y
    (-) x y       = binop64 (-) x y
    negate        = to64 . negate. from64
    (*) x y       = binop64 (*) x y
    abs           = absReal
    signum        = signumReal
    fromInteger   = integerToInt64
    fromInt       = fromInteger . toInteger

instance Integral Int64 where
  x `div` y     = fromInteger (toInteger x `div` toInteger y)
  x `quot` y    = fromInteger (toInteger x `quot` toInteger y)
  x `rem` y     = fromInteger (toInteger x `rem` toInteger y)
  x `mod` y     = fromInteger (toInteger x `mod` toInteger y)
  x `quotRem` y = (x `quot` y,x `rem` y)
  divMod        = quotRem
  even          = even . toInteger
  toInteger x   = int64ToInteger x

instance Bits Int64 where
  x .&. y       = I64 { lo = lo x .&. lo y
		      , hi = hi x .&. hi y }
  x .|. y       = I64 { lo = lo x .|. lo y
		      , hi = hi x .|. hi y }
  x `xor` y     = I64 { lo = lo x `xor` lo y
		      , hi = hi x `xor` hi y }
  complement    = xor (-1)
  x `shift` i   = to64 val
	where val    | i == 0 = from64 x
		     | i > 0  && i < 64  = from64 x * (2 ^ i)
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

instance Bounded Int64 where
  minBound = I64 { hi = minBound
		 , lo = minBound }
  maxBound = I64 { hi = maxBound
		 , lo = maxBound }

int32ToInt64 = integerToInt64 . int32ToInteger
int64ToInt32 = integerToInt32 . int64ToInteger

intToInt64 = integerToInt64 . toInteger
int64ToInt = toInt . int64ToInteger

integerToInt64 i = 
    case divMod i (toInteger (2 ^ 32)) of
	(a,b) -> I64 { hi = integerToInt32 a, lo = integerToWord32 b }
int64ToInteger (I64 { hi = hi, lo = lo }) 
	= toInteger  hi * (2 ^ 32) + toInteger lo
#endif

--
-- Code that's independent of Int64 rep.
-- 
instance Enum Int64 where
    succ i
      | i == maxBound = succError "Int64"
      | otherwise     = i+1

    pred i
      | i == minBound = predError "Int64"
      | otherwise     = i-1

    toEnum    i = intToInt64 i
    fromEnum  x
      | x >= intToInt64 (minBound::Int) && x <= intToInt64 (maxBound::Int)
      = int64ToInt x
      | otherwise
      = fromEnumError "Int64" x

    enumFrom e1        = map integerToInt64 [int64ToInteger e1 .. int64ToInteger (maxBound::Int64)]
    enumFromTo e1 e2   = map integerToInt64 [int64ToInteger e1 .. int64ToInteger e2]
    enumFromThen e1 e2 = map integerToInt64 [int64ToInteger e1, int64ToInteger e2 .. int64ToInteger last]
		       where 
			  last :: Int64
		          last 
			   | e2 < e1   = minBound
			   | otherwise = maxBound

    enumFromThenTo e1 e2 e3 = map integerToInt64 [int64ToInteger e1, int64ToInteger e2 .. int64ToInteger e3]

instance Show Int64 where
    showsPrec p i64 = showsPrec p (int64ToInteger i64)

instance Read Int64 where
  readsPrec _ s = [ (integerToInt64 x,r) | (x,r) <- readDec s ]

instance Ix Int64 where
    range (m,n)          = [m..n]
    index b@(m,_) i
	   | inRange b i = int64ToInt (i-m)
	   | otherwise   = indexError i b "Int64"
    inRange (m,n) i      = m <= i && i <= n

instance Real Int64 where
  toRational x = toInteger x % 1

sizeofInt64 :: Word32
sizeofInt64 = 8
\end{code}

%
%
\subsection[Int Utils]{Miscellaneous utilities}
%
%

Code copied from the Prelude

#ifndef __HUGS__

\begin{code}
absReal :: (Ord a, Num a) => a -> a
absReal x    | x >= 0    = x
	     | otherwise = -x

signumReal :: (Ord a, Num a) => a -> a
signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1
\end{code}

\begin{code}
indexInt8OffAddr  :: Addr -> Int -> Int8
indexInt8OffAddr (A# a#) (I# i#) = intToInt8 (I# (ord# (indexCharOffAddr# a# i#)))

indexInt16OffAddr :: Addr -> Int -> Int16
indexInt16OffAddr a i =
#ifdef WORDS_BIGENDIAN
  intToInt16 ( int8ToInt l + (int8ToInt maxBound) * int8ToInt h)
#else
  intToInt16 ( int8ToInt h + (int8ToInt maxBound) * int8ToInt l)
#endif
 where
   byte_idx = i * 2
   l = indexInt8OffAddr a byte_idx
   h = indexInt8OffAddr a (byte_idx+1)

indexInt32OffAddr :: Addr -> Int -> Int32
indexInt32OffAddr (A# a#) i = intToInt32 (I# (indexIntOffAddr# a# i'#))
 where
   -- adjust index to be in Int units, not Int32 ones.
  (I# i'#) 
#if WORD_SIZE_IN_BYTES==8
   = i `div` 2
#else
   = i
#endif

indexInt64OffAddr :: Addr -> Int -> Int64
indexInt64OffAddr (A# a#) (I# i#)
#if WORD_SIZE_IN_BYTES==8
 = I64# (indexIntOffAddr# a# i#)
#else
 = I64# (indexInt64OffAddr# a# i#)
#endif

#ifndef __PARALLEL_HASKELL__

indexInt8OffForeignObj  :: ForeignObj -> Int -> Int8
indexInt8OffForeignObj (ForeignObj fo#) (I# i#) = intToInt8 (I# (ord# (indexCharOffForeignObj# fo# i#)))

indexInt16OffForeignObj :: ForeignObj -> Int -> Int16
indexInt16OffForeignObj fo i =
# ifdef WORDS_BIGENDIAN
  intToInt16 ( int8ToInt l + (int8ToInt maxBound) * int8ToInt h)
# else
  intToInt16 ( int8ToInt h + (int8ToInt maxBound) * int8ToInt l)
# endif
 where
   byte_idx = i * 2
   l = indexInt8OffForeignObj fo byte_idx
   h = indexInt8OffForeignObj fo (byte_idx+1)

indexInt32OffForeignObj :: ForeignObj -> Int -> Int32
indexInt32OffForeignObj (ForeignObj fo#) i = intToInt32 (I# (indexIntOffForeignObj# fo# i'#))
 where
   -- adjust index to be in Int units, not Int32 ones.
  (I# i'#) 
# if WORD_SIZE_IN_BYTES==8
   = i `div` 2
# else
   = i
# endif

indexInt64OffForeignObj :: ForeignObj -> Int -> Int64
indexInt64OffForeignObj (ForeignObj fo#) (I# i#)
# if WORD_SIZE_IN_BYTES==8
 = I64# (indexIntOffForeignObj# fo# i#)
# else
 = I64# (indexInt64OffForeignObj# fo# i#)
# endif

#endif /* __PARALLEL_HASKELL__ */
\end{code}

Read words out of mutable memory:

\begin{code}
readInt8OffAddr :: Addr -> Int -> IO Int8
readInt8OffAddr a i = _casm_ `` %r=(StgInt8)(((StgInt8*)%0)[(StgInt)%1]); '' a i

readInt16OffAddr  :: Addr -> Int -> IO Int16
readInt16OffAddr a i = _casm_ `` %r=(StgInt16)(((StgInt16*)%0)[(StgInt)%1]); '' a i

readInt32OffAddr  :: Addr -> Int -> IO Int32
readInt32OffAddr a i = _casm_ `` %r=(StgInt32)(((StgInt32*)%0)[(StgInt)%1]); '' a i

readInt64OffAddr  :: Addr -> Int -> IO Int64
#if WORD_SIZE_IN_BYTES==8
readInt64OffAddr a i = _casm_ `` %r=(StgInt)(((StgInt*)%0)[(StgInt)%1]); '' a i
#else
readInt64OffAddr a i = _casm_ `` %r=(StgInt64)(((StgInt64*)%0)[(StgInt)%1]); '' a i
#endif

#ifndef __PARALLEL_HASKELL__

readInt8OffForeignObj :: ForeignObj -> Int -> IO Int8
readInt8OffForeignObj fo i = _casm_ `` %r=(StgInt8)(((StgInt8*)%0)[(StgInt)%1]); '' fo i

readInt16OffForeignObj  :: ForeignObj -> Int -> IO Int16
readInt16OffForeignObj fo i = _casm_ `` %r=(StgInt16)(((StgInt16*)%0)[(StgInt)%1]); '' fo i

readInt32OffForeignObj  :: ForeignObj -> Int -> IO Int32
readInt32OffForeignObj fo i = _casm_ `` %r=(StgInt32)(((StgInt32*)%0)[(StgInt)%1]); '' fo i

readInt64OffForeignObj  :: ForeignObj -> Int -> IO Int64
# if WORD_SIZE_IN_BYTES==8
readInt64OffForeignObj fo i = _casm_ `` %r=(StgInt)(((StgInt*)%0)[(StgInt)%1]); '' fo i
# else
readInt64OffForeignObj fo i = _casm_ `` %r=(StgInt64)(((StgInt64*)%0)[(StgInt)%1]); '' fo i
# endif

#endif /* __PARALLEL_HASKELL__ */
\end{code}

\begin{code}
writeInt8OffAddr  :: Addr -> Int -> Int8  -> IO ()
writeInt8OffAddr a i e = _casm_ `` (((StgInt8*)%0)[(StgInt)%1])=(StgInt8)%2; '' a i e

writeInt16OffAddr :: Addr -> Int -> Int16 -> IO ()
writeInt16OffAddr a i e = _casm_ `` (((StgInt16*)%0)[(StgInt)%1])=(StgInt16)%2; '' a i e

writeInt32OffAddr :: Addr -> Int -> Int32 -> IO ()
writeInt32OffAddr a i e = _casm_ `` (((StgInt32*)%0)[(StgInt)%1])=(StgInt32)%2; '' a i e

writeInt64OffAddr :: Addr -> Int -> Int64 -> IO ()
#if WORD_SIZE_IN_BYTES==8
writeInt64OffAddr a i e = _casm_ `` (((StgInt*)%0)[(StgInt)%1])=(StgInt)%2; '' a i e
#else
writeInt64OffAddr a i e = _casm_ `` (((StgInt64*)%0)[(StgInt)%1])=(StgInt64)%2; '' a i e
#endif

#ifndef __PARALLEL_HASKELL__

writeInt8OffForeignObj  :: ForeignObj -> Int -> Int8  -> IO ()
writeInt8OffForeignObj fo i e = _casm_ `` (((StgInt8*)%0)[(StgInt)%1])=(StgInt8)%2; '' fo i e

writeInt16OffForeignObj :: ForeignObj -> Int -> Int16 -> IO ()
writeInt16OffForeignObj fo i e = _casm_ `` (((StgInt16*)%0)[(StgInt)%1])=(StgInt16)%2; '' fo i e

writeInt32OffForeignObj :: ForeignObj -> Int -> Int32 -> IO ()
writeInt32OffForeignObj fo i e = _casm_ `` (((StgInt32*)%0)[(StgInt)%1])=(StgInt32)%2; '' fo i e

writeInt64OffForeignObj :: ForeignObj -> Int -> Int64 -> IO ()
# if WORD_SIZE_IN_BYTES==8
writeInt64OffForeignObj fo i e = _casm_ `` (((StgInt*)%0)[(StgInt)%1])=(StgInt)%2; '' fo i e
# else
writeInt64OffForeignObj fo i e = _casm_ `` (((StgInt64*)%0)[(StgInt)%1])=(StgInt64)%2; '' fo i e
# endif

#endif /* __PARALLEL_HASKELL__ */

\end{code}

#endif 

C&P'ed from Ix.lhs

\begin{code}
{-# NOINLINE indexError #-}
indexError :: Show a => a -> (a,a) -> String -> b
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
