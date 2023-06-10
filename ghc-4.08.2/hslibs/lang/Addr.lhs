%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Addr]{Module @Addr@}

\begin{code}
#include "MachDeps.h"

module Addr 
	( Addr				-- abstract, instance of Eq, Ord, Show, Typeable
	, AddrOff	           	-- abstract, instance of Eq, Ord, Show, Enum, Num, Real, Integral, Typeable
	, nullAddr			-- :: Addr
	, alignAddr			-- :: Addr -> Int     -> Addr
	, plusAddr			-- :: Addr -> AddrOff -> Addr
	, minusAddr			-- :: Addr -> Addr    -> AddrOff

	-- SUP: deprecated in the new FFI, index/read/write???OffAddr are
	-- subsumed by the Storable class
	-- NOTE: The functions for ForeignObj, StablePtr, and Word have
	-- officially never been part of this module.
	, indexCharOffAddr		-- :: Addr -> Int -> Char
	, indexIntOffAddr		-- :: Addr -> Int -> Int
	, indexAddrOffAddr		-- :: Addr -> Int -> Addr
	, indexFloatOffAddr		-- :: Addr -> Int -> Float
	, indexDoubleOffAddr		-- :: Addr -> Int -> Double
	, indexWord8OffAddr		-- :: Addr -> Int -> Word8
	, indexWord16OffAddr		-- :: Addr -> Int -> Word16
	, indexWord32OffAddr		-- :: Addr -> Int -> Word32
	, indexWord64OffAddr		-- :: Addr -> Int -> Word64
	, indexInt8OffAddr		-- :: Addr -> Int -> Int8
	, indexInt16OffAddr		-- :: Addr -> Int -> Int16
	, indexInt32OffAddr		-- :: Addr -> Int -> Int32
	, indexInt64OffAddr		-- :: Addr -> Int -> Int64
	, indexStablePtrOffAddr		-- :: Addr -> Int -> StablePtr a
	, indexWordOffAddr		-- :: Addr -> Int -> Word


	, readCharOffAddr		-- :: Addr -> Int -> IO Char
	, readIntOffAddr		-- :: Addr -> Int -> IO Int
	, readAddrOffAddr		-- :: Addr -> Int -> IO Addr
	, readFloatOffAddr		-- :: Addr -> Int -> IO Float
	, readDoubleOffAddr		-- :: Addr -> Int -> IO Double
	, readWord8OffAddr		-- :: Addr -> Int -> IO Word8
	, readWord16OffAddr		-- :: Addr -> Int -> IO Word16
	, readWord32OffAddr		-- :: Addr -> Int -> IO Word32
	, readWord64OffAddr		-- :: Addr -> Int -> IO Word64
	, readInt8OffAddr		-- :: Addr -> Int -> IO Int8
	, readInt16OffAddr		-- :: Addr -> Int -> IO Int16
	, readInt32OffAddr		-- :: Addr -> Int -> IO Int32
	, readInt64OffAddr		-- :: Addr -> Int -> IO Int64
	, readStablePtrOffAddr		-- :: Addr -> Int -> IO (StablePtr a)
	, readWordOffAddr		-- :: Addr -> Int -> IO Word

	, writeCharOffAddr		-- :: Addr -> Int -> Char   -> IO ()
	, writeIntOffAddr		-- :: Addr -> Int -> Int    -> IO ()
, writeAddrOffAddr		-- :: Addr -> Int -> Addr   -> IO ()
	, writeFloatOffAddr		-- :: Addr -> Int -> Float  -> IO ()
	, writeDoubleOffAddr		-- :: Addr -> Int -> Double -> IO ()
	, writeWord8OffAddr		-- :: Addr -> Int -> Word8  -> IO ()
	, writeWord16OffAddr		-- :: Addr -> Int -> Word16 -> IO ()
	, writeWord32OffAddr		-- :: Addr -> Int -> Word32 -> IO ()
	, writeWord64OffAddr		-- :: Addr -> Int -> Word64 -> IO ()
	, writeInt8OffAddr		-- :: Addr -> Int -> Int8   -> IO ()
	, writeInt16OffAddr		-- :: Addr -> Int -> Int16  -> IO ()
	, writeInt32OffAddr		-- :: Addr -> Int -> Int32  -> IO ()
	, writeInt64OffAddr		-- :: Addr -> Int -> Int64  -> IO ()
	, writeStablePtrOffAddr		-- :: Addr -> Int -> StablePtr a -> IO ()
#ifndef __PARALLEL_HASKELL__
	, writeForeignObjOffAddr	-- :: Addr -> Int -> ForeignObj -> IO ()
#endif
	, writeWordOffAddr		-- :: Addr -> Int -> Word  -> IO ()

	-- deprecated (non-standard) coercions
	, addrToInt			-- :: Addr -> Int  
	, intToAddr			-- :: Int  -> Addr

	-- deprecated
	, Word(..)
	, wordToInt			-- :: Word -> Int
	, intToWord			-- :: Int  -> Word
	) where

import PrelFloat	( Float(..), Double(..) )
import NumExts		( showHex )
import PrelAddr		( Addr(..), AddrOff(..)
			, nullAddr, alignAddr , plusAddr, minusAddr
			, indexAddrOffAddr, Word(..),
			, intToWord, wordToInt
			)
import PrelStable	( StablePtr(..) )
import PrelBase		( Int(..), Char(..) )
import PrelIOBase	( IO(..), ForeignObj(..) )
import PrelGHC		( indexCharOffAddr#,       indexIntOffAddr#
			, indexWordOffAddr#,       indexFloatOffAddr#
			, indexDoubleOffAddr#,     indexStablePtrOffAddr#
			, readCharOffAddr#,        readIntOffAddr#
			, readWordOffAddr#,        readAddrOffAddr#
			, readFloatOffAddr#,       readDoubleOffAddr#
			, readStablePtrOffAddr#
			, writeCharOffAddr#,       writeIntOffAddr#
			, writeWordOffAddr#,       writeAddrOffAddr#
			, writeFloatOffAddr#,      writeDoubleOffAddr#
			, writeForeignObjOffAddr#, writeStablePtrOffAddr#
			, addr2Int#,               int2Addr#
			)
import Word		( indexWord8OffAddr,       indexWord16OffAddr
			, indexWord32OffAddr,      indexWord64OffAddr
			, readWord8OffAddr,        readWord16OffAddr
			, readWord32OffAddr,       readWord64OffAddr
			, writeWord8OffAddr,       writeWord16OffAddr
			, writeWord32OffAddr,      writeWord64OffAddr
			)
import Int		( indexInt8OffAddr,        indexInt16OffAddr
			, indexInt32OffAddr,       indexInt64OffAddr
			, readInt8OffAddr,         readInt16OffAddr
			, readInt32OffAddr,        readInt64OffAddr
			, writeInt8OffAddr,        writeInt16OffAddr
			, writeInt32OffAddr,       writeInt64OffAddr
			, toInt
			)
import PrelGHC		( int2Word#,		   word2Integer# )
import PrelNum		( Integer(J#) )
\end{code}

\begin{code}
instance Show Addr where
   showsPrec p (A# addr) rs = pad_out (showHex (word2Integer(int2Word#(addr2Int# addr))) "") rs
     where
        -- want 0s prefixed to pad it out to a fixed length.
       pad_out ('0':'x':ls) rs = 
	  '0':'x':(replicate (2*ADDR_SIZE_IN_BYTES - length ls) '0') ++ ls ++ rs
       -- word2Integer :: Word# -> Integer (stolen from Word.lhs)
       word2Integer w = case word2Integer# w of
			(# s, d #) -> J# s d
\end{code}

We have the following (boring) instances for AddrOff here,
because in PrelAddr most of the classes are not known.

\begin{code}
instance Eq AddrOff where
   AddrOff# x == AddrOff# y = x == y

instance Ord AddrOff where
   AddrOff# x <= AddrOff# y = x <= y

instance Show AddrOff where
   showsPrec d (AddrOff# x) =
      showParen (d >= 10) (showString "AddrOff " . showsPrec 10 x)

instance Enum AddrOff where
   toEnum x              = AddrOff# x
   fromEnum (AddrOff# x) = x

instance Num AddrOff where
   (AddrOff# x) + (AddrOff# y) = AddrOff# (x + y)
   (AddrOff# x) - (AddrOff# y) = AddrOff# (x - y)
   (AddrOff# x) * (AddrOff# y) = AddrOff# (x * y)
   abs    (AddrOff# x)         = AddrOff# (abs    x)
   signum (AddrOff# x)         = AddrOff# (signum x)
   fromInteger x               = AddrOff# (fromInteger x)

instance Real AddrOff where
   toRational (AddrOff# x) = toRational x

instance Integral AddrOff where
   AddrOff# x `quotRem` AddrOff# y =
      case x `quotRem` y of { (q,r) -> (AddrOff# q, AddrOff# r) }
   toInteger (AddrOff# x) = toInteger x
   toInt (AddrOff# x)     = x

{-# RULES
   "fromIntegral/Int->AddrOff"   fromIntegral = AddrOff# ;
   "fromIntegral/AddrOff->Int"   forall x . fromIntegral (AddrOff# x) = x
 #-}
\end{code}

Coercing between machine ints and words (deprecated)

\begin{code}
addrToInt :: Addr -> Int
intToAddr :: Int -> Addr

#ifdef __HUGS__
addrToInt = primAddrToInt
intToAddr = primIntToAddr
#else
addrToInt (A# a#) = I# (addr2Int# a#)
intToAddr (I# i#) = A# (int2Addr# i#)
#endif
\end{code}

Indexing immutable memory:
SUP: deprecated in the new FFI, subsumed by the Storable class

\begin{code}
indexCharOffAddr   :: Addr -> Int -> Char
indexIntOffAddr    :: Addr -> Int -> Int
indexWordOffAddr   :: Addr -> Int -> Word
--in PrelAddr: indexAddrOffAddr   :: Addr -> Int -> Addr
indexFloatOffAddr  :: Addr -> Int -> Float
indexDoubleOffAddr :: Addr -> Int -> Double
indexStablePtrOffAddr :: Addr -> Int -> StablePtr a

#ifdef __HUGS__
indexCharOffAddr   = error "TODO: indexCharOffAddr  "
indexIntOffAddr    = error "TODO: indexIntOffAddr   "
indexWordOffAddr   = error "TODO: indexWordOffAddr  "
indexAddrOffAddr   = error "TODO: indexAddrOffAddr  "
indexFloatOffAddr  = error "TODO: indexFloatOffAddr "
indexDoubleOffAddr = error "TODO: indexDoubleOffAddr"
indexStablePtrOffAddr = error "TODO: indexStablePtrOffAddr"
#else
indexCharOffAddr   (A# addr#) (I# n#) = C# (indexCharOffAddr# addr# n#)
indexIntOffAddr    (A# addr#) (I# n#) = I# (indexIntOffAddr# addr# n#)
indexWordOffAddr   (A# addr#) (I# n#) = W# (indexWordOffAddr# addr# n#)
indexFloatOffAddr  (A# addr#) (I# n#) = F# (indexFloatOffAddr# addr# n#)
indexDoubleOffAddr (A# addr#) (I# n#) = D# (indexDoubleOffAddr# addr# n#)
indexStablePtrOffAddr (A# addr#) (I# n#) 
  = StablePtr (indexStablePtrOffAddr# addr# n#)
#endif
\end{code}

Indexing mutable memory:
SUP: deprecated in the new FFI, subsumed by the Storable class

\begin{code}
readCharOffAddr       :: Addr -> Int -> IO Char
readIntOffAddr        :: Addr -> Int -> IO Int
readWordOffAddr       :: Addr -> Int -> IO Word
readAddrOffAddr       :: Addr -> Int -> IO Addr
readFloatOffAddr      :: Addr -> Int -> IO Float
readDoubleOffAddr     :: Addr -> Int -> IO Double
readStablePtrOffAddr  :: Addr -> Int -> IO (StablePtr a)

#ifdef __HUGS__
readCharOffAddr       = error "TODO: readCharOffAddr"
readIntOffAddr        = error "TODO: readIntOffAddr"
readWordOffAddr       = error "TODO: readWordOffAddr"
readAddrOffAddr       = error "TODO: readAddrOffAddr"
readFloatOffAddr      = error "TODO: readFloatOffAddr"
readDoubleOffAddr     = error "TODO: readDoubleOffAddr"
readStablePtrOffAddr  = error "TODO: readStablePtrOffAddr"
#else
readCharOffAddr (A# a) (I# i)
  = IO $ \s -> case readCharOffAddr# a i s       of { (# s,x #) -> (# s, C# x #) }
readIntOffAddr (A# a) (I# i)
  = IO $ \s -> case readIntOffAddr# a i s        of { (# s,x #) -> (# s, I# x #) }
readWordOffAddr (A# a) (I# i)
  = IO $ \s -> case readWordOffAddr# a i s       of { (# s,x #) -> (# s, W# x #) }
readAddrOffAddr (A# a) (I# i)
  = IO $ \s -> case readAddrOffAddr# a i s       of { (# s,x #) -> (# s, A# x #) }
readFloatOffAddr (A# a) (I# i)
  = IO $ \s -> case readFloatOffAddr# a i s      of { (# s,x #) -> (# s, F# x #) }
readDoubleOffAddr (A# a) (I# i)
  = IO $ \s -> case readDoubleOffAddr# a i s     of { (# s,x #) -> (# s, D# x #) }
readStablePtrOffAddr (A# a) (I# i)
  = IO $ \s -> case readStablePtrOffAddr# a i s  of { (# s,x #) -> (# s, StablePtr x #) }
#endif
\end{code}

SUP: deprecated in the new FFI, subsumed by the Storable class

\begin{code}
writeCharOffAddr      :: Addr -> Int -> Char        -> IO ()
writeIntOffAddr       :: Addr -> Int -> Int         -> IO ()
writeWordOffAddr      :: Addr -> Int -> Word        -> IO ()
writeAddrOffAddr      :: Addr -> Int -> Addr        -> IO ()
writeFloatOffAddr     :: Addr -> Int -> Float       -> IO ()
writeDoubleOffAddr    :: Addr -> Int -> Double      -> IO ()
writeStablePtrOffAddr :: Addr -> Int -> StablePtr a -> IO ()

#ifdef __HUGS__
writeCharOffAddr      = error "TODO: writeCharOffAddr"
writeIntOffAddr       = error "TODO: writeIntOffAddr"
writeWordOffAddr      = error "TODO: writeWordOffAddr"
writeAddrOffAddr      = error "TODO: writeAddrOffAddr"
writeFloatOffAddr     = error "TODO: writeFloatOffAddr"
writeDoubleOffAddr    = error "TODO: writeDoubleOffAddr"
writeStablePtrOffAddr = error "TODO: writeStablePtrOffAddr"
#else
writeCharOffAddr (A# a#) (I# i#) (C# c#) = IO $ \ s# ->
      case (writeCharOffAddr#  a# i# c# s#) of s2# -> (# s2#, () #)

writeIntOffAddr (A# a#) (I# i#) (I# e#) = IO $ \ s# ->
      case (writeIntOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeWordOffAddr (A# a#) (I# i#) (W# e#) = IO $ \ s# ->
      case (writeWordOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeAddrOffAddr (A# a#) (I# i#) (A# e#) = IO $ \ s# ->
      case (writeAddrOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeFloatOffAddr (A# a#) (I# i#) (F# e#) = IO $ \ s# ->
      case (writeFloatOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeDoubleOffAddr (A# a#) (I# i#) (D# e#) = IO $ \ s# ->
      case (writeDoubleOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)
#ifndef __PARALLEL_HASKELL__
writeForeignObjOffAddr   :: Addr -> Int -> ForeignObj -> IO ()
writeForeignObjOffAddr (A# a#) (I# i#) (ForeignObj e#) = IO $ \ s# ->
      case (writeForeignObjOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)
#endif
writeStablePtrOffAddr (A# a#) (I# i#) (StablePtr e#) = IO $ \ s# ->
      case (writeStablePtrOffAddr#  a# i# e# s#) of s2# -> (# s2# , () #)

#endif
\end{code}
