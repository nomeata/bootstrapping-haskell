% -----------------------------------------------------------------------------
% $Id: Storable.lhs,v 1.6 2000/05/10 14:57:16 rrt Exp $
%
% (c) The FFI task force, 2000
%

A class and associated functions for primitive marshaling

\begin{code}
#include "MachDeps.h"

module Storable
	( Storable(
	     sizeOf,            -- :: a -> Int
	     alignment,         -- :: a -> Int
	     peekElemOff,       -- :: Addr -> Int          -> IO a
	     pokeElemOff,       -- :: Addr -> Int     -> a -> IO ()
	     peekByteOff,       -- :: Addr -> AddrOff      -> IO a
	     pokeByteOff,       -- :: Addr -> AddrOff -> a -> IO ()
	     peek,              -- :: Addr                 -> IO a
	     poke)              -- :: Addr            -> a -> IO ()

	, malloc,               -- ::               Int      -> IO Addr
	, mallocElem,           -- :: Storable a => a        -> IO Addr
	, mallocElems,          -- :: Storable a => a -> Int -> IO Addr
	, realloc,              -- ::            Addr -> Int -> IO Addr
	, free                  -- :: Addr -> IO ()

	, alloca		-- ::               Int      -> (Addr -> IO a) -> IO a
	, allocaElem		-- :: Storable a => a        -> (Addr -> IO b) -> IO b
	, allocaElems		-- :: Storable a => a -> Int -> (Addr -> IO b) -> IO b
        ) where
\end{code}

\begin{code}
import Addr		( Addr, AddrOff, nullAddr, plusAddr
			, readCharOffAddr,        writeCharOffAddr
			, readIntOffAddr,         writeIntOffAddr
			, readAddrOffAddr,        writeAddrOffAddr
			, readFloatOffAddr,       writeFloatOffAddr
			, readDoubleOffAddr,      writeDoubleOffAddr
			, readWord8OffAddr,       writeWord8OffAddr
			, readWord16OffAddr,      writeWord16OffAddr
			, readWord32OffAddr,      writeWord32OffAddr
			, readWord64OffAddr,      writeWord64OffAddr
			, readInt8OffAddr,        writeInt8OffAddr
			, readInt16OffAddr,       writeInt16OffAddr
			, readInt32OffAddr,       writeInt32OffAddr
			, readInt64OffAddr,       writeInt64OffAddr
			, readStablePtrOffAddr,   writeStablePtrOffAddr
			)
import Int		( Int8,  Int16,  Int32,  Int64  )
import Word		( Word8, Word16, Word32, Word64 )
import StablePtr	( StablePtr )
import IOExts		( fixIO )
import PrelAddr		( Word, intToWord )
import Exception	( bracket )
\end{code}

Primitive marshaling

Minimal complete definition: sizeOf, alignment, and one definition
in each of the peek/poke families.

\begin{code}
class Storable a where

   -- sizeOf/alignment *never* use their first argument
   sizeOf      :: a -> Int
   alignment   :: a -> Int

   -- replacement for read-/write???OffAddr
   peekElemOff :: Addr -> Int          -> IO a
   pokeElemOff :: Addr -> Int     -> a -> IO ()

   -- the same with *byte* offsets
   peekByteOff :: Addr -> AddrOff      -> IO a
   pokeByteOff :: Addr -> AddrOff -> a -> IO ()

   -- ... and with no offsets at all
   peek        :: Addr                 -> IO a
   poke        :: Addr            -> a -> IO ()

   -- circular default instances
   peekElemOff addr off = fixIO (\val -> peekByteOff addr (fromIntegral (off * sizeOf val)))
   pokeElemOff addr off val = pokeByteOff addr (fromIntegral (off * sizeOf val)) val

   peekByteOff addr off = peek (addr `plusAddr` off)
   pokeByteOff addr off = poke (addr `plusAddr` off)

   peek addr = peekElemOff addr 0
   poke addr = pokeElemOff addr 0
\end{code}

System-dependent, but rather obvious instances

\begin{code}
instance Storable Char where
   sizeOf      = const SIZEOF_CHAR
   alignment   = const ALIGNMENT_CHAR
   peekElemOff = readCharOffAddr
   pokeElemOff = writeCharOffAddr

instance Storable Int where
   sizeOf      = const SIZEOF_INT
   alignment   = const ALIGNMENT_INT
   peekElemOff = readIntOffAddr
   pokeElemOff = writeIntOffAddr

instance Storable Addr where
   sizeOf      = const SIZEOF_VOID_P
   alignment   = const ALIGNMENT_VOID_P
   peekElemOff = readAddrOffAddr
   pokeElemOff = writeAddrOffAddr

instance Storable (StablePtr a) where
   sizeOf      = const SIZEOF_VOID_P
   alignment   = const ALIGNMENT_VOID_P
   peekElemOff = readStablePtrOffAddr
   pokeElemOff = writeStablePtrOffAddr

instance Storable Float where
   sizeOf      = const SIZEOF_FLOAT
   alignment   = const ALIGNMENT_FLOAT
   peekElemOff = readFloatOffAddr
   pokeElemOff = writeFloatOffAddr

instance Storable Double where
   sizeOf      = const SIZEOF_DOUBLE
   alignment   = const ALIGNMENT_DOUBLE
   peekElemOff = readDoubleOffAddr
   pokeElemOff = writeDoubleOffAddr

instance Storable Word8 where
   sizeOf      = const SIZEOF_WORD8
   alignment   = const ALIGNMENT_WORD8
   peekElemOff = readWord8OffAddr
   pokeElemOff = writeWord8OffAddr

instance Storable Word16 where
   sizeOf      = const SIZEOF_WORD16
   alignment   = const ALIGNMENT_WORD16
   peekElemOff = readWord16OffAddr
   pokeElemOff = writeWord16OffAddr

instance Storable Word32 where
   sizeOf      = const SIZEOF_WORD32
   alignment   = const ALIGNMENT_WORD32
   peekElemOff = readWord32OffAddr
   pokeElemOff = writeWord32OffAddr

instance Storable Word64 where
   sizeOf      = const SIZEOF_WORD64
   alignment   = const ALIGNMENT_WORD64
   peekElemOff = readWord64OffAddr
   pokeElemOff = writeWord64OffAddr

instance Storable Int8 where
   sizeOf      = const SIZEOF_INT8
   alignment   = const ALIGNMENT_INT8
   peekElemOff = readInt8OffAddr
   pokeElemOff = writeInt8OffAddr

instance Storable Int16 where
   sizeOf       = const SIZEOF_INT16
   alignment    = const ALIGNMENT_INT16
   peekElemOff  = readInt16OffAddr
   pokeElemOff  = writeInt16OffAddr

instance Storable Int32 where
   sizeOf      = const SIZEOF_INT32
   alignment   = const ALIGNMENT_INT32
   peekElemOff = readInt32OffAddr
   pokeElemOff = writeInt32OffAddr

instance Storable Int64 where
   sizeOf      = const SIZEOF_INT64
   alignment   = const ALIGNMENT_INT64
   peekElemOff = readInt64OffAddr
   pokeElemOff = writeInt64OffAddr
\end{code}

(de-)allocation of raw bytes

\begin{code}
malloc :: Int -> IO Addr
malloc numBytes = failWhenNULL "malloc" (malloc_ (intToWord numBytes))

mallocElem :: Storable a => a -> IO Addr
mallocElem unused = malloc (sizeOf unused)

mallocElems :: Storable a => a -> Int -> IO Addr
mallocElems unused numElems = malloc (numElems * sizeOf unused)

realloc :: Addr -> Int -> IO Addr
realloc oldAddr numBytes =
   failWhenNULL "realloc" (realloc_ oldAddr (intToWord numBytes))

foreign import "free" unsafe free :: Addr -> IO ()
\end{code}

temporary allocation of raw bytes

\begin{code}
alloca :: Int -> (Addr -> IO a) -> IO a
alloca numBytes act = bracket (malloc numBytes) free act

allocaElem :: Storable a => a -> (Addr -> IO b) -> IO b
allocaElem unused act = alloca (sizeOf unused) act

allocaElems :: Storable a => a -> Int -> (Addr -> IO b) -> IO b
allocaElems unused numElems act = alloca (numElems * sizeOf unused) act
\end{code}

utility functions, not exported

\begin{code}
failWhenNULL :: String -> IO Addr -> IO Addr
failWhenNULL fun act = do
   addr <- act
   if addr == nullAddr
      then ioError (userError (fun ++ ": out of memory"))
      else return addr

-- TODO: Word should always match size_t
foreign import "malloc"  unsafe malloc_  ::         Word -> IO Addr
foreign import "realloc" unsafe realloc_ :: Addr -> Word -> IO Addr
\end{code}
