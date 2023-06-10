% -----------------------------------------------------------------------------
% $Id: ForeignObj.lhs,v 1.5.2.3 2000/10/30 11:22:09 simonmar Exp $
%
% (c) The GRAP/AQUA Project, Glasgow University & The FFI task force, 2000
%

This module defines foreign objects, i.e. addresses with associated
finalizers. It currently contains large wads of code for legacy reasons,
which can be nuked in future releases.

At the moment we cannot use ForeignObjs in GUM/GranSim, sorry.
Probably SMP could use ForeignObjs, but that needs check!      -- HWL

\begin{code}
module ForeignObj
#ifndef __PARALLEL_HASKELL__
        ( ForeignObj,         -- abstract, instance of: Eq
        , newForeignObj       -- :: Addr -> IO () -> IO ForeignObj
        , addForeignFinalizer -- :: ForeignObj -> IO () -> IO ()
        -- the coercion from a foreign object to an address is unsafe,
        -- and should be used with great care!
        , foreignObjToAddr    -- :: ForeignObj -> Addr

        -- SUP: deprecated, the address associated with a foreign object is immutable
        , writeForeignObj     -- :: ForeignObj  -> Addr{-new obj-}   -> IO ()

        -- SUP: deprecated
        , makeForeignObj      -- :: Addr -> Addr -> IO ForeignObj

        -- SUP: deprecated, use class Storable
        , indexCharOffForeignObj    -- :: ForeignObj -> Int -> Char
        , indexAddrOffForeignObj    -- :: ForeignObj -> Int -> Addr
        , indexFloatOffForeignObj   -- :: ForeignObj -> Int -> Float
        , indexDoubleOffForeignObj  -- :: ForeignObj -> Int -> Double
       
        , indexIntOffForeignObj     -- :: ForeignObj -> Int -> Int
        , indexInt8OffForeignObj    -- :: ForeignObj -> Int -> Int8
        , indexInt16OffForeignObj   -- :: ForeignObj -> Int -> Int16
        , indexInt32OffForeignObj   -- :: ForeignObj -> Int -> Int32
        , indexInt64OffForeignObj   -- :: ForeignObj -> Int -> Int64

        , indexWordOffForeignObj    -- :: ForeignObj -> Int -> Word
        , indexWord8OffForeignObj   -- :: ForeignObj -> Int -> Word8
        , indexWord16OffForeignObj  -- :: ForeignObj -> Int -> Word16
        , indexWord32OffForeignObj  -- :: ForeignObj -> Int -> Word32
        , indexWord64OffForeignObj  -- :: ForeignObj -> Int -> Word64

        , readCharOffForeignObj     -- :: ForeignObj -> Int -> IO Char
        , readAddrOffForeignObj     -- :: ForeignObj -> Int -> IO Addr
        , readFloatOffForeignObj    -- :: ForeignObj -> Int -> IO Float
        , readDoubleOffForeignObj   -- :: ForeignObj -> Int -> IO Double
       
        , readIntOffForeignObj      -- :: ForeignObj -> Int -> IO Int
        , readInt8OffForeignObj     -- :: ForeignObj -> Int -> IO Int8
        , readInt16OffForeignObj    -- :: ForeignObj -> Int -> IO Int16
        , readInt32OffForeignObj    -- :: ForeignObj -> Int -> IO Int32
        , readInt64OffForeignObj    -- :: ForeignObj -> Int -> IO Int64

        , readWordOffForeignObj     -- :: ForeignObj -> Int -> IO Word
        , readWord8OffForeignObj    -- :: ForeignObj -> Int -> IO Word8
        , readWord16OffForeignObj   -- :: ForeignObj -> Int -> IO Word16
        , readWord32OffForeignObj   -- :: ForeignObj -> Int -> IO Word32
        , readWord64OffForeignObj   -- :: ForeignObj -> Int -> IO Word64

        , writeCharOffForeignObj    -- :: ForeignObj -> Int -> Char   -> IO ()
        , writeAddrOffForeignObj    -- :: ForeignObj -> Int -> Addr   -> IO ()
        , writeFloatOffForeignObj   -- :: ForeignObj -> Int -> Float  -> IO ()
        , writeDoubleOffForeignObj  -- :: ForeignObj -> Int -> Double -> IO ()

        , writeIntOffForeignObj     -- :: ForeignObj -> Int -> Int    -> IO ()
        , writeInt8OffForeignObj    -- :: ForeignObj -> Int -> Int8   -> IO ()
        , writeInt16OffForeignObj   -- :: ForeignObj -> Int -> Int16  -> IO ()
        , writeInt32OffForeignObj   -- :: ForeignObj -> Int -> Int32  -> IO ()
        , writeInt64OffForeignObj   -- :: ForeignObj -> Int -> Int64  -> IO ()

        , writeWordOffForeignObj    -- :: ForeignObj -> Int -> Word   -> IO ()
        , writeWord8OffForeignObj   -- :: ForeignObj -> Int -> Word8  -> IO ()
        , writeWord16OffForeignObj  -- :: ForeignObj -> Int -> Word16 -> IO ()
        , writeWord32OffForeignObj  -- :: ForeignObj -> Int -> Word32 -> IO ()
        , writeWord64OffForeignObj  -- :: ForeignObj -> Int -> Word64 -> IO ()
        ) 
#endif
	where
\end{code}

#ifndef __PARALLEL_HASKELL__

\begin{code}
import PrelAddr		( Addr(..), Word(..) )
import PrelIOBase	( ForeignObj(..) )
import qualified PrelForeign ( makeForeignObj )
import PrelForeign	( writeForeignObj )
import PrelWeak		( addForeignFinalizer )
import PrelGHC		( indexCharOffForeignObj#,   indexIntOffForeignObj#
			, indexAddrOffForeignObj#,   indexFloatOffForeignObj#
			, indexDoubleOffForeignObj#, indexWordOffForeignObj#
			)
import PrelBase		( Int(..), Char(..) )
import PrelFloat	( Float(..), Double(..) )
import IOExts		( unsafePerformIO )
import Int		( indexInt8OffForeignObj,  indexInt16OffForeignObj
			, indexInt32OffForeignObj, indexInt64OffForeignObj
			, readInt8OffForeignObj,   readInt16OffForeignObj
			, readInt32OffForeignObj,  readInt64OffForeignObj
			, writeInt8OffForeignObj,  writeInt16OffForeignObj
			, writeInt32OffForeignObj, writeInt64OffForeignObj
			)
import Word		( indexWord8OffForeignObj,  indexWord16OffForeignObj
			, indexWord32OffForeignObj, indexWord64OffForeignObj
			, readWord8OffForeignObj,   readWord16OffForeignObj
			, readWord32OffForeignObj,  readWord64OffForeignObj
			, writeWord8OffForeignObj,  writeWord16OffForeignObj
			, writeWord32OffForeignObj, writeWord64OffForeignObj
			)
\end{code}

\begin{code}
newForeignObj :: Addr -> IO () -> IO ForeignObj
newForeignObj = PrelForeign.makeForeignObj

-- For backward compatibility's sake
makeForeignObj :: Addr -> Addr -> IO ForeignObj
makeForeignObj adr finAdr = newForeignObj adr (ap0 finAdr adr)

foreign import dynamic ap0 :: Addr -> (Addr -> IO ())

foreignObjToAddr :: ForeignObj -> Addr
foreignObjToAddr fo = unsafePerformIO (_casm_ `` %r=(StgAddr)%0; '' fo)
\end{code}

read value out of immutable memory

\begin{code}
indexCharOffForeignObj   :: ForeignObj -> Int -> Char
indexCharOffForeignObj (ForeignObj fo#) (I# i#) = C# (indexCharOffForeignObj# fo# i#)

indexIntOffForeignObj    :: ForeignObj -> Int -> Int
indexIntOffForeignObj (ForeignObj fo#) (I# i#) = I# (indexIntOffForeignObj# fo# i#)

indexWordOffForeignObj    :: ForeignObj -> Int -> Word
indexWordOffForeignObj (ForeignObj fo#) (I# i#) = W# (indexWordOffForeignObj# fo# i#)

indexAddrOffForeignObj   :: ForeignObj -> Int -> Addr
indexAddrOffForeignObj (ForeignObj fo#) (I# i#) = A# (indexAddrOffForeignObj# fo# i#)

indexFloatOffForeignObj  :: ForeignObj -> Int -> Float
indexFloatOffForeignObj (ForeignObj fo#) (I# i#) = F# (indexFloatOffForeignObj# fo# i#)

indexDoubleOffForeignObj :: ForeignObj -> Int -> Double
indexDoubleOffForeignObj (ForeignObj fo#) (I# i#) = D# (indexDoubleOffForeignObj# fo# i#)
\end{code}

read value out of mutable memory

\begin{code}
readCharOffForeignObj    :: ForeignObj -> Int -> IO Char
readCharOffForeignObj fo i = _casm_ `` %r=(StgChar)(((StgChar*)%0)[(StgInt)%1]); '' fo i

readIntOffForeignObj     :: ForeignObj -> Int -> IO Int
readIntOffForeignObj fo i = _casm_ `` %r=(StgInt)(((StgInt*)%0)[(StgInt)%1]); '' fo i

readWordOffForeignObj     :: ForeignObj -> Int -> IO Word
readWordOffForeignObj fo i = _casm_ `` %r=(StgWord)(((StgWord*)%0)[(StgInt)%1]); '' fo i

readAddrOffForeignObj    :: ForeignObj -> Int -> IO Addr
readAddrOffForeignObj fo i = _casm_ `` %r=(StgAddr)(((StgAddr*)%0)[(StgInt)%1]); '' fo i

readFloatOffForeignObj   :: ForeignObj -> Int -> IO Float
readFloatOffForeignObj fo i = _casm_ `` %r=(StgFloat)(((StgFloat*)%0)[(StgInt)%1]); '' fo i

readDoubleOffForeignObj  :: ForeignObj -> Int -> IO Double
readDoubleOffForeignObj fo i = _casm_ `` %r=(StgDouble)(((StgDouble*)%0)[(StgInt)%1]); '' fo i
\end{code}

write value into mutable memory

\begin{code}
writeCharOffForeignObj   :: ForeignObj -> Int -> Char   -> IO ()
writeCharOffForeignObj fo i e = _casm_ `` (((StgChar*)%0)[(StgInt)%1])=(StgChar)%2; '' fo i e

writeIntOffForeignObj    :: ForeignObj -> Int -> Int    -> IO ()
writeIntOffForeignObj fo i e = _casm_ `` (((StgInt*)%0)[(StgInt)%1])=(StgInt)%2; '' fo i e

writeWordOffForeignObj    :: ForeignObj -> Int -> Word  -> IO ()
writeWordOffForeignObj fo i e = _casm_ `` (((StgWord*)%0)[(StgInt)%1])=(StgWord)%2; '' fo i e

writeAddrOffForeignObj   :: ForeignObj -> Int -> Addr   -> IO ()
writeAddrOffForeignObj fo i e = _casm_ `` (((StgAddr*)%0)[(StgInt)%1])=(StgAddr)%2; ''fo i e

writeFloatOffForeignObj  :: ForeignObj -> Int -> Float  -> IO ()
writeFloatOffForeignObj fo i e = _casm_ `` (((StgFloat*)%0)[(StgInt)%1])=(StgFloat)%2; '' fo i e

writeDoubleOffForeignObj :: ForeignObj -> Int -> Double -> IO ()
writeDoubleOffForeignObj fo i e = _casm_ `` (((StgDouble*)%0)[(StgInt)%1])=(StgDouble)%2; '' fo i e
\end{code}

#endif
