-----------------------------------------------------------------------------
--  $Id: ArrayBase.hs,v 1.1 2000/04/28 15:29:18 simonmar Exp $
--
-- (c) The GHC Team 2000
--
-- Basis for IArray and MArray.  Not intended for external consumption; use
-- IArray or MArray instead.

module ArrayBase where

import ST
import Addr
import StablePtr

import qualified Array
import Ix

import Prelude hiding (read)

import PrelST
import PrelStable
import PrelFloat
import PrelBase
import PrelGHC
import PrelAddr

-----------------------------------------------------------------------------
-- Class of immutable arrays:

class HasBounds a => IArray a e where
   (!)     :: Ix ix => a ix e -> ix -> e
   array   :: Ix ix => (ix,ix) -> [(ix,e)] -> a ix e	

class HasBounds a where
   bounds :: Ix ix => a ix e -> (ix,ix)

-----------------------------------------------------------------------------
-- Normal polymorphic arrays

instance IArray Array.Array e where
   (!)    = (Array.!)
   array  = Array.array

instance HasBounds Array.Array where
   bounds = Array.bounds

-----------------------------------------------------------------------------
-- Flat unboxed arrays

data UArray ix a = UArray ix ix ByteArray#

instance HasBounds UArray where
   bounds (UArray l u _) = (l,u)

newUArray :: (Ix ix, MArray (STUArray s) e (ST s)) =>
  (ix,ix) -> [(ix,e)] -> ST s (UArray ix e)
{-# INLINE newUArray #-}
newUArray ixs ivs 
  = do  marr <- marray ixs
	foldr (fill marr) (unsafeFreezeSTUArray marr) ivs

{-# INLINE fill #-}
fill :: (Ix ix, MArray (STUArray s) e (ST s)) => 
	STUArray s ix e -> (ix,e) -> ST s a -> ST s a
fill marr (i,v) next = put marr i v >> next

unsafeFreezeSTUArray :: Ix ix => STUArray s ix e -> ST s (UArray ix e)
unsafeFreezeSTUArray (STUArray l u arr#) = ST $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of { (# s2#, frozen #) ->
    (# s2#, UArray l u frozen #) }

-----------------------------------------------------------------------------
-- Flat arrays

instance IArray UArray Char where
   (!) = indexCharArray
   array ixs ivs  = runST (newUArray ixs ivs)

instance IArray UArray Int where
   (!) = indexIntArray
   array ixs ivs  = runST (newUArray ixs ivs)

instance IArray UArray Word where
   (!) = indexWordArray
   array ixs ivs  = runST (newUArray ixs ivs)

instance IArray UArray Addr where
   (!) = indexAddrArray
   array ixs ivs  = runST (newUArray ixs ivs)

instance IArray UArray Float where
   (!) = indexFloatArray
   array ixs ivs  = runST (newUArray ixs ivs)

instance IArray UArray Double where
   (!) = indexDoubleArray
   array ixs ivs  = runST (newUArray ixs ivs)

-----------------------------------------------------------------------------
-- Mutable arrays

class (Monad m, HasBounds a) => MArray a e m where
   get     :: Ix ix => a ix e -> ix -> m e
   put     :: Ix ix => a ix e -> ix -> e -> m ()
   marray  :: Ix ix => (ix,ix) -> m (a ix e)

-----------------------------------------------------------------------------
-- Polymorphic non-strict mutable arrays (ST monad)

instance HasBounds (STArray s) where
   bounds     = boundsSTArray

instance MArray (STArray s) e (ST s) where
   get       = readSTArray
   put      = writeSTArray
   marray ixs = newSTArray ixs st_bot

st_bot = error "STArray: undefined element" 
	--ToDo: throw (ArrayException UndefinedElement)

-----------------------------------------------------------------------------
-- Flat unboxed mutable arrays (ST monad)

data STUArray s ix a = STUArray ix ix (MutableByteArray# s)

boundsSTUArray (STUArray l u _) = (l,u)

instance HasBounds (STUArray s) where
   bounds = boundsSTUArray

instance MArray (STUArray s) Char (ST s) where
   get    = readCharArray
   put    = writeCharArray
   marray = newCharArray

instance MArray (STUArray s) Int (ST s) where
   get    = readIntArray
   put    = writeIntArray
   marray = newIntArray

instance MArray (STUArray s) Word (ST s) where
   get    = readWordArray
   put    = writeWordArray
   marray = newWordArray

instance MArray (STUArray s) Addr (ST s) where
   get    = readAddrArray
   put    = writeAddrArray
   marray = newAddrArray

instance MArray (STUArray s) Float (ST s) where
   get    = readFloatArray
   put    = writeFloatArray
   marray = newFloatArray

instance MArray (STUArray s) Double (ST s) where
   get    = readDoubleArray
   put    = writeDoubleArray
   marray = newDoubleArray

instance MArray (STUArray s) (StablePtr a) (ST s) where
   get    = readStablePtrArray
   put    = writeStablePtrArray
   marray = newStablePtrArray

-----------------------------------------------------------------------------
-- Low-level operations: firstly, creating new mutable arrays

type IPr = (Int, Int)

{-# SPECIALIZE newCharArray   :: IPr -> ST s (STUArray s Int Char) #-}
{-# SPECIALIZE newIntArray    :: IPr -> ST s (STUArray s Int Int) #-}
{-# SPECIALIZE newWordArray   :: IPr -> ST s (STUArray s Int Word) #-}
{-# SPECIALIZE newAddrArray   :: IPr -> ST s (STUArray s Int Addr) #-}
{-# SPECIALIZE newFloatArray  :: IPr -> ST s (STUArray s Int Float) #-}
{-# SPECIALIZE newDoubleArray :: IPr -> ST s (STUArray s Int Double) #-}

newCharArray :: Ix ix => (ix,ix) -> ST s (STUArray s ix Char)
newCharArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newCharArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, STUArray l u barr# #) }}

newIntArray :: Ix ix => (ix,ix) -> ST s (STUArray s ix Int)
newIntArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newIntArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, STUArray l u barr# #) }}

newWordArray :: Ix ix => (ix,ix) -> ST s (STUArray s ix Word)
newWordArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newWordArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, STUArray l u barr# #) }}

newAddrArray :: Ix ix => (ix,ix) -> ST s (STUArray s ix Addr)
newAddrArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newAddrArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, STUArray l u barr# #) }}

newFloatArray :: Ix ix => (ix,ix) -> ST s (STUArray s ix Float)
newFloatArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newFloatArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, STUArray l u barr# #) }}

newDoubleArray :: Ix ix => (ix,ix) -> ST s (STUArray s ix Double)
newDoubleArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newDoubleArray# n# s#)  of { (# s2#, barr# #) ->
    (# s2#, STUArray l u barr# #) }}

newStablePtrArray :: Ix ix => (ix,ix) -> ST s (STUArray s ix (StablePtr a))
newStablePtrArray ixs@(l,u) = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newStablePtrArray# n# s#) of { (# s2#, barr# #) ->
    (# s2#, STUArray l u barr# #) }}

-----------------------------------------------------------------------------
-- reading from mutable unboxed arrays

readCharArray   :: Ix ix => STUArray s ix Char   -> ix -> ST s Char 
readIntArray    :: Ix ix => STUArray s ix Int    -> ix -> ST s Int
readWordArray   :: Ix ix => STUArray s ix Word   -> ix -> ST s Word
readAddrArray   :: Ix ix => STUArray s ix Addr   -> ix -> ST s Addr
readFloatArray  :: Ix ix => STUArray s ix Float  -> ix -> ST s Float
readDoubleArray :: Ix ix => STUArray s ix Double -> ix -> ST s Double
readStablePtrArray :: Ix ix => STUArray s ix (StablePtr a) -> ix -> ST s (StablePtr a)

{-# SPECIALIZE readCharArray   :: STUArray s Int Char -> Int -> ST s Char #-}
{-# SPECIALIZE readIntArray    :: STUArray s Int Int -> Int -> ST s Int #-}
{-# SPECIALIZE readAddrArray   :: STUArray s Int Addr -> Int -> ST s Addr #-}
--NO:{-# SPECIALIZE readFloatArray  :: STUArray s Int Float -> Int -> ST s Float #-}
{-# SPECIALIZE readDoubleArray :: STUArray s Int Double -> Int -> ST s Double #-}

readCharArray (STUArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	of { I# n# ->
    case readCharArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, C# r# #) }}

readIntArray (STUArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	of { I# n# ->
    case readIntArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, I# r# #) }}

readWordArray (STUArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	of { I# n# ->
    case readWordArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, W# r# #) }}

readAddrArray (STUArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	of { I# n# ->
    case readAddrArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, A# r# #) }}

readFloatArray (STUArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	of { I# n# ->
    case readFloatArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, F# r# #) }}

readDoubleArray (STUArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n) 	    	of { I# n# ->
    case readDoubleArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, D# r# #) }}

readStablePtrArray (STUArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)    	    	  of { I# n# ->
    case readStablePtrArray# barr# n# s#  of { (# s2#, r# #) ->
    (# s2# , StablePtr r# #) }}

-----------------------------------------------------------------------------
-- writing into mutable unboxed arrays

writeCharArray   :: Ix ix => STUArray s ix Char   -> ix -> Char -> ST s () 
writeIntArray    :: Ix ix => STUArray s ix Int    -> ix -> Int  -> ST s () 
writeWordArray   :: Ix ix => STUArray s ix Word   -> ix -> Word -> ST s () 
writeAddrArray   :: Ix ix => STUArray s ix Addr   -> ix -> Addr -> ST s () 
writeFloatArray  :: Ix ix => STUArray s ix Float  -> ix -> Float -> ST s () 
writeDoubleArray :: Ix ix => STUArray s ix Double -> ix -> Double -> ST s () 
writeStablePtrArray :: Ix ix => STUArray s ix (StablePtr a) -> ix -> StablePtr a -> ST s () 

{-# SPECIALIZE writeCharArray   :: STUArray s Int Char -> Int -> Char -> ST s () #-}
{-# SPECIALIZE writeIntArray    :: STUArray s Int Int -> Int -> Int  -> ST s () #-}
{-# SPECIALIZE writeAddrArray   :: STUArray s Int Addr -> Int -> Addr -> ST s () #-}
--NO:{-# SPECIALIZE writeFloatArray  :: STUArray s Int Float -> Int -> Float -> ST s () #-}
{-# SPECIALIZE writeDoubleArray :: STUArray s Int Double -> Int -> Double -> ST s () #-}

writeCharArray (STUArray l u barr#) n (C# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeCharArray# barr# n# ele s#    of { s2#   ->
    (# s2#, () #) }}

writeIntArray (STUArray l u barr#) n (I# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeIntArray# barr# n# ele s#     of { s2#   ->
    (# s2#, () #) }}

writeWordArray (STUArray l u barr#) n (W# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeWordArray# barr# n# ele s#    of { s2#   ->
    (# s2#, () #) }}

writeAddrArray (STUArray l u barr#) n (A# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeAddrArray# barr# n# ele s#    of { s2#   ->
    (# s2#, () #) }}

writeFloatArray (STUArray l u barr#) n (F# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeFloatArray# barr# n# ele s#   of { s2#   ->
    (# s2#, () #) }}

writeDoubleArray (STUArray l u barr#) n (D# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeDoubleArray# barr# n# ele s#  of { s2#   ->
    (# s2#, () #) }}

writeStablePtrArray (STUArray l u barr#) n (StablePtr sp#) = ST $ \ s# ->
    case (index (l,u) n)    	    	       of { I# n# ->
    case writeStablePtrArray# barr# n# sp# s#  of { s2#   ->
    (# s2# , () #) }}

-----------------------------------------------------------------------------
-- indexing into immutable unboxed arrays

indexCharArray      :: Ix ix => UArray ix Char   -> ix -> Char 
indexIntArray       :: Ix ix => UArray ix Int    -> ix -> Int
indexWordArray      :: Ix ix => UArray ix Word   -> ix -> Word
indexAddrArray      :: Ix ix => UArray ix Addr   -> ix -> Addr
indexFloatArray     :: Ix ix => UArray ix Float  -> ix -> Float
indexDoubleArray    :: Ix ix => UArray ix Double -> ix -> Double
indexStablePtrArray :: Ix ix => UArray ix (StablePtr a) -> ix -> (StablePtr a)

{-# SPECIALIZE indexCharArray   :: UArray Int Char -> Int -> Char #-}
{-# SPECIALIZE indexIntArray    :: UArray Int Int  -> Int -> Int #-}
{-# SPECIALIZE indexAddrArray   :: UArray Int Addr -> Int -> Addr #-}
--NO:{-# SPECIALIZE indexFloatArray  :: UArray Int Float -> Int -> Float #-}
{-# SPECIALIZE indexDoubleArray :: UArray Int Double -> Int -> Double #-}

indexCharArray (UArray l u barr#) n
  = case (index (l,u) n)	    	of { I# n# ->
    case indexCharArray# barr# n# 	of { r# ->
    (C# r#)}}

indexIntArray (UArray l u barr#) n
  = case (index (l,u) n)	    	of { I# n# ->
    case indexIntArray# barr# n# 	of { r# ->
    (I# r#)}}

indexWordArray (UArray l u barr#) n
  = case (index (l,u) n)	    	of { I# n# ->
    case indexWordArray# barr# n# 	of { r# ->
    (W# r#)}}

indexAddrArray (UArray l u barr#) n
  = case (index (l,u) n)	    	of { I# n# ->
    case indexAddrArray# barr# n# 	of { r# ->
    (A# r#)}}

indexFloatArray (UArray l u barr#) n
  = case (index (l,u) n)	    	of { I# n# ->
    case indexFloatArray# barr# n# 	of { r# ->
    (F# r#)}}

indexDoubleArray (UArray l u barr#) n
  = case (index (l,u) n) 	    	of { I# n# ->
    case indexDoubleArray# barr# n# 	of { r# ->
    (D# r#)}}

indexStablePtrArray (UArray l u barr#) n
  = case (index (l,u) n) 	    	of { I# n# ->
    case indexStablePtrArray# barr# n# 	of { r# ->
    (StablePtr r#)}}
