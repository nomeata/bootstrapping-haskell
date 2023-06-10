%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[PrelByteArr]{Module @PrelByteArr@}

Byte-arrays are flat arrays of non-pointers only.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelByteArr where

import {-# SOURCE #-} PrelErr ( error )
import PrelArr
import PrelFloat
import PrelList (foldl)
import PrelST
import PrelBase
import PrelAddr
import PrelGHC

\end{code}

%*********************************************************
%*							*
\subsection{The @Array@ types}
%*							*
%*********************************************************

\begin{code}
data Ix ix => ByteArray ix      	= ByteArray	   ix ix ByteArray#
data Ix ix => MutableByteArray s ix     = MutableByteArray ix ix (MutableByteArray# s)

instance CCallable (ByteArray ix)
instance CCallable (MutableByteArray RealWorld ix)
	-- Note the RealWorld!  You can only ccall with MutableByteArray args
	-- which are in the real world.  When this was missed out, the result
	-- was that a CCallOpId had a free tyvar, and since the compiler doesn't
	-- expect that it didn't get zonked or substituted.  Bad news.

instance Eq (MutableByteArray s ix) where
	MutableByteArray _ _ arr1# == MutableByteArray _ _ arr2#
		= sameMutableByteArray# arr1# arr2#
\end{code}

%*********************************************************
%*							*
\subsection{Operations on mutable arrays}
%*							*
%*********************************************************

Idle ADR question: What's the tradeoff here between flattening these
datatypes into @MutableArray ix ix (MutableArray# s elt)@ and using
it as is?  As I see it, the former uses slightly less heap and
provides faster access to the individual parts of the bounds while the
code used has the benefit of providing a ready-made @(lo, hi)@ pair as
required by many array-related functions.  Which wins? Is the
difference significant (probably not).

Idle AJG answer: When I looked at the outputted code (though it was 2
years ago) it seems like you often needed the tuple, and we build
it frequently. Now we've got the overloading specialiser things
might be different, though.

\begin{code}
newCharArray, newIntArray, newWordArray, newAddrArray, newFloatArray, newDoubleArray
	 :: Ix ix => (ix,ix) -> ST s (MutableByteArray s ix) 

{-# SPECIALIZE newCharArray   :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newIntArray    :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newWordArray   :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newAddrArray   :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newFloatArray  :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newDoubleArray :: IPr -> ST s (MutableByteArray s Int) #-}

newCharArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newCharArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray l u barr# #) }}

newIntArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newIntArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray l u barr# #) }}

newWordArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newWordArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray l u barr# #) }}

newAddrArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newAddrArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray l u barr# #) }}

newFloatArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newFloatArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray l u barr# #) }}

newDoubleArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newDoubleArray# n# s#)  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray l u barr# #) }}


readCharArray   :: Ix ix => MutableByteArray s ix -> ix -> ST s Char 
readIntArray    :: Ix ix => MutableByteArray s ix -> ix -> ST s Int
readWordArray   :: Ix ix => MutableByteArray s ix -> ix -> ST s Word
readAddrArray   :: Ix ix => MutableByteArray s ix -> ix -> ST s Addr
readFloatArray  :: Ix ix => MutableByteArray s ix -> ix -> ST s Float
readDoubleArray :: Ix ix => MutableByteArray s ix -> ix -> ST s Double

{-# SPECIALIZE readCharArray   :: MutableByteArray s Int -> Int -> ST s Char #-}
{-# SPECIALIZE readIntArray    :: MutableByteArray s Int -> Int -> ST s Int #-}
{-# SPECIALIZE readAddrArray   :: MutableByteArray s Int -> Int -> ST s Addr #-}
--NO:{-# SPECIALIZE readFloatArray  :: MutableByteArray s Int -> Int -> ST s Float #-}
{-# SPECIALIZE readDoubleArray :: MutableByteArray s Int -> Int -> ST s Double #-}

readCharArray (MutableByteArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	of { I# n# ->
    case readCharArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, C# r# #) }}

readIntArray (MutableByteArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	of { I# n# ->
    case readIntArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, I# r# #) }}

readWordArray (MutableByteArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	of { I# n# ->
    case readWordArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, W# r# #) }}

readAddrArray (MutableByteArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	of { I# n# ->
    case readAddrArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, A# r# #) }}

readFloatArray (MutableByteArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	of { I# n# ->
    case readFloatArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, F# r# #) }}

readDoubleArray (MutableByteArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n) 	    	of { I# n# ->
    case readDoubleArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, D# r# #) }}

--Indexing of ordinary @Arrays@ is standard Haskell and isn't defined here.
indexCharArray   :: Ix ix => ByteArray ix -> ix -> Char 
indexIntArray    :: Ix ix => ByteArray ix -> ix -> Int
indexWordArray   :: Ix ix => ByteArray ix -> ix -> Word
indexAddrArray   :: Ix ix => ByteArray ix -> ix -> Addr
indexFloatArray  :: Ix ix => ByteArray ix -> ix -> Float
indexDoubleArray :: Ix ix => ByteArray ix -> ix -> Double

{-# SPECIALIZE indexCharArray   :: ByteArray Int -> Int -> Char #-}
{-# SPECIALIZE indexIntArray    :: ByteArray Int -> Int -> Int #-}
{-# SPECIALIZE indexAddrArray   :: ByteArray Int -> Int -> Addr #-}
--NO:{-# SPECIALIZE indexFloatArray  :: ByteArray Int -> Int -> Float #-}
{-# SPECIALIZE indexDoubleArray :: ByteArray Int -> Int -> Double #-}

indexCharArray (ByteArray l u barr#) n
  = case (index (l,u) n)	    	of { I# n# ->
    case indexCharArray# barr# n# 	of { r# ->
    (C# r#)}}

indexIntArray (ByteArray l u barr#) n
  = case (index (l,u) n)	    	of { I# n# ->
    case indexIntArray# barr# n# 	of { r# ->
    (I# r#)}}

indexWordArray (ByteArray l u barr#) n
  = case (index (l,u) n)	    	of { I# n# ->
    case indexWordArray# barr# n# 	of { r# ->
    (W# r#)}}

indexAddrArray (ByteArray l u barr#) n
  = case (index (l,u) n)	    	of { I# n# ->
    case indexAddrArray# barr# n# 	of { r# ->
    (A# r#)}}

indexFloatArray (ByteArray l u barr#) n
  = case (index (l,u) n)	    	of { I# n# ->
    case indexFloatArray# barr# n# 	of { r# ->
    (F# r#)}}

indexDoubleArray (ByteArray l u barr#) n
  = case (index (l,u) n) 	    	of { I# n# ->
    case indexDoubleArray# barr# n# 	of { r# ->
    (D# r#)}}

writeCharArray   :: Ix ix => MutableByteArray s ix -> ix -> Char -> ST s () 
writeIntArray    :: Ix ix => MutableByteArray s ix -> ix -> Int  -> ST s () 
writeWordArray   :: Ix ix => MutableByteArray s ix -> ix -> Word -> ST s () 
writeAddrArray   :: Ix ix => MutableByteArray s ix -> ix -> Addr -> ST s () 
writeFloatArray  :: Ix ix => MutableByteArray s ix -> ix -> Float -> ST s () 
writeDoubleArray :: Ix ix => MutableByteArray s ix -> ix -> Double -> ST s () 

{-# SPECIALIZE writeCharArray   :: MutableByteArray s Int -> Int -> Char -> ST s () #-}
{-# SPECIALIZE writeIntArray    :: MutableByteArray s Int -> Int -> Int  -> ST s () #-}
{-# SPECIALIZE writeAddrArray   :: MutableByteArray s Int -> Int -> Addr -> ST s () #-}
--NO:{-# SPECIALIZE writeFloatArray  :: MutableByteArray s Int -> Int -> Float -> ST s () #-}
{-# SPECIALIZE writeDoubleArray :: MutableByteArray s Int -> Int -> Double -> ST s () #-}

writeCharArray (MutableByteArray l u barr#) n (C# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeCharArray# barr# n# ele s#    of { s2#   ->
    (# s2#, () #) }}

writeIntArray (MutableByteArray l u barr#) n (I# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeIntArray# barr# n# ele s#     of { s2#   ->
    (# s2#, () #) }}

writeWordArray (MutableByteArray l u barr#) n (W# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeWordArray# barr# n# ele s#    of { s2#   ->
    (# s2#, () #) }}

writeAddrArray (MutableByteArray l u barr#) n (A# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeAddrArray# barr# n# ele s#    of { s2#   ->
    (# s2#, () #) }}

writeFloatArray (MutableByteArray l u barr#) n (F# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeFloatArray# barr# n# ele s#   of { s2#   ->
    (# s2#, () #) }}

writeDoubleArray (MutableByteArray l u barr#) n (D# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeDoubleArray# barr# n# ele s#  of { s2#   ->
    (# s2#, () #) }}
\end{code}
