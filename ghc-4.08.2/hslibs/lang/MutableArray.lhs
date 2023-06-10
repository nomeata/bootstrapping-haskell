%
% (c) The AQUA Project, Glasgow University, 1997
%
\section[MutableArray]{The @MutableArray@ interface}

Mutable (byte)arrays interface, re-exports type types and operations
over them from @ArrBase@. Have to be used in conjunction with
@ST@.

\begin{code}
module MutableArray 
   (
    MutableByteArray(..),

    ST,
    Ix,

    -- Creators:
    newCharArray,
    newAddrArray,
    newIntArray,
    newFloatArray,
    newDoubleArray,
    newStablePtrArray,  -- :: Ix ix => (ix,ix) -> ST s (MutableByteArray s ix) 

    boundsOfMutableByteArray, -- :: Ix ix => MutableByteArray s ix -> (ix, ix)

    readCharArray,      -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Char 
    readIntArray,       -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Int
    readAddrArray,      -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Addr
    readFloatArray,     -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Float
    readDoubleArray,    -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Double
    readStablePtrArray, -- :: Ix ix => MutableByteArray s ix -> ix -> ST s (StablePtr a)

    writeCharArray,       -- :: Ix ix => MutableByteArray s ix -> ix -> Char -> ST s () 
    writeIntArray,        -- :: Ix ix => MutableByteArray s ix -> ix -> Int  -> ST s () 
    writeAddrArray,       -- :: Ix ix => MutableByteArray s ix -> ix -> Addr -> ST s () 
    writeFloatArray,      -- :: Ix ix => MutableByteArray s ix -> ix -> Float -> ST s () 
    writeDoubleArray,     -- :: Ix ix => MutableByteArray s ix -> ix -> Double -> ST s () 
    writeStablePtrArray,  -- :: Ix ix => MutableByteArray s ix -> ix -> StablePtr a -> ST s () 

    freezeByteArray,	  -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    unsafeFreezeByteArray, -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)

    thawByteArray,         -- :: Ix ix => ByteArray ix -> ST s (MutableByteArray s ix)
    unsafeThawByteArray,   -- :: Ix ix => ByteArray ix -> ST s (MutableByteArray s ix)

     -- the sizes are reported back are *in bytes*.
    sizeofMutableByteArray, -- :: Ix ix => MutableByteArray s ix -> Int

    readWord8Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Word8
    readWord16Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Word16
    readWord32Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Word32
    readWord64Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Word64

    writeWord8Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> Word8  -> ST s ()
    writeWord16Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> Word16 -> ST s ()
    writeWord32Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> Word32 -> ST s ()
    writeWord64Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> Word64 -> ST s ()

    readInt8Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Int8
    readInt16Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Int16
    readInt32Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Int32
    readInt64Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Int64

    writeInt8Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> Int8  -> ST s ()
    writeInt16Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> Int16 -> ST s ()
    writeInt32Array,	    -- :: Ix ix => MutableByteArray s ix -> ix -> Int32 -> ST s ()
    writeInt64Array	    -- :: Ix ix => MutableByteArray s ix -> ix -> Int64 -> ST s ()

    ) where

import PrelBase
import PrelArr
import PrelByteArr
import PrelAddr
import PrelArrExtra
import PrelStable
import PrelST
import PrelGHC (unsafeCoerce#)
import ST
import Ix
import Word
import Int

\end{code}

Note: the absence of operations to read/write ForeignObjs to a mutable
array is not accidental; storing foreign objs in a mutable array is
not supported.

\begin{code}
sizeofMutableByteArray :: Ix ix => MutableByteArray s ix -> Int
sizeofMutableByteArray (MutableByteArray _ _ arr#) = 
  case (sizeofMutableByteArray# arr#) of
    i# -> (I# i#)

\end{code}

\begin{code}
newStablePtrArray :: Ix ix => (ix,ix) -> ST s (MutableByteArray s ix) 
newStablePtrArray ixs@(l,u) = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newStablePtrArray# n# s#) of { (# s2#, barr# #) ->
    (# s2#, (MutableByteArray l u barr#) #) }}

readStablePtrArray    :: Ix ix => MutableByteArray s ix -> ix -> ST s (StablePtr a)
readStablePtrArray (MutableByteArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)    	    	  of { I# n# ->
    case readStablePtrArray# barr# n# s#  of { (# s2#, r# #) ->
    (# s2# , (StablePtr r#) #) }}

writeStablePtrArray    :: Ix ix => MutableByteArray s ix -> ix -> StablePtr a  -> ST s () 
writeStablePtrArray (MutableByteArray l u barr#) n (StablePtr sp#) = ST $ \ s# ->
    case (index (l,u) n)    	    	       of { I# n# ->
    case writeStablePtrArray# barr# n# sp# s#  of { s2#   ->
    (# s2# , () #) }}
\end{code}


Reminder: indexing an array at some base type is done in units
of the size of the type being; *not* in bytes.

\begin{code}
readWord8Array  :: (Ix ix) => MutableByteArray s ix -> ix -> ST s Word8
readWord16Array :: (Ix ix) => MutableByteArray s ix -> ix -> ST s Word16
readWord32Array :: (Ix ix) => MutableByteArray s ix -> ix -> ST s Word32

readWord8Array (MutableByteArray l u arr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	    of { I# n# ->
    case readCharArray# arr# n# s#  of { (# s2# , r# #) ->
    (# s2# , intToWord8 (I# (ord# r#)) #) }}


readWord16Array (MutableByteArray l u arr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	                    of { I# n# ->
    case readWordArray# arr# (n# `quotInt#` 2#) s#  of { (# s2# , w# #) -> 
    case n# `remInt#` 2# of
      0# -> (# s2# , wordToWord16 (W# w#) #)           
              -- the double byte hides in the lower half of the wrd.
      1# -> (# s2# , wordToWord16 (W# (shiftRL# w# 16#)) #)  
              -- take the upper 16 bits.
    }}

readWord32Array (MutableByteArray l u arr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	        of { I# n# ->
    case readWordArray# arr# n# s#      of { (# s2# , w# #) ->
    (# s2# , wordToWord32 (W# w#) #) }}


  -- FIXME, Num shouldn't be required, but it makes my life easier.
readWord64Array :: (Num ix, Ix ix) => MutableByteArray s ix -> ix -> ST s Word64
readWord64Array mb n = do
  l <- readWord32Array mb (2*n)
  h <- readWord32Array mb (2*n + 1)
#ifdef WORDS_BIGENDIAN
  return ( word32ToWord64 h + word32ToWord64 l * word32ToWord64 (maxBound::Word32))  
#else
  return ( word32ToWord64 l + word32ToWord64 h * word32ToWord64 (maxBound::Word32))  
#endif

writeWord8Array  :: (Ix ix) => MutableByteArray s ix -> ix -> Word8  -> ST s ()
writeWord16Array :: (Ix ix) => MutableByteArray s ix -> ix -> Word16 -> ST s ()
writeWord32Array :: (Ix ix) => MutableByteArray s ix -> ix -> Word32 -> ST s ()

writeWord8Array (MutableByteArray l u arr#) n w = ST $ \ s# ->
    case word8ToWord w of
       W# w# -> case index (l,u) n of 
                   I# n# -> case writeCharArray# arr# n# (chr# (word2Int# w#)) s#  of 
                               s2# -> (# s2# , () #)

writeWord16Array (MutableByteArray l u arr#) n w = ST $ \ s# ->
    case (index (l,u) n) of 
      I# n# -> 
	 let
          w# = 
            let W# w' = word16ToWord w in
            case n# `remInt#` 2# of
              0# -> w'
	      1# -> shiftL# w' 16#
   
          mask =
            case n# `remInt#` 2# of
              0# -> case ``0xffff0000'' of W# x -> x   -- writing to the lower half of the word.
              1# -> int2Word# 0x0000ffff#
         in
         case readWordArray# arr# (n# `quotInt#` 2#) s#  of 
           (# s2# , v# #) -> 
              case writeWordArray# arr# (n# `quotInt#` 2#) (w# `or#` (v# `and#` mask )) s2#  of 
               s3# -> (# s3# , () #) 

writeWord32Array (MutableByteArray l u arr#) n w = ST $ \ s# ->
    case (index (l,u) n) of 
      I# n# ->
        case writeWordArray# arr# n# w# s#  of 
          s2# -> (# s2# , () #) 
  where
   W# w# = word32ToWord w

  -- FIXME, Num shouldn't be required, but it makes my life easier.
writeWord64Array :: (Num ix, Ix ix) => MutableByteArray s ix -> ix -> Word64 -> ST s ()
writeWord64Array mb n w = do
#ifdef WORDS_BIGENDIAN
   writeWord32Array mb (n*2) h
   writeWord32Array mb (n*2+1) l
#else
   writeWord32Array mb (n*2) l
   writeWord32Array mb (n*2+1) h
#endif
  where
    h       = word64ToWord32 h'
    l       = word64ToWord32 l'
    (h',l') = w `divMod` (word32ToWord64 (maxBound::Word32) + 1)


\end{code}

\begin{code}
readInt8Array  :: (Ix ix) => MutableByteArray s ix -> ix -> ST s Int8
readInt16Array :: (Ix ix) => MutableByteArray s ix -> ix -> ST s Int16
readInt32Array :: (Ix ix) => MutableByteArray s ix -> ix -> ST s Int32

readInt8Array (MutableByteArray l u arr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	    of { I# n# ->
    case readCharArray# arr# n# s#  of { (# s2# , r# #) ->
    (# s2# , intToInt8 (I# (ord# r#)) #) }}

readInt16Array (MutableByteArray l u arr#) n = ST $ \ s# ->
    case (index (l,u) n) of 
     I# n# ->
       case readIntArray# arr# (n# `quotInt#` 2#) s#  of 
        (# s2# , i# #) -> 
          case n# `remInt#` 2# of
	     0# -> (# s2# , intToInt16 (I# i#) #)
	     1# -> (# s2# , intToInt16 (I# (word2Int# (shiftRL# (int2Word# i#) 16# ))) #)

readInt32Array (MutableByteArray l u arr#) n = ST $ \ s# ->
    case (index (l,u) n) of 
      I# n# -> case readIntArray# arr# n# s# of
                 (# s2# , i# #) -> (# s2# , intToInt32 (I# i#) #)

readInt64Array :: (Num ix, Ix ix) => MutableByteArray s ix -> ix -> ST s Int64
readInt64Array mb n = do
  l <- readInt32Array mb (2*n)
  h <- readInt32Array mb (2*n + 1)
#ifdef WORDS_BIGENDIAN
  return ( int32ToInt64 h + int32ToInt64 l * int32ToInt64 (maxBound::Int32))  
#else
  return ( int32ToInt64 l + int32ToInt64 h * int32ToInt64 (maxBound::Int32))  
#endif

writeInt8Array  :: (Ix ix) => MutableByteArray s ix -> ix -> Int8  -> ST s ()
writeInt16Array :: (Ix ix) => MutableByteArray s ix -> ix -> Int16 -> ST s ()
writeInt32Array :: (Ix ix) => MutableByteArray s ix -> ix -> Int32 -> ST s ()

writeInt8Array (MutableByteArray l u arr#) n i = ST $ \ s# ->
    case (index (l,u) n) of
      I# n# ->
        case writeCharArray# arr# n# ch s#  of 
           s2# -> (# s2# , () #) 
  where
   ch = case int8ToInt i of { I# j -> chr# j }

writeInt16Array (MutableByteArray l u arr#) n i = ST $ \ s# ->
    case (index (l,u) n) of
      I# n# ->
	 let
          i# = 
            let I# i' = int16ToInt i in
            case n# `remInt#` 2# of
              0# -> i'
	      1# -> iShiftL# i' 16#
   
          mask =
            case n# `remInt#` 2# of
              0# -> case ``0xffff0000'' of W# x -> x   -- writing to the lower half of the word.
              1# -> int2Word# 0x0000ffff#
	 in
         case readIntArray# arr# (n# `quotInt#` 2#) s#  of 
           (# s2# , v# #) ->
	      let w' = word2Int# (int2Word# i# `or#` (int2Word# v# `and#` mask))
	      in
              case writeIntArray# arr# (n# `quotInt#` 2#) w' s2#  of
                s2# -> (# s2# , () #) 

writeInt32Array (MutableByteArray l u arr#) n i = ST $ \ s# ->
   case (index (l,u) n) of
     I# n# ->
        case writeIntArray# arr# n# i# s#  of 
          s2# -> (# s2# , () #) 
  where
   I# i# = int32ToInt i

writeInt64Array :: (Num ix, Ix ix) => MutableByteArray s ix -> ix -> Int64 -> ST s ()
writeInt64Array mb n w = do
#ifdef WORDS_BIGENDIAN
   writeInt32Array mb (n*2) h
   writeInt32Array mb (n*2+1) l
#else
   writeInt32Array mb (n*2)   l
   writeInt32Array mb (n*2+1) h
#endif
  where
    h       = int64ToInt32 h'
    l       = int64ToInt32 l'
    (h',l') = w `divMod` (int32ToInt64 (maxBound::Int32) * 2 - 1)

\end{code}

\begin{code}
{-# SPECIALIZE boundsOfMutableByteArray :: MutableByteArray s Int -> IPr #-}
boundsOfMutableByteArray :: Ix ix => MutableByteArray s ix -> (ix, ix)
boundsOfMutableByteArray (MutableByteArray l u _) = (l,u)

\end{code}

\begin{code}
thawByteArray :: Ix ix => ByteArray ix -> ST s (MutableByteArray s ix)
thawByteArray (ByteArray l u barr#) =
     {- 
        The implementation is made more complex by the
        fact that the indexes are in units of whatever
        base types that's stored in the byte array.
     -}
   case (sizeofByteArray# barr#) of 
     i# -> do
       marr <- newCharArray (0,I# i#)
       mapM_ (\ idx@(I# idx#) -> 
                 writeCharArray marr idx (C# (indexCharArray# barr# idx#)))
  	     [0..]
       let (MutableByteArray _ _ arr#) = marr
       return (MutableByteArray l u arr#) 

{-
  in-place conversion of immutable arrays to mutable ones places
  a proof obligation on the user: no other parts of your code can
  have a reference to the array at the point where you unsafely
  thaw it (and, subsequently mutate it, I suspect.)
-}
unsafeThawByteArray :: Ix ix => ByteArray ix -> ST s (MutableByteArray s ix)
unsafeThawByteArray (ByteArray l u barr#)
  = return (MutableByteArray l u (unsafeCoerce# barr#))

\end{code}
