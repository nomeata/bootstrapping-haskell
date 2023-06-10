%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section{Working with C strings}

A collection of lower-level functions to help converting between
C strings and Haskell Strings (packed or otherwise).

A more user-friendly Haskell interface to packed string representation
is the PackedString interface.

\begin{code}
module CString 
	(
   	  unpackCString       -- :: Addr -> [Char]
	, unpackCStringIO     -- :: Addr -> IO [Char]	
	, unpackCStringLenIO  -- :: Addr -> Int -> IO String
	, unpackCStringBA     -- :: ByteArray Int -> [Char]
	, unpackCStringBA#    -- :: ByteArray#    -> Int# -> [Char]
	, unpackCString#      -- :: Addr# -> [Char]

  	, unpackNBytes        -- :: Addr -> Int -> [Char]
	, unpackNBytesST      -- :: Addr -> Int -> ST s [Char]
	, unpackNBytesAccST   -- :: Addr -> Int -> [Char] -> ST s [Char]
	, unpackNBytesAccIO   -- :: Addr -> Int -> [Char] -> IO [Char]
	, unpackNBytesBA      -- :: ByteArray Int -> Int  -> [Char]

	, unpackNBytes#       -- :: Addr# -> Int# -> [Char] **
	, unpackNBytesST#     -- :: Addr# -> Int# -> ST s [Char]
	, unpackNBytesBA#     -- :: ByteArray#    -> Int# -> [Char]

	, packString	      -- :: [Char] -> ByteArray Int
	, packStringIO	      -- :: [Char] -> IO (ByteArray Int)
	, packStringST	      -- :: [Char] -> ST s (ByteArray Int)
	, packNBytesST	      -- :: Int -> [Char] -> ByteArray Int
	, packCString#	      -- :: [Char] -> ByteArray#


	  -- unmarshaling (char*) vectors.
	, unvectorize         -- :: Addr -> Int -> IO [String]
	, vectorize	      -- :: [[Char]] -> IO (ByteArray Int)


	, allocChars         -- :: Int -> IO (MutableByteArray RealWorld Int)
	, allocWords         -- :: Int -> IO (MutableByteArray RealWorld Int)
	, freeze	     -- :: MutableByteArray RealWorld Int -> IO (ByteArray Int)
	, strcpy	     -- :: Addr -> IO String

	) where

import PrelPack
import GlaExts
import Addr		( Addr, nullAddr, readCharOffAddr, indexAddrOffAddr )
import PrelIOBase	( IO(..) )
import MutableArray

\end{code}

\begin{code}
packStringIO :: [Char] -> IO (ByteArray Int)
packStringIO str = stToIO (packStringST str)
\end{code}

NOTE: unpackCStringIO must traverse the entire string before
returning, since it is often used on dynamically allocated strings
which need to be deallocated after unpacking.

\begin{code}
unpackCStringIO :: Addr -> IO String
unpackCStringIO addr
 | addr == nullAddr = return "(null)"
 | otherwise        = unpack 0#
  where
    unpack nh = do
       ch <- readCharOffAddr addr (I# nh)
       if ch == '\0'
        then return []
        else do
         ls <- unpack (nh +# 1#)
         return (ch : ls)

-- unpack 'len' chars
unpackCStringLenIO :: Addr -> Int -> IO String
unpackCStringLenIO addr l@(I# len#)
 | len# <# 0#  = ioError (userError ("CString.unpackCStringLenIO: negative length (" ++ show l ++ ")"))
 | len# ==# 0# = return ""
 | otherwise   = unpack [] (len# -# 1#)
  where
    unpack acc 0# = do
       ch <- readCharOffAddr addr (I# 0#)
       return (ch:acc)
    unpack acc nh = do
       ch <- readCharOffAddr addr (I# nh)
       unpack (ch:acc) (nh -# 1#)

unpackNBytesAccIO :: Addr -> Int -> [Char] -> IO [Char]
unpackNBytesAccIO ptr len acc = stToIO (unpackNBytesAccST ptr len acc)
\end{code}

Turn a NULL-terminated vector of null-terminated strings into a string list
(ToDo: create a module of common marshaling functions)

\begin{code}
unvectorize :: Addr -> Int -> IO [String]
unvectorize ptr n
  | str == nullAddr = return []
  | otherwise       = do
	x  <- unpackCStringIO str
	xs <- unvectorize ptr (n+1)
	return (x : xs)
  where
   str = indexAddrOffAddr ptr n

\end{code}

 Turn a string list into a NULL-terminated vector of null-terminated
strings No indices...I hate indices.  Death to Ix.

\begin{code}
vectorize :: [String] -> IO (ByteArray Int)
vectorize vs = do
  arr <- allocWords (len + 1)
  fill arr 0 vs
  freeze arr
 where
    len :: Int
    len = length vs

    fill :: MutableByteArray RealWorld Int -> Int -> [String] -> IO ()
    fill arr n [] =
	_casm_ ``((PP_)%0)[%1] = NULL;'' arr n
    fill arr n (x:xs) = do
	barr <- packStringIO x
        _casm_ ``((PP_)%0)[%1] = (P_)%2;'' arr n barr
	fill arr (n+1) xs

\end{code}

Allocating chunks of memory in the Haskell heap, leaving
out the bounds - use with care.

\begin{code}
-- Allocate a mutable array of characters with no indices.
allocChars :: Int -> IO (MutableByteArray RealWorld Int)
allocChars size = stToIO (newCharArray (0,size))

allocWords :: Int -> IO (MutableByteArray RealWorld Int)
allocWords size = stToIO (newIntArray (0,size))

-- Freeze these index-free mutable arrays
freeze :: MutableByteArray RealWorld Int -> IO (ByteArray Int)
freeze mb = stToIO (unsafeFreezeByteArray mb)

-- Copy a null-terminated string from outside the heap to
-- Haskellized nonsense inside the heap
strcpy :: Addr -> IO String
strcpy str = unpackCStringIO str

\end{code}
