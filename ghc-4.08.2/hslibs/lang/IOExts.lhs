%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[IOExts]{Module @IOExts@}

@IOExts@ provides useful functionality that fall outside the
standard Haskell IO interface. Expect the contents of IOExts
to be the same for Hugs and GHC (same goes for any other
Hugs/GHC extension libraries, unless a function/type is
explicitly flagged as being implementation specific
extension.)

\begin{code}
{-# OPTIONS -#include "stgio.h" #-}

module IOExts 
        ( fixIO 	      -- :: (a -> IO a) -> IO a
        , unsafePerformIO     -- :: IO a -> a
        , unsafeInterleaveIO  -- :: IO a -> IO a

        , IORef		      -- instance of: Eq
        , newIORef	      -- :: a -> IO (IORef a)
        , readIORef	      -- :: IORef a -> IO a
        , writeIORef	      -- :: IORef a -> a -> IO ()
	, updateIORef	      -- :: IORef a -> (a -> a) -> IO ()

	, IOArray	      -- instance of: Eq
	, newIOArray	      -- :: Ix ix => (ix,ix) -> elt -> IO (IOArray ix elt)
	, boundsIOArray       -- :: Ix ix => IOArray ix elt -> (ix, ix)
	, readIOArray         -- :: Ix ix => IOArray ix elt -> ix -> IO elt
	, writeIOArray        -- :: Ix ix => IOArray ix elt -> ix -> elt -> IO ()
	, freezeIOArray       -- :: Ix ix => IOArray ix elt -> IO (Array ix elt)
	, thawIOArray	      -- :: Ix ix => Array ix elt -> IO (IOArray ix elt)

#ifndef __HUGS__
	, unsafeFreezeIOArray -- :: Ix ix => IOArray ix elt -> IO (Array ix elt)
	, unsafeThawIOArray   -- :: Ix ix => Array ix elt -> IO (IOArray ix elt)
#endif
	
        , trace		      -- :: String -> a -> a

#ifndef __HUGS__
	, IOModeEx(..)	      	-- instance (Eq, Read, Show)
	, openFileEx	      	-- :: FilePath -> IOModeEx -> IO Handle
	, hSetBinaryMode      	-- :: Handle -> Bool -> IO Bool
			      	
	, hGetBuf            	-- :: Handle -> Addr -> Int -> IO Int
	, hGetBufFull        	-- :: Handle -> Addr -> Int -> IO Int
	, hGetBufBA  	      	-- :: Handle -> MutableByteArray RealWorld a 
		      	      	--	-> Int -> IO Int
	, hGetBufBAFull      	-- :: Handle -> MutableByteArray RealWorld a 
		              	--	-> Int -> IO Int

	, hPutBuf     	      	-- :: Handle -> Addr -> Int -> IO Int
	, hPutBufFull         	-- :: Handle -> Addr -> Int -> IO ()
	, hPutBufBA   	      	-- :: Handle -> MutableByteArray RealWorld a
		      	      	--	-> Int -> IO Int
	, hPutBufBAFull       	-- :: Handle -> MutableByteArray RealWorld a
		              	--	-> Int -> IO ()

	, hIsTerminalDevice 	-- :: Handle -> IO Bool
        , hSetEcho		-- :: Handle -> Bool -> IO ()
	, hGetEcho		-- :: Handle -> IO Bool

	, withHandleFor		-- :: Handle -> Handle -> IO a -> IO a
	, withStdout		-- :: IO a -> IO a
	, withStdin		-- :: IO a -> IO a
	, withStderr		-- :: IO a -> IO a

#ifndef __PARALLEL_HASKELL__
	, mkWeakIORef           -- :: IORef a -> IO () -> IO (Weak (IORef a))
#endif
	, unsafePtrEq		-- :: a -> a -> Bool
	, slurpFile
	, hConnectTo
        , performGC
	, freeHaskellFunctionPtr
#endif

#ifndef __HUGS__
	, HandlePosition
	, HandlePosn(..)
	, hTell                -- :: Handle -> IO HandlePosition
#endif	
        ) where

\end{code}

\begin{code}
#ifndef __HUGS__
import PrelBase
import PrelIOBase
import ST
import IO
import PrelHandle ( openFileEx, IOModeEx(..),
		    hSetEcho, hGetEcho, getHandleFd, slurpFile
		  )
import MutableArray
import PrelST
import PrelArr
#if !defined(__PARALLEL_HASKELL__)
import PrelWeak
#endif
import PrelGHC
import PrelHandle
import PrelIOBase
import IO 	( hPutStr, hPutChar )
import PrelAddr ( Addr )
#else 
import PrelPrim ( RealWorld
			  , IORef
			  , newIORef
			  , writeIORef
			  , readIORef
			  , unsafeInterleaveIO
			  , unsafePerformIO
			  , ioToST
			  , stToIO
			  , primReallyUnsafePtrEquality
			  )

import ST
import IO
import Array
#endif /* ! __HUGS__ */

#ifndef __PARALLEL_HASKELL__
#ifndef __HUGS__
#define FILE_OBJECT	    ForeignObj
#else
#define FILE_OBJECT	    Addr
#endif
#else
#define FILE_OBJECT	    Addr
#endif

import Ix

fixIO 		:: (a -> IO a) -> IO a
fixIO m         = stToIO (fixST (ioToST . m))


unsafePtrEq :: a -> a -> Bool

#ifdef __HUGS__
unsafePtrEq = primReallyUnsafePtrEquality
#else
unsafePtrEq a b =
    case reallyUnsafePtrEquality# a b of
	 0# -> False
	 _  -> True 
#endif
\end{code}

%*********************************************************
%*							*
\subsection{IO Refs}
%*							*
%*********************************************************

\begin{code}
#ifdef __HUGS__
-- IORef, newIORef, readIORef, writeIORef can be found
-- in the prelude
#else
newIORef    :: a -> IO (IORef a)
readIORef   :: IORef a -> IO a
writeIORef  :: IORef a -> a -> IO ()

newtype IORef a = IORef (STRef RealWorld a) 
    deriving Eq

newIORef v = stToIO (newSTRef v) >>= \ var -> return (IORef var)
readIORef  (IORef var) = stToIO (readSTRef var)
writeIORef (IORef var) v = stToIO (writeSTRef var v)
#endif

updateIORef :: IORef a -> (a -> a) -> IO ()
updateIORef ref f = do
  x <- readIORef ref
  let x' = f x
  writeIORef ref x'
  -- or should we return new value ? (or old?)

#if !defined(__PARALLEL_HASKELL__) && !defined(__HUGS__)
mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef r@(IORef (STRef r#)) f = IO $ \s ->
  case mkWeak# r# r f s of (# s1, w #) -> (# s1, Weak w #)
#endif
\end{code}

%*********************************************************
%*							*
\subsection{IO Arrays}
%*							*
%*********************************************************

\begin{code}
newIOArray          :: Ix ix => (ix,ix) -> elt -> IO (IOArray ix elt)
boundsIOArray       :: Ix ix => IOArray ix elt -> (ix, ix)
readIOArray         :: Ix ix => IOArray ix elt -> ix -> IO elt
writeIOArray        :: Ix ix => IOArray ix elt -> ix -> elt -> IO ()
freezeIOArray       :: Ix ix => IOArray ix elt -> IO (Array ix elt)
thawIOArray	    :: Ix ix => Array ix elt -> IO (IOArray ix elt)
#ifndef __HUGS__
unsafeFreezeIOArray :: Ix ix => IOArray ix elt -> IO (Array ix elt)
unsafeThawIOArray   :: Ix ix => Array ix elt -> IO (IOArray ix elt)
#endif

#ifdef __HUGS__
type IOArray ix elt = STArray RealWorld ix elt
newIOArray    bnds elt   = stToIO (newSTArray bnds elt)
boundsIOArray arr        = boundsSTArray arr
readIOArray   arr ix     = stToIO (readSTArray arr ix)
writeIOArray  arr ix ele = stToIO (writeSTArray arr ix ele)
freezeIOArray arr        = stToIO (freezeSTArray arr)
thawIOArray   arr        = stToIO (thawSTArray arr)
#else
newtype IOArray ix elt = IOArray (STArray RealWorld ix elt)
    deriving Eq

newIOArray ixs elt = 
    stToIO (newSTArray ixs elt) >>= \arr -> 
    return (IOArray arr)

boundsIOArray (IOArray arr) = boundsSTArray arr

readIOArray (IOArray arr) ix = stToIO (readSTArray arr ix)

writeIOArray (IOArray arr) ix elt = stToIO (writeSTArray arr ix elt)

freezeIOArray (IOArray arr) = stToIO (freezeSTArray arr)

thawIOArray arr = do 
	marr <- stToIO (thawSTArray arr)
	return (IOArray marr)

unsafeFreezeIOArray (IOArray arr) = stToIO (unsafeFreezeSTArray arr)
unsafeThawIOArray   arr = do
        marr <- stToIO (unsafeThawSTArray arr)
	return (IOArray marr)
#endif
\end{code}

%*********************************************************
%*							*
\subsection{Trace}
%*							*
%*********************************************************

\begin{code}
#ifdef __HUGS__
trace, trace_quiet :: String -> a -> a
trace s x
   = trace_quiet ("trace: " ++ s) x
trace_quiet s x
   = (unsafePerformIO (putStr (s ++ "\n"))) `seq` x
#else
{-# NOINLINE trace #-}
trace :: String -> a -> a
trace string expr = unsafePerformIO $ do
    fd <- getHandleFd stderr
    hPutStr stderr string
    hPutChar stderr '\n'
    postTraceHook fd
    return expr

foreign import "PostTraceHook" postTraceHook :: Int -> IO ()
#endif

\end{code}

Not something you want to call normally, but useful
in the cases where you do want to flush stuff out of
the heap or make sure you've got room enough

\begin{code}
#ifndef __HUGS__
foreign import "performGC" performGC :: IO ()
#endif
\end{code}

When using 'foreign export dynamic' to dress up a Haskell
IO action to look like a C function pointer, a little bit
of memory is allocated (along with a stable pointer to
the Haskell IO action). When done with the C function
pointer, you'll need to call @freeHaskellFunctionPtr()@ to
let go of these resources - here's the Haskell wrapper for
that RTS entry point, should you want to free it from
within Haskell.

SUP: This really belongs into module Foreign, but for legacy reasons
we leave it here for now.

\begin{code}
#ifndef __HUGS__
foreign import unsafe freeHaskellFunctionPtr :: Addr -> IO ()
#endif
\end{code}

(Experimental) 

Support for redirecting I/O on a handle to another for the
duration of an IO action. To re-route a handle, it is first
flushed, followed by replacing its innards (i.e., FILE_OBJECT)
with that of the other. This happens before and after the
action is executed.

If the action raises an exception, the handle is replaced back
to its old contents, but without flushing it first - as this
may provoke exceptions. Notice that the action may perform
I/O on either Handle, with the result that the I/O is interleaved.
(Why you would want to do this, is a completely different matter.)

ToDo: probably want to restrict what kind of handles can be
replaced with another - i.e., don't want to be able to replace
a writeable handle with a readable one.

\begin{code}
#ifndef __HUGS__
withHandleFor :: Handle
	      -> Handle
	      -> IO a
	      -> IO a
withHandleFor h1 h2 act = do
   h1_fo <- getFO h1
   plugIn h1_fo
 where
  plugIn h1_fo = do
    hFlush h2
    h2_fo <- withHandle h2 $ \ h2_ -> return (h2_{haFO__=h1_fo}, haFO__ h2_)
    catch (act >>= \ x -> hFlush h2 >> setFO h2 h2_fo >> return x)
    	  (\ err -> setFO h2 h2_fo >> ioError err)

  setFO h fo = 
    withHandle h $ \ h_ -> return (h_{haFO__=fo}, ())

  getFO h = 
    wantRWHandle "withHandleFor" h $ \ h_ ->
    return (haFO__ h_)
#endif        
\end{code}

Derived @withHandleFor@ combinators and, at the moment, these
are exported from @IOExts@ and not @withHandleFor@ itself.

\begin{code}
#ifndef __HUGS__
withStdin  h a = withHandleFor h stdin  a
withStdout h a = withHandleFor h stdout a
withStderr h a = withHandleFor h stderr a
#endif
\end{code}

@hTell@ is the lower-level version of @hGetPosn@ - return the
position, without bundling it together with the handle itself:

\begin{code}
#ifndef __HUGS__
hTell :: Handle -> IO HandlePosition
hTell h = do
  (HandlePosn _ x) <- hGetPosn h
  return x
#endif
\end{code}

@hSetBinaryMode@ lets you change the translation mode for a handle.
On some platforms (e.g., Win32) a distinction is made between being in
'text mode' or 'binary mode', with the former terminating lines
by \r\n rather than just \n.

Debating the Winnitude or otherwise of such a scheme is less than
interesting -- it's there, so we have to cope.

A side-effect of calling @hSetBinaryMode@ is that the output buffer
(if any) is flushed prior to changing the translation mode.

\begin{code}
#if !defined(__HUGS__)
hSetBinaryMode :: Handle -> Bool -> IO Bool
#if defined(__PARALLEL_HASKELL__)
-- ToDo: fix the code; currently type of fo clashes (Addr vs. ForeignObj)
hSetBinaryMode handle is_binary = 
  error "hSetBinaryMode not implemented in parallel Haskell"
#else
hSetBinaryMode handle is_binary = do 
        -- is_binary = True => set translation mode to binary.
    wantRWHandle "hSetBinaryMode" handle $ \ handle_ -> do
    let fo = haFO__ handle_
    rc	    <- setBinaryMode fo flg
    if rc >= 0 then 
       return (int2Bool rc)
     else
       constructErrorAndFail "hSetBinaryMode"
  where
   flg | is_binary = 1
       | otherwise = 0

   int2Bool 0 = False
   int2Bool _ = True
#endif
#endif
\end{code}

-----------------------------------------------------------------------------
Reading sequences of bytes.

Semantics of hGetBuf:

   - hGetBuf reads data into the buffer until either

	(a) the operation would block
	(b) EOF is reached
	(c) the buffer is full
     
     It returns the amount of data actually read.  This may
     be zero in cases (a) and (b).  hGetBuf never raises
     an EOF exception, it always returns zero instead.

     If the handle is a pipe or socket, and the writing end
     is closed, hGetBuf will behave as for condition (a).

   - hGetBufFull behaves as hGetBuf, except that it
     never returns on condition (a), it blocks until either
     (b) or (c) are true.  It can also return zero if
     the EOF has been reached and no data has been read.

Semantics of hPutBuf:

    - hPutBuf writes data from the buffer until either

	  (a) the operation would block
	  (b) the buffer is empty

      It returns the amount of data actually written.  

      If the handle is a pipe or socket, and the reading end is
      closed, hPutBuf will raise a ResourceVanished exception.
      (If this is a POSIX system, and the program has not 
      asked to ignore SIGPIPE, then a SIGPIPE may be delivered
      instead, whose default action is to terminate the program).

    - hPutBufFull behaves as hPutBuf, except that it
      never returns on condition (a), it blocks and continues
      writing until (b) is true.  hPutBufFull returns ().

Known bugs:

    - hGetBufFull/hPutBufFull don't lock the Handle between transfers,
      which means that simultaneous reads/writes from the same handle
      may split the data in the buffer.  Locking the buffer wouldn't
      work too well, because it would lock out any other
      readers/writers altogether (we want to allow a write to a handle
      that has a reader blocked on it, for example).  This is really a
      shortcoming in the I/O system.

\begin{code}
#ifndef __HUGS__
hGetBuf :: Handle -> Addr -> Int -> IO Int
hGetBuf handle buf sz
  | sz <= 0 = illegalBufferSize handle "hGetBuf" sz
  | otherwise = retIfBlock "hGetBuf" handle (\fo -> readChunk fo buf 0 sz)

hGetBufFull :: Handle -> Addr -> Int -> IO Int
hGetBufFull handle buf sz
  | sz <= 0 = illegalBufferSize handle "hGetBufFull" sz
  | otherwise = hGetBuf' sz 0
  where
  hGetBuf' sz len = do
	r <- mayBlockRead "hGetBufFull" handle (\fo -> readChunk fo buf len sz)
    	if r >= sz || r == 0  -- r == 0 indicates EOF
	    then return (len+r)
	    else hGetBuf' (sz-r) (len+r)

hGetBufBA :: Handle -> MutableByteArray RealWorld a -> Int -> IO Int
hGetBufBA handle buf sz
  | sz <= 0 = illegalBufferSize handle "hGetBufBA" sz
  | otherwise = mayBlockRead "hGetBufBA" handle 
			(\fo -> readChunkBA fo buf 0 sz)

hGetBufBAFull :: Handle -> MutableByteArray RealWorld a -> Int -> IO Int
hGetBufBAFull handle buf sz
  | sz <= 0 = illegalBufferSize handle "hGetBufBAFull" sz
  | otherwise = hGetBuf' sz 0
  where
  hGetBuf' sz len = do
	r <- mayBlockRead "hGetBufBA" handle (\fo -> readChunkBA fo buf len sz)
    	if r >= sz || r == 0  -- r == 0 indicates EOF
	    then return (len+r)
	    else hGetBuf' (sz-r) (len+r)

hPutBuf :: Handle -> Addr -> Int -> IO Int
hPutBuf handle buf sz
  | sz <= 0 = illegalBufferSize handle "hPutBuf" sz
  | otherwise = retIfBlock "hPutBuf" handle (\fo -> writeBuf fo buf 0 sz)

hPutBufFull :: Handle -> Addr -> Int -> IO ()
hPutBufFull handle buf sz
  | sz <= 0 = illegalBufferSize handle "hPutBufFull" sz
  | otherwise = hPutBufFull' sz 0
  where
  hPutBufFull' sz len = do
	r <- mayBlockWrite "hPutBufFull" handle (\fo -> writeBuf fo buf len sz)
    	if r >= sz
	    then return ()
	    else hPutBufFull' (sz-r) (len+r) -- <= sz indicates blocking

hPutBufBA :: Handle -> MutableByteArray RealWorld a -> Int -> IO Int
hPutBufBA handle buf sz
  | sz <= 0 = illegalBufferSize handle "hPutBufBA" sz
  | otherwise = retIfBlock "hPutBufBA" handle 
			(\fo -> writeBufBA fo buf 0 sz)

hPutBufBAFull :: Handle -> MutableByteArray RealWorld a -> Int -> IO ()
hPutBufBAFull handle buf sz
  | sz <= 0 = illegalBufferSize handle "hPutBufBAFull" sz
  | otherwise = hPutBuf' sz 0
  where
  hPutBuf' sz len = do
	r <- mayBlockWrite "hPutBufBAFull" handle (\fo -> writeBufBA fo buf len sz)
    	if r >= sz
	    then return ()
	    else hPutBuf' (sz-r) (len+r) -- <= sz indicates blocking

foreign import "libHS_cbits" "readChunk" unsafe
           readChunkBA      :: FILE_OBJECT -> MutableByteArray s a -> Int -> Int -> IO Int{-ret code-}

foreign import "libHS_cbits" "writeBuf" unsafe
           writeBuf         :: FILE_OBJECT -> Addr -> Int -> Int -> IO Int{-ret code-}

foreign import "libHS_cbits" "writeBufBA" unsafe
           writeBufBA       :: FILE_OBJECT -> MutableByteArray s a -> Int -> Int -> IO Int{-ret code-}
#endif
\end{code}

-----------------------------------------------------------------------------
-- Internal Utils

\begin{code}
#ifndef __HUGS__
illegalBufferSize handle fn (sz :: Int) = 
	ioError (IOError (Just handle)
			    InvalidArgument  fn
			    ("illegal buffer size " ++ showsPrec 9 sz []))

retIfBlock :: String -> Handle -> (FILE_OBJECT -> IO Int) -> IO Int
retIfBlock fname handle fn = do
    wantReadableHandle fname handle $ \ handle_ -> do
    let fo = haFO__ handle_
    rc <- fn fo
    case rc of
      -5 -> return 0
      -6 -> return 0
      -7 -> return 0
      _  -> if rc >= 0
   	       then return rc
   	       else constructErrorAndFail fname
#endif
\end{code}
