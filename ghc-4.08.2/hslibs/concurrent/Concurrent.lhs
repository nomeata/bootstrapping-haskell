%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Concurrent]{Concurrent Haskell constructs}

A common interface to a collection of useful concurrency abstractions.
Currently, the collection only contains the abstractions found in the
{\em Concurrent Haskell} paper (presented at the Haskell Workshop
1995, draft available via \tr{ftp} from
\tr{ftp.dcs.gla.ac.uk/pub/glasgow-fp/drafts}.)  plus a couple of
others. See the paper and the individual files containing the module
definitions for explanation on what they do.

\begin{code}
module Concurrent
	( module Chan
	, module CVar
	, module MVar
	, module QSem
	, module QSemN
	, module SampleVar

        , ThreadId

	-- Forking and suchlike
	, forkIO	-- :: IO () -> IO ThreadId
	, myThreadId 	-- :: IO ThreadId
	, killThread	-- :: ThreadId -> IO ()
	, raiseInThread -- :: ThreadId -> Exception -> IO ()
	, par  		-- :: a -> b -> b
	, seq  		-- :: a -> b -> b
	, fork  	-- :: a -> b -> b
	, yield         -- :: IO ()

	, threadDelay		-- :: Int -> IO ()
	, threadWaitRead	-- :: Int -> IO ()
	, threadWaitWrite	-- :: Int -> IO ()

	 -- merging of streams
	, mergeIO	-- :: [a]   -> [a] -> IO [a]
	, nmergeIO	-- :: [[a]] -> IO [a]
    ) where

#ifndef __HUGS__
import PrelConc
import PrelHandle       ( topHandler )
import PrelException
import PrelIOBase	( IO(..) )
import PrelIOBase	( unsafePerformIO , unsafeInterleaveIO )
import PrelBase		( fork# )
import PrelGHC		( Addr#, unsafeCoerce# )
#else
import PrelPrim ( unsafeInterleaveIO
		, unsafePerformIO
		, ThreadId
		, Exception
		, forkIO
		, primYield
		, primDelay
		, primWaitRead
		, primWaitWrite
		, primGetThreadId
		, primKillThread
		, primRaiseInThread
	        )
#endif

import MVar
import CVar
import Chan
import QSem
import QSemN
import SampleVar
import Parallel

infixr 0 `fork`
\end{code}

Thread Ids, specifically the instances of Eq and Ord for these things.
The ThreadId type itself is defined in std/PrelConc.lhs.

Rather than define a new primitve, we use a little helper function
cmp_thread in the RTS.

\begin{code}
#ifndef __HUGS__
foreign import ccall "cmp_thread" unsafe cmp_thread :: Addr# -> Addr# -> Int
-- Returns -1, 0, 1

cmpThread :: ThreadId -> ThreadId -> Ordering
cmpThread (ThreadId t1) (ThreadId t2) = 
   case cmp_thread (unsafeCoerce# t1) (unsafeCoerce# t2) of
      -1 -> LT
      0  -> EQ
      _  -> GT -- must be 1

instance Eq ThreadId where
   t1 == t2 = 
      case t1 `cmpThread` t2 of
         EQ -> True
         _  -> False

instance Ord ThreadId where
   compare = cmpThread
#else
-- already defined in Hugs Prelude
#endif
\end{code}

\begin{code}
#ifndef __HUGS__
forkIO :: IO () -> IO ThreadId
forkIO action = IO $ \ s -> 
   case (fork# action_plus s) of (# s1, id #) -> (# s1, ThreadId id #)
 where
  action_plus = 
    catchException action 
		   (topHandler False{-don't quit on exception raised-})
#else
-- forkIO in prelude
#endif

{-# INLINE fork #-}
fork :: a -> b -> b
fork x y = unsafePerformIO (forkIO (x `seq` return ())) `seq` y
\end{code}


\begin{code}
max_buff_size :: Int
max_buff_size = 1

mergeIO :: [a] -> [a] -> IO [a]
nmergeIO :: [[a]] -> IO [a]

mergeIO ls rs
 = newEmptyMVar		       >>= \ tail_node ->
   newMVar tail_node	       >>= \ tail_list ->
   newQSem max_buff_size       >>= \ e ->
   newMVar 2                   >>= \ branches_running ->
   let
    buff = (tail_list,e)
   in
    forkIO (suckIO branches_running buff ls) >>
    forkIO (suckIO branches_running buff rs) >>
    takeMVar tail_node	>>= \ val ->
    signalQSem e 	>>
    return val

type Buffer a 
 = (MVar (MVar [a]), QSem)

suckIO :: MVar Int -> Buffer a -> [a] -> IO ()

suckIO branches_running buff@(tail_list,e) vs
 = case vs of
	[] -> takeMVar branches_running >>= \ val ->
	      if val == 1 then
		 takeMVar tail_list     >>= \ node ->
		 putMVar node []        >>
		 putMVar tail_list node
	      else 	
  		 putMVar branches_running (val-1)
	(x:xs) ->
		waitQSem e 	   		 >>
		takeMVar tail_list 		 >>= \ node ->
	        newEmptyMVar 	   		 >>= \ next_node ->
		unsafeInterleaveIO (
			takeMVar next_node  >>= \ y ->
			signalQSem e	    >>
			return y)	         >>= \ next_node_val ->
		putMVar node (x:next_node_val)   >>
		putMVar tail_list next_node 	 >>
		suckIO branches_running buff xs

nmergeIO lss
 = let
    len = length lss
   in
    newEmptyMVar	  >>= \ tail_node ->
    newMVar tail_node	  >>= \ tail_list ->
    newQSem max_buff_size >>= \ e ->
    newMVar len		  >>= \ branches_running ->
    let
     buff = (tail_list,e)
    in
    mapIO (\ x -> forkIO (suckIO branches_running buff x)) lss >>
    takeMVar tail_node	>>= \ val ->
    signalQSem e 	>>
    return val
  where
    mapIO f xs = sequence (map f xs)
\end{code}
\begin{code}
#ifdef __HUGS__
threadDelay	:: Int -> IO ()
threadWaitRead	:: Int -> IO ()
threadWaitWrite	:: Int -> IO ()
yield         :: IO ()
myThreadId    :: IO ThreadId
killThread    :: ThreadId -> IO ()
raiseInThread :: ThreadId -> Exception -> IO ()


threadDelay	n = do { primDelay n ; return () }
threadWaitRead	n = do { primWaitRead n ; return () }
threadWaitWrite	n = do { primWaitWrite n ; return () }
yield             = do { primYield ; return () }
myThreadId        = primGetThreadId
killThread        = primKillThread
raiseInThread     = primRaiseInThread
#endif
\end{code}
