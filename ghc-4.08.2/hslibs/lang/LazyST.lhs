%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1997
%

\section[LazyST]{The Lazy State Transformer Monad, @LazyST@}

This module presents an identical interface to ST, but the underlying
implementation of the state thread is lazy.

\begin{code}
module LazyST (
	ST,

	runST,
	unsafeInterleaveST,

	ST.STRef,
	newSTRef, readSTRef, writeSTRef,

	STArray,
	newSTArray, readSTArray, writeSTArray, boundsSTArray, 
	thawSTArray, freezeSTArray, unsafeFreezeSTArray, 
#ifndef __HUGS__
-- no 'good' reason, just doesn't support it right now.
        unsafeThawSTArray,
#endif

	ST.unsafeIOToST, ST.stToIO,

	strictToLazyST, lazyToStrictST
    ) where

#ifndef __HUGS__
import qualified ST
import qualified PrelST
import PrelArr  ( Array (..), STArray(..) )
import PrelBase	( Eq(..), Int, Bool, ($), ()(..) )
import Monad
import Ix
import PrelGHC
#else
import qualified ST
import Monad
import Ix
import Array
import PrelPrim ( unST 
		 , mkST 
		 , PrimMutableArray
		 , PrimArray
		 , primNewArray
		 , primReadArray
		 , primWriteArray
		 , primUnsafeFreezeArray
		 , primSizeMutableArray
		 , primSizeArray
		 , primIndexArray
		 )
#endif


#ifndef __HUGS__
newtype ST s a = ST (State s -> (a, State s))
data State s = S# (State# s)
#else
newtype ST s a = ST (s -> (a,s))
#endif

instance Functor (ST s) where
    fmap f m = ST $ \ s ->
      let 
       ST m_a = m
       (r,new_s) = m_a s
      in
      (f r,new_s)

instance Monad (ST s) where

        return a = ST $ \ s -> (a,s)
        m >> k   =  m >>= \ _ -> k
	fail s   = error s

        (ST m) >>= k
         = ST $ \ s ->
           let
             (r,new_s) = m s
             ST k_a = k r
           in
           k_a new_s


#ifndef __HUGS__
{-# NOINLINE runST #-}
runST :: (forall s. ST s a) -> a
runST st = case st of ST the_st -> let (r,_) = the_st (S# realWorld#) in r
#else
runST :: (__forall s. ST s a) -> a
runST st = case st of ST the_st -> let (r,_) = the_st realWorld in r
	where realWorld = error "runST: entered the RealWorld"
#endif
\end{code}

%*********************************************************
%*							*
\subsection{Variables}
%*							*
%*********************************************************

\begin{code}
newSTRef   :: a -> ST s (ST.STRef s a)
readSTRef  :: ST.STRef s a -> ST s a
writeSTRef :: ST.STRef s a -> a -> ST s ()

newSTRef   = strictToLazyST . ST.newSTRef
readSTRef  = strictToLazyST . ST.readSTRef
writeSTRef r a = strictToLazyST (ST.writeSTRef r a)

\end{code}

%*********************************************************
%*							*
\subsection{Arrays}
%*							*
%*********************************************************

\begin{code}
#ifdef __HUGS__
data STArray s ix elt = STArray (ix,ix) (PrimMutableArray s elt)
#endif

newSTArray 	    :: Ix ix => (ix,ix) -> elt -> ST s (STArray s ix elt)
readSTArray   	    :: Ix ix => STArray s ix elt -> ix -> ST s elt 
writeSTArray	    :: Ix ix => STArray s ix elt -> ix -> elt -> ST s () 
boundsSTArray       :: Ix ix => STArray s ix elt -> (ix, ix)  
thawSTArray 	    :: Ix ix => Array ix elt -> ST s (STArray s ix elt)
freezeSTArray	    :: Ix ix => STArray s ix elt -> ST s (Array ix elt)
unsafeFreezeSTArray :: Ix ix => STArray s ix elt -> ST s (Array ix elt)

#ifndef __HUGS__

newSTArray ixs init   	= strictToLazyST (ST.newSTArray ixs init)

readSTArray arr ix      = strictToLazyST (ST.readSTArray arr ix)
writeSTArray arr ix v   = strictToLazyST (ST.writeSTArray arr ix v)
boundsSTArray arr       = ST.boundsSTArray arr
thawSTArray arr	        = strictToLazyST (ST.thawSTArray arr)
freezeSTArray arr       = strictToLazyST (ST.freezeSTArray arr)
unsafeFreezeSTArray arr = strictToLazyST (ST.unsafeFreezeSTArray arr)
unsafeThawSTArray arr   = strictToLazyST (ST.unsafeThawSTArray arr)

#else

newSTArray ixs elt = do
  { arr <- strictToLazyST (primNewArray (rangeSize ixs) elt)
  ; return (STArray ixs arr)
  }

boundsSTArray (STArray ixs arr)        = ixs
readSTArray   (STArray ixs arr) ix     
	= strictToLazyST (primReadArray arr (index ixs ix))
writeSTArray  (STArray ixs arr) ix elt 
	= strictToLazyST (primWriteArray arr (index ixs ix) elt)
freezeSTArray (STArray ixs arr)        = do
  { arr' <- strictToLazyST (primFreezeArray arr)
  ; return (Array ixs arr')
  }

unsafeFreezeSTArray (STArray ixs arr)  = do 
  { arr' <- strictToLazyST (primUnsafeFreezeArray arr)
  ; return (Array ixs arr')
  }

thawSTArray (Array ixs arr) = do
  { arr' <- strictToLazyST (primThawArray arr)
  ; return (STArray ixs arr')
  }

primFreezeArray :: PrimMutableArray s a -> ST.ST s (PrimArray a)
primFreezeArray arr = do
  { let n = primSizeMutableArray arr
  ; arr' <- primNewArray n arrEleBottom
  ; mapM_ (copy arr arr') [0..n-1]
  ; primUnsafeFreezeArray arr'
  }
 where
  copy arr arr' i = do { x <- primReadArray arr i; primWriteArray arr' i x }
  arrEleBottom = error "primFreezeArray: panic"

primThawArray :: PrimArray a -> ST.ST s (PrimMutableArray s a)
primThawArray arr = do
  { let n = primSizeArray arr
  ; arr' <- primNewArray n arrEleBottom
  ; mapM_ (copy arr arr') [0..n-1]
  ; return arr'
  }
 where
  copy arr arr' i = primWriteArray arr' i (primIndexArray arr i)
  arrEleBottom = error "primFreezeArray: panic"
#endif
\end{code}

%*********************************************************
%*							*
\subsection{Strict <--> Lazy}
%*							*
%*********************************************************

\begin{code}
#ifndef __HUGS__
strictToLazyST :: ST.ST s a -> ST s a
strictToLazyST m = ST $ \s ->
        let 
  	   pr = case s of { S# s# -> PrelST.liftST m s# }
	   r  = case pr of { PrelST.STret _ v -> v }
	   s' = case pr of { PrelST.STret s2# _ -> S# s2# }
	in
	(r, s')

lazyToStrictST :: ST s a -> ST.ST s a
lazyToStrictST (ST m) = PrelST.ST $ \s ->
        case (m (S# s)) of (a, S# s') -> (# s', a #)

#else

strictToLazyST :: ST.ST s a -> ST s a
strictToLazyST m = ST $ \s ->
        let 
  	   pr = unST m s
	   r  = fst pr
	   s' = snd pr
	in
	(r, s')


lazyToStrictST :: ST s a -> ST.ST s a
lazyToStrictST (ST m) = mkST $ m
#endif

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST = strictToLazyST . ST.unsafeInterleaveST . lazyToStrictST


\end{code}

