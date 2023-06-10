%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[module_ST]{The State Transformer Monad, @ST@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module ST 
      (
	ST                  -- abstract, instance of Functor, Monad.
      , runST		    -- :: (forall s. ST s a) -> a
      , fixST		    -- :: (a -> ST s a) -> ST s a
      , unsafeInterleaveST  -- :: ST s a -> ST s a

      , STRef
      , newSTRef
      , readSTRef
      , writeSTRef
      
      , unsafeIOToST
      , stToIO
      , STArray
      , newSTArray
      , readSTArray
      , writeSTArray
      , boundsSTArray
      , thawSTArray
      , freezeSTArray
      , unsafeFreezeSTArray
#ifndef __HUGS__
-- no 'good' reason, just doesn't support it right now.
      , unsafeThawSTArray
#endif
      ) where

#ifndef __HUGS__
import PrelArr
import PrelST
import PrelBase	( Eq(..), Int, Bool, ($), ()(..), unsafeCoerce# )
import PrelIOBase ( IO(..), stToIO )
#else
import PrelPrim ( PrimMutableArray
		, PrimArray
		, ST
		, stToIO
		, ioToST
		, STRef
		, newSTRef
		, readSTRef
		, writeSTRef
		, unsafeInterleaveST
		, unsafePerformIO
		, runST
		, unST
		, primNewArray
		, primReadArray
		, primWriteArray
		, primUnsafeFreezeArray
		, primSizeMutableArray
		, primSizeArray
		, primIndexArray
		)
#endif
import Monad
import Ix
import Array
\end{code}

%*********************************************************
%*							*
\subsection{fixST}
%*							*
%*********************************************************

\begin{code}
#ifdef __HUGS__
fixST :: (a -> ST s a) -> ST s a
fixST m = ST (\ s -> 
		let 
		   (r,s) = unST (m r) s
		in
		   (r,s))

#else
fixST :: (a -> ST s a) -> ST s a
fixST k = ST $ \ s ->
    let ans       = liftST (k r) s
	STret _ r = ans
    in
    case ans of STret s' x -> (# s', x #)
#endif
\end{code}

%*********************************************************
%*							*
\subsection{Arrays}
%*							*
%*********************************************************

\begin{code}
#ifdef __HUGS__
data STArray s ix elt = STArray (ix,ix) (PrimMutableArray s elt)
-- Not for Hugs (yet)
--  deriving Eq

newSTArray ixs elt = do
  { arr <- primNewArray (rangeSize ixs) elt
  ; return (STArray ixs arr)
  }

boundsSTArray (STArray ixs arr)        = ixs
readSTArray   (STArray ixs arr) ix     = primReadArray arr (index ixs ix)
writeSTArray  (STArray ixs arr) ix elt = primWriteArray arr (index ixs ix) elt
freezeSTArray (STArray ixs arr)        = do
  { arr' <- primFreezeArray arr
  ; return (Array ixs arr')
  }

unsafeFreezeSTArray (STArray ixs arr)  = do 
  { arr' <- primUnsafeFreezeArray arr
  ; return (Array ixs arr')
  }

thawSTArray (Array ixs arr) = do
  { arr' <- primThawArray arr
  ; return (STArray ixs arr')
  }

primFreezeArray :: PrimMutableArray s a -> ST s (PrimArray a)
primFreezeArray arr = do
  { let n = primSizeMutableArray arr
  ; arr' <- primNewArray n arrEleBottom
  ; mapM_ (copy arr arr') [0..n-1]
  ; primUnsafeFreezeArray arr'
  }
 where
  copy arr arr' i = do { x <- primReadArray arr i; primWriteArray arr' i x }
  arrEleBottom = error "primFreezeArray: panic"

primThawArray :: PrimArray a -> ST s (PrimMutableArray s a)
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


\begin{code}
#ifdef __HUGS__
unsafeIOToST        :: IO a -> ST s a
unsafeIOToST         = return . unsafePerformIO
#else
unsafeIOToST        :: IO a -> ST s a
unsafeIOToST (IO io) = ST $ \ s ->
    case ((unsafeCoerce# io) s) of
      (#  new_s, a #) -> unsafeCoerce# (STret new_s a)
--      IOfail new_s e -> error ("I/O Error (unsafeIOToST): " ++ showsPrec 0 e "\n")
#endif
\end{code}
