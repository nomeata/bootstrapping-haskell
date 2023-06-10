\section{SST: the strict state transformer monad}
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
module SST(
	SST, SST_R, FSST, FSST_R,

	runSST, sstToST, stToSST, ioToSST,
	thenSST, thenSST_, returnSST, fixSST,
	thenFSST, thenFSST_, returnFSST, failFSST,
	recoverFSST, recoverSST, fixFSST,
	unsafeInterleaveSST, 

	newMutVarSST, readMutVarSST, writeMutVarSST,
	SSTRef
  ) where

#include "HsVersions.h"

import GlaExts
import ST

#if __GLASGOW_HASKELL__ < 301
import STBase		( ST(..), STret(..), StateAndPtr#(..) )
import ArrBase		( StateAndMutableArray#(..) )
import IOBase		( IO(..), IOResult(..) )
#else
import PrelST		( ST(..), STret(..), StateAndPtr#(..) )
import PrelArr		( StateAndMutableArray#(..) )
import PrelIOBase	( IO(..), IOResult(..) )
#endif

\end{code}

@SST@ is very like the standard @ST@ monad, but it comes with its
friend @FSST@.  Because we want the monadic bind operator to work
for mixtures of @SST@ and @FSST@, we can't use @ST@ at all.

For simplicity we don't even dress them up in newtypes.

%************************************************************************
%*									*
\subsection{The data types}
%*									*
%************************************************************************

\begin{code}
type SST  s r     = State# s -> SST_R s r
type FSST s r err = State# s -> FSST_R s r err

data SST_R s r = SST_R r (State# s)

data FSST_R s r err
  = FSST_R_OK   r   (State# s)
  | FSST_R_Fail err (State# s)
\end{code}

Converting to/from ST

\begin{code}
sstToST :: SST s r -> ST s r
stToSST :: ST s r -> SST s r

sstToST sst = ST (\ s -> case sst s of SST_R r s' -> STret s' r)

stToSST (ST st) = \ s -> case st s of STret s' r -> SST_R r s'
\end{code}

...and IO

\begin{code}
ioToSST :: IO a -> SST RealWorld (Either IOError a)
ioToSST (IO io)
  = \s -> case io s of
	    IOok   s' r   -> SST_R (Right r) s'
	    IOfail s' err -> SST_R (Left err) s'
\end{code}

%************************************************************************
%*									*
\subsection{The @SST@ operations}
%*									*
%************************************************************************

\begin{code}
-- Type of runSST should be builtin ...
-- runSST :: forall r. (forall s. SST s r) -> r

runSST :: SST RealWorld r  -> r
runSST m = case m realWorld# of SST_R r s -> r

unsafeInterleaveSST :: SST s r -> SST s r
unsafeInterleaveSST m s = SST_R r s		-- Duplicates the state!
			where
			  SST_R r _ = m s

returnSST :: r -> SST s r
fixSST    :: (r -> SST s r) -> SST s r
{-# INLINE returnSST #-}
{-# INLINE thenSST #-}
{-# INLINE thenSST_ #-}

returnSST r s = SST_R r s

fixSST m s = result
	   where
	     result 	  = m loop s
	     SST_R loop _ = result
\end{code}

OK, here comes the clever bind operator.

\begin{code}
thenSST   :: SST s r -> (r -> State# s -> b) -> State# s -> b
thenSST_  :: SST s r -> (State# s -> b) -> State# s -> b
-- Hence:
--	thenSST :: SST s r -> (r -> SST  s r')     -> SST  s r'
-- and  thenSST :: SST s r -> (r -> FSST s r' err) -> FSST s r' err

-- Hence:
--	thenSST_ :: SST s r -> SST  s r'     -> SST  s r'
-- and  thenSST_ :: SST s r -> FSST s r' err -> FSST s r' err

thenSST  m k s = case m s of { SST_R r s' -> k r s' }

thenSST_ m k s = case m s of { SST_R r s' -> k s' }
\end{code}


%************************************************************************
%*									*
\subsection{FSST: the failable strict state transformer monad}
%*									*
%************************************************************************

\begin{code}
failFSST    :: err -> FSST s r err
fixFSST     :: (r -> FSST s r err) -> FSST s r err
recoverFSST :: (err -> FSST s r err) -> FSST s r err -> FSST s r err
recoverSST  :: (err -> SST s r) -> FSST s r err -> SST s r
returnFSST  :: r -> FSST s r err
thenFSST    :: FSST s r err -> (r -> FSST s r' err) -> FSST s r' err
thenFSST_   :: FSST s r err -> FSST s r' err -> FSST s r' err
{-# INLINE failFSST #-}
{-# INLINE returnFSST #-}
{-# INLINE thenFSST #-}
{-# INLINE thenFSST_ #-}

thenFSST m k s = case m s of
		   FSST_R_OK r s'     -> k r s'
		   FSST_R_Fail err s' -> FSST_R_Fail err s'

thenFSST_ m k s = case m s of
		    FSST_R_OK r s'     -> k s'
		    FSST_R_Fail err s' -> FSST_R_Fail err s'

returnFSST r s = FSST_R_OK r s

failFSST err s = FSST_R_Fail err s

recoverFSST recovery_fn m s
  = case m s of 
	FSST_R_OK r s'     -> FSST_R_OK r s'
	FSST_R_Fail err s' -> recovery_fn err s'

recoverSST recovery_fn m s
  = case m s of 
	FSST_R_OK r s'     -> SST_R r s'
	FSST_R_Fail err s' -> recovery_fn err s'

fixFSST m s = result
	    where
	      result 	       = m loop s
	      FSST_R_OK loop _ = result
\end{code}

%************************************************************************
%*									*
\subsection{Mutables}
%*									*
%************************************************************************

Here we implement mutable variables.  ToDo: get rid of the array impl.

\begin{code}
type SSTRef s a = MutableArray s Int a

newMutVarSST   :: a -> SST s (SSTRef s a)
readMutVarSST  :: SSTRef s a -> SST s a
writeMutVarSST :: SSTRef s a -> a -> SST s ()

newMutVarSST init s#
  = case (newArray# 1# init s#)     of { StateAndMutableArray# s2# arr# ->
    SST_R (MutableArray vAR_IXS arr#) s2# }
  where
    vAR_IXS = error "Shouldn't access `bounds' of a MutableVar\n"

readMutVarSST (MutableArray _ var#) s#
  = case readArray# var# 0# s#	of { StateAndPtr# s2# r ->
    SST_R r s2# }

writeMutVarSST (MutableArray _ var#) val s#
  = case writeArray# var# 0# val s# of { s2# ->
    SST_R () s2# }
\end{code}

