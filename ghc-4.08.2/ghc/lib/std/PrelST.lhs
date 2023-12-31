%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelST]{The @ST@ monad}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelST where

import PrelShow
import PrelBase
import PrelGHC
import PrelNum ()	-- So that we get the .hi file for system imports

default ()
\end{code}

%*********************************************************
%*							*
\subsection{The @ST@ monad}
%*							*
%*********************************************************

The state-transformer monad proper.  By default the monad is strict;
too many people got bitten by space leaks when it was lazy.

\begin{code}
newtype ST s a = ST (STRep s a)
type STRep s a = State# s -> (# State# s, a #)

instance Functor (ST s) where
    fmap f (ST m) = ST $ \ s ->
      case (m s) of { (# new_s, r #) ->
      (# new_s, f r #) }

instance Monad (ST s) where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    return x = ST $ \ s -> (# s, x #)
    m >> k   =  m >>= \ _ -> k

    (ST m) >>= k
      = ST $ \ s ->
	case (m s) of { (# new_s, r #) ->
	case (k r) of { ST k2 ->
	(k2 new_s) }}

data STret s a = STret (State# s) a

-- liftST is useful when we want a lifted result from an ST computation.  See
-- fixST below.
liftST :: ST s a -> State# s -> STret s a
liftST (ST m) = \s -> case m s of (# s', r #) -> STret s' r

{-# NOINLINE unsafeInterleaveST #-}
unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST (ST m) = ST ( \ s ->
    let
	r = case m s of (# _, res #) -> res
    in
    (# s, r #)
  )

instance  Show (ST s a)  where
    showsPrec _ _  = showString "<<ST action>>"
    showList	   = showList__ (showsPrec 0)
\end{code}

Definition of runST
~~~~~~~~~~~~~~~~~~~

SLPJ 95/04: Why @runST@ must not have an unfolding; consider:
\begin{verbatim}
f x =
  runST ( \ s -> let
		    (a, s')  = newArray# 100 [] s
		    (_, s'') = fill_in_array_or_something a x s'
		  in
		  freezeArray# a s'' )
\end{verbatim}
If we inline @runST@, we'll get:
\begin{verbatim}
f x = let
	(a, s')  = newArray# 100 [] realWorld#{-NB-}
	(_, s'') = fill_in_array_or_something a x s'
      in
      freezeArray# a s''
\end{verbatim}
And now the @newArray#@ binding can be floated to become a CAF, which
is totally and utterly wrong:
\begin{verbatim}
f = let
    (a, s')  = newArray# 100 [] realWorld#{-NB-} -- YIKES!!!
    in
    \ x ->
	let (_, s'') = fill_in_array_or_something a x s' in
	freezeArray# a s''
\end{verbatim}
All calls to @f@ will share a {\em single} array!  End SLPJ 95/04.

\begin{code}
{-# INLINE runST #-}
-- The INLINE prevents runSTRep getting inlined in *this* module
-- so that it is still visible when runST is inlined in an importing
-- module.  Regrettably delicate.  runST is behaving like a wrapper.
runST :: (forall s. ST s a) -> a
runST st = runSTRep (case st of { ST st_rep -> st_rep })

-- I'm only letting runSTRep be inlined right at the end, in particular *after* full laziness
-- That's what the "INLINE 100" says.
-- 		SLPJ Apr 99
{-# INLINE 100 runSTRep #-}
runSTRep :: (forall s. STRep s a) -> a
runSTRep st_rep = case st_rep realWorld# of
	      		(# _, r #) -> r
\end{code}
