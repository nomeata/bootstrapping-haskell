%
% (c) The AQUA Project, Glasgow University, 1994-1999
%
\section[Monad]{Module @Monad@}

\begin{code}
module Monad 
    ( MonadPlus (   -- class context: Monad
	  mzero     -- :: (MonadPlus m) => m a
	, mplus     -- :: (MonadPlus m) => m a -> m a -> m a
	)
    , join          -- :: (Monad m) => m (m a) -> m a
    , guard	    -- :: (MonadPlus m) => Bool -> m ()
    , when          -- :: (Monad m) => Bool -> m () -> m ()
    , unless        -- :: (Monad m) => Bool -> m () -> m ()
    , ap	    -- :: (Monad m) => (m (a -> b)) -> (m a) -> m b
    , msum	    -- :: (MonadPlus m) => [m a] -> m a
    , filterM	    -- :: (Monad m) => (a -> m Bool) -> [m a] -> m [a]
    , mapAndUnzipM  -- :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
    , zipWithM      -- :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
    , zipWithM_     -- :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
    , foldM	    -- :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a 
    
    , liftM	    -- :: (Monad m) => (a -> b) -> (m a -> m b)
    , liftM2	    -- :: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)
    , liftM3        -- :: ...
    , liftM4        -- :: ...
    , liftM5        -- :: ...

    , Monad((>>=), (>>), return, fail)
    , Functor(fmap)

    , mapM	    -- :: (Monad m) => (a -> m b) -> [a] -> m [b]
    , mapM_	    -- :: (Monad m) => (a -> m b) -> [a] -> m ()
    , sequence	    -- :: (Monad m) => [m a] -> m [a]
    , sequence_     -- :: (Monad m) => [m a] -> m ()
    , (=<<)         -- :: (Monad m) => (a -> m b) -> m a -> m b
    ) where

import Prelude
\end{code}

%*********************************************************
%*							*
\subsection{Monadic classes: @MonadPlus@}
%*							*
%*********************************************************


\begin{code}
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus [] where
   mzero = []
   mplus = (++)

instance MonadPlus Maybe where
   mzero = Nothing

   Nothing `mplus` ys  = ys
   xs      `mplus` _ys = xs
\end{code}


%*********************************************************
%*							*
\subsection{Functions mandated by the Prelude}
%*							*
%*********************************************************

\begin{code}
guard           :: MonadPlus m => Bool -> m ()
guard pred
 | pred      = return ()
 | otherwise = mzero

-- This subsumes the list-based filter function.

filterM		:: (Monad m) => ( a -> m Bool ) -> [a] -> m [a]
filterM _predM []     = return []
filterM  predM (x:xs) = do
   flg <- predM x
   ys  <- filterM predM xs
   return (if flg then x:ys else ys)

-- This subsumes the list-based concat function.

msum        :: MonadPlus m => [m a] -> m a
{-# INLINE msum #-}
msum        =  foldr mplus mzero
\end{code}


%*********************************************************
%*							*
\subsection{Other monad functions}
%*							*
%*********************************************************

\begin{code}
join             :: (Monad m) => m (m a) -> m a
join x           = x >>= id

mapAndUnzipM     :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs = sequence (map f xs) >>= return . unzip

zipWithM         :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence (zipWith f xs ys)

zipWithM_ :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

foldM            :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ a []     = return a
foldM f a (x:xs) = f a x >>= \fax -> foldM f fax xs

unless 		 :: (Monad m) => Bool -> m () -> m ()
unless p s 	 =  if p then return () else s

when 		 :: (Monad m) => Bool -> m () -> m ()
when p s	 =  if p then s else return ()

ap :: (Monad m) => m (a->b) -> m a -> m b
ap = liftM2 ($)

liftM	:: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM2	:: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM3	:: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM4	:: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM5	:: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r

liftM f m1	        = do { x1 <- m1; return (f x1) }
liftM2 f m1 m2 		= do { x1 <- m1; x2 <- m2; return (f x1 x2) }
liftM3 f m1 m2 m3 	= do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }
liftM4 f m1 m2 m3 m4	= do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; return (f x1 x2 x3 x4) }
liftM5 f m1 m2 m3 m4 m5 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; return (f x1 x2 x3 x4 x5) }

\end{code}
