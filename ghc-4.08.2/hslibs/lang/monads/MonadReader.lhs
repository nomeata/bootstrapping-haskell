\h1{MonadReader}

\haskell:module{
  \Name{MonadReader}
  \Version{0.2}
  \License{
        The Haskell Monad Template Library is Copyright &copy;
        Andy Gill, and the Oregon Graduate Institute of Science and
        Technology, 1999, All rights reserved, and is distributed as
        free software under the license in the file "License", which
	is included in the distribution.}
  \Author{
	Rendered by \A[HREF="http://www.cse.ogi.edu/~andy"]{Andy Gill},
	inspired by the paper
	\em{Functional Programming with Overloading and
	    Higher-Order Polymorphism}, 
	  \A[HREF="http://www.cse.ogi.edu/~mpj"]{Mark P Jones},
		Advanced School of Functional Programming, 1995.}
  \Restrictions{	
	This requires multi parameter classes.
	}
  \Tested{Hugs98, GHC 4.03}
}

\begin{code}
module MonadReader (
      MonadReader(..),
      Reader,         -- abstract
      asks,
      runReader,
      mapReader,
      withReader,
      ReaderT,        -- abstract
      runReaderT,
      mapReaderT,
      withReaderT,
      module MonadTrans,
      module MonadFix,
      ) where
\end{code}

\begin{code}
import Monad
import MonadTrans
import MonadFix
\end{code}

\haskell:class{
  \Name{MonadReader}
  \ask{asks for the internal (non-mutable) state.}
}

\begin{code}
class (Monad m) => MonadReader s m where
      ask   :: m s
      local :: (s -> s) -> m a -> m a
\end{code}


This allows you to provide a projection function.

\begin{code}
asks :: (MonadReader s m) => (s -> a) -> m a
asks f = do s <- ask
            return (f s)
\end{code}

Our parameterizable reader monad

\begin{code}
newtype Reader w a = Reader { runReader :: w -> a }
\end{code}

\begin{code}
instance Functor (Reader w) where
      fmap f m = Reader (\ w -> f (runReader m w))

instance Monad (Reader w) where
   return v  = Reader (\ w -> v)
   p  >>= f  = Reader (\ w -> runReader (f (runReader p w)) w)
   fail str  = Reader (\ w -> error str)

instance MonadReader w (Reader w) where
	ask = Reader (\ w -> w)
	local g m = Reader (runReader m .  g)

instance MonadFix (Reader w) where
	mfix f = Reader (\ w -> let a = runReader (f a) w in a)
\end{code}

\begin{code}
mapReader :: (a -> b) -> Reader w a -> Reader w b
mapReader g m = Reader (g . runReader m)
\end{code}

This is a more general version of local.

\begin{code}
withReader :: (w -> w2) -> Reader w2 a -> Reader w a
withReader g m = Reader (runReader m .  g)
\end{code}

Our parameterizable reader monad, with inner monad

\begin{code}
newtype ReaderT w m a = ReaderT { runReaderT :: w -> m a }
\end{code}

\begin{code}
instance (Monad m) => Functor (ReaderT w m) where
	fmap f p = ReaderT (\ w ->
 		do x <- runReaderT p w
 		   return (f x))

instance (Monad m) => Monad (ReaderT w m) where
	return v  = ReaderT (\ w -> return v)
	p  >>= f  = ReaderT (\ w -> do r <- runReaderT p w
                                       runReaderT (f r) w)
	fail str  = ReaderT (\ w -> fail str)

instance (MonadPlus m) => MonadPlus (ReaderT w m) where
	mzero       = ReaderT (\ w -> mzero)
	p `mplus` q = ReaderT (\ w -> runReaderT p w `mplus` runReaderT q w)

instance (Monad m) => MonadReader w (ReaderT w m) where
	ask = ReaderT (\ w -> return w)
	local g m = ReaderT (\ w -> runReaderT m (g w))

instance MonadTrans (ReaderT w) where
      lift f = ReaderT (\ w -> do { r <- f ; runReaderT (return r) w })

instance (MonadFix m) => MonadFix (ReaderT s m) where
	mfix f = ReaderT (\ s -> mfix (\ a' ->
				do a <- runReaderT (f a') s
				   return a))
\end{code}

\begin{code}
mapReaderT :: (m a -> n b) -> ReaderT w m a -> ReaderT w n b
mapReaderT g m = ReaderT (g . runReaderT m)

withReaderT :: (Monad m) => (w -> w2) -> ReaderT w2 m a -> ReaderT w m a
withReaderT g m = ReaderT (\ w -> runReaderT m (g w))
\end{code}

