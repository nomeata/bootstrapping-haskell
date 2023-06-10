\h1{MonadWriter}
\haskell:module{
  \Name{MonadWriter}
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
	This requires multi parameter type classes.
	}
  \Tested{Hugs98, GHC 4.03}
}
\begin{code}
module MonadWriter (
      MonadWriter(..),
      Writer,         -- abstract
      listens,
      censor,
      runWriter,
      mapWriter,
      WriterT,        -- abstract
      runWriterT,
      mapWriterT,
      module MonadTrans 
      ) where
\end{code}

\begin{code}
import Monad
import Monoid
import MonadTrans
import MonadFix
\end{code}

\haskell:class{
  \Name{MonadWriter}
  \tell{tell is like tell on the MUD's it shouts to monad
        what you want to be heard. The monad carries this 'packet'
	upwards, merging it if needed (hence the Monoid requirement)}
  \listen{listen listens to a monad acting, and returns what
          the monad "said"}.
  \pass{pass lets you provide a writer transformer,
        which changes to internals of the written object}
}

\begin{code}
class (Monoid s,Monad m) => MonadWriter s m where
      tell :: s -> m ()
      listen :: m a -> m (a,s)
      pass :: m (a,s -> s) -> m a
\end{code}

\begin{code}
listens p m = do (a,s) <- listen m
                 return (a,p s)

censor :: (MonadWriter s m) => (s -> s) -> m a -> m a
censor f m = pass (do v <- m ; return (v,f))
\end{code}

Our parameterizable writer monad.
\begin{code}
newtype Writer w a = Writer { runWriter :: (a,w) }
\end{code}

\begin{code}
instance Functor (Writer w) where
      fmap f p = Writer (
              let (x,w') = runWriter p
              in  (f x,w'))

instance (Monoid w) => Monad (Writer w) where
   return v  = Writer (v,mempty)
   p  >>= f  = Writer (let (r,w) = runWriter p
                           (r',w') = runWriter (f r)
                     in (r',w `mappend` w'))

instance (Monoid w) => MonadWriter w (Writer w) where
      tell a = Writer ((),a)
      listen (Writer (v,r)) = Writer ((v,r),r)
      pass m = Writer (let ((v,f),r) = runWriter m
		       in (v,f r))

instance (Monoid w) => MonadFix (Writer w) where
	mfix m = Writer(let (v,r) = runWriter (m v)
			    in (v,r))
\end{code}
\begin{code}
mapWriter :: ((a,w) -> (b,w)) -> Writer w a -> Writer w b
mapWriter f m = Writer (f (runWriter m))
\end{code}

 Our parameterizable writer monad, with inner state.
\begin{code}
newtype WriterT w m a = WriterT { runWriterT :: m (a,w) }
\end{code}

\begin{code}
instance (Monad m) => Functor (WriterT w m) where
      fmap f p = WriterT (
              do (x,w') <- runWriterT p
                 return (f x,w'))

instance (Monoid w,Monad m) => Monad (WriterT w m) where
   return v  = WriterT (return (v,mempty))
   p  >>= f  = WriterT (do (r,w) <- runWriterT p
                           (r',w') <- runWriterT (f r)
                           return (r',w `mappend` w'))
   fail str  = WriterT (fail str)

instance (Monoid w,MonadPlus m) => MonadPlus (WriterT w m) where
      mzero       = WriterT (mzero)
      p `mplus` q = WriterT (runWriterT p `mplus` runWriterT q)

instance (Monoid w,Monad m) => MonadWriter w (WriterT w m) where
      tell a = WriterT (return ((),a))
      listen m = WriterT (
                          do (v,r) <- runWriterT m
                             return ((v,r),r))
      pass m = WriterT (do ((v,f),r) <- runWriterT m
		           return (v,f r))

instance (MonadFix m,Monoid w) => MonadFix (WriterT w m) where
	mfix m = WriterT (mfix (\ ~(v',_) -> do (v,r) <- runWriterT (m v')
			                        return (v,r)))

instance (Monoid w) => MonadTrans (WriterT w) where
      lift f = WriterT (do { r <- f ; runWriterT (return r) })
\end{code}

\begin{code}
mapWriterT :: (m (a,w) -> n (b,w)) -> WriterT w m a -> WriterT w n b
mapWriterT f m = WriterT (f (runWriterT m))
\end{code}
