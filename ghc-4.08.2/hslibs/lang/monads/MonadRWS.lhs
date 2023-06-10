g\h1{MonadRWS}
\haskell:module{
  \Name{MonadRWS}
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
module MonadRWS (
      RWS,         -- abstract
      runRWS,
      mapRWS,
      RWST,        -- abstract
      runRWST,
--      mapRWST,
      module MonadTrans,
      module MonadState,
      module MonadReader,
      module MonadWriter,
      ) where
\end{code}

\begin{code}
import Monad
import Monoid
import MonadTrans
import MonadFix
import MonadState
import MonadReader
import MonadWriter
\end{code}


\begin{code}
newtype RWS r w s a = RWS { runRWS :: r -> s -> (a,s,w) }

instance Functor (RWS r w s) where
	fmap f p = RWS (\ r s ->
		let (x,s',w') = runRWS p r s
		in  (f x,s',w'))

instance (Monoid w) => Monad (RWS r w s) where
   return v  = RWS (\ r s -> (v,s,mempty))
   (>>=) k m = RWS (\ r s -> let (v,s',w) = runRWS k r s
				 (v',s'',w') = runRWS (m v) r s'
			       in (v',s'',w `mappend` w'))

instance (Monoid w) => MonadState s (RWS r w s) where
	get   = RWS (\ r s -> (s,s,mempty))
	put v = RWS (\ r _ -> ((),v,mempty))

instance (Monoid w) => MonadReader r (RWS r w s) where
	ask = RWS (\ r s -> (r,s,mempty))
	local f m = RWS (\ r s -> runRWS m (f r) s)

instance (Monoid w) => MonadWriter w (RWS r w s) where
	tell a = RWS (\ r s -> ((),s,a))
	listen m = RWS (\ r s -> let (v,s',w) = runRWS m r s
				 in ((v,w),s',w))
	pass m = RWS (\ r s -> let ((v,f),s',w) = runRWS m r s
				   in (v,s',f w))
instance(Monoid w) => MonadFix (RWS r w s) where
   mfix f = RWS ( \ r s -> let (v,s',w) = runRWS (f v) r s
			   in (v,s',w))
\end{code}

\begin{code}
mapRWS :: ((a,s,w) -> (a',s,w')) -> RWS r w s a -> RWS r w' s a'
mapRWS f m = RWS (\ r s -> f (runRWS m r s))

withRWS :: (r -> r') -> RWS r' w s a -> RWS r w s a
withRWS f m = RWS (\ r s -> runRWS m (f r) s)
\end{code}

\begin{code}
newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a,s,w) }

instance (Monad m) => Functor (RWST r w s m) where
	fmap f p = RWST (\ r s ->
		         do (x,s',w') <- runRWST p r s
			    return (f x,s',w'))
instance (Monad m,Monoid w) => Monad (RWST r w s m) where
   return v  = RWST (\ r s -> return (v,s,mempty))
   (>>=) k m = RWST (\ r s -> do (v,s',w) <- runRWST k r s
				 (v',s'',w') <- runRWST (m v) r s'
			         return (v',s'',w `mappend` w'))

instance (Monad m,Monoid w) => MonadState s (RWST r w s m) where
	get   = RWST (\ r s -> return (s,s,mempty))
	put v = RWST (\ r _ -> return ((),v,mempty))

instance (Monad m,Monoid w) => MonadReader r (RWST r w s m) where
	ask = RWST (\ r s -> return (r,s,mempty))
	local f m = RWST (\ r s -> runRWST m (f r) s)

instance (Monad m,Monoid w) => MonadWriter w (RWST r w s m) where
	tell a = RWST (\ r s -> return ((),s,a))
	listen m = RWST (\ r s -> do (v,s',w) <- runRWST m r s
				     return ((v,w),s',w))
	pass m = RWST (\ r s -> do ((v,f),s',w) <- runRWST m r s
				   return (v,s',f w))

instance (MonadFix m,Monoid w) => MonadFix (RWST r w s m) where
   mfix f = RWST (\ r s -> mfix (\ ~(v',_,_) ->
					do (v,s',w) <- runRWST (f v') r s
					   return (v,s',w)))

instance (Monoid w) => MonadTrans (RWST r w s) where
   lift f = RWST (\ r s -> do { r <- f ; runRWST (return r) r s })

\end{code}

\begin{code}
mapRWST :: (m (a,s,w) -> n (a',s,w')) -> RWST r w s m a -> RWST r w' s n a'
mapRWST f m = RWST (\ r s -> f (runRWST m r s))

withRWST :: (r -> r') -> RWST r' w s m a -> RWST r w s m a
withRWST f m = RWST (\ r s -> runRWST m (f r) s)
\end{code}


