\h1{MonadState}
\haskell:module{
  \Name{MonadState}
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
module MonadState (
	MonadState(..),
	modify,
	gets,
	State,          -- abstract
	runState,
	mapState,
	evalState,
	execState,
	StateT,         -- abstract
	runStateT,
	mapStateT,
	evalStateT,
	execStateT,
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
  \Name{MonadState}
  \get{returns the state from the internals of the monad.}
  \put{changes (replaces) the state inside the monad.}
}

\begin{code}
class (Monad m) => MonadState s m where
	get :: m s
	put :: s -> m ()
\end{code}

\haskell:function{
  \Purpose{Monadic state transformer.}
  \Description{
      Maps an old state to a new state inside a state monad.
      The old state is thrown away.}
  \Example{
	\haskell:code[bgcolor="#ff88ff"]{
	  Main> :t modify ((+1) :: Int -> Int)
	  modify (...) :: (MonadState Int a) => a ()
	}
	{This says that modify (+1) acts over any
	Monad that is a member of the MonadState class,
	with an \haskell:expr{Int} state.}
  }
}
\begin{code}
modify :: (MonadState s m) => (s -> s) -> m ()
modify f = do s <- get
              put (f s)
\end{code}

\haskell:function{
  \Purpose{gets part of the state}
  \Description{
	gets specific component of the state,
	using a projection function supplied.
  }
	
}
\begin{code}
gets :: (MonadState s m) => (s -> a) -> m a
gets f = do s <- get
            return (f s)
\end{code}

\h2{Our Parameterizable State Monad}

\begin{code}
newtype State s a = State { runState :: s -> (a,s) }
\end{code}

The State Monad structure is paramterized over just the state.

\begin{code}
instance Functor (State s) where
      fmap f p = State (\ s ->
              let (x,s') = runState p s
              in  (f x,s'))

instance Monad (State s) where
   return v  = State (\ s -> (v,s))
   p  >>= f  = State (\ s -> let (r,s') = runState p s
                           in runState (f r) s')
   fail str  = State (\ s -> error str)

instance MonadState s (State s) where
      get   = State (\ s -> (s,s))
      put v = State (\ _ -> ((),v))

instance MonadFix (State s) where
	mfix f = State ( \ s -> let (a,s') = runState (f a) s
                           in (a,s'))
\end{code}

\begin{code}
mapState :: ((a,s) -> (b,s)) -> State s a -> State s b
mapState f m = State (f . runState m)
\end{code}

\begin{code}
withState :: (s -> s) -> State s a -> State s a
withState f m = State (runState m . f)
\end{code}

\begin{code}
evalState :: State s a -> s -> a
evalState m s = fst (runState m s)
\end{code}

\begin{code}
execState :: State s a -> s -> s
execState m s = snd (runState m s)  
\end{code}

\h2{Another Parameterizable State Monad}

Our parameterizable state monad, with an inner monad.
\begin{code}
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
\end{code}

The StateT Monad structure is parameterized over two things:
\ul{
 \li{s - The state.}
 \li{m - The inner monad.}
}

Here are some examples of use:

\haskell:code[bgcolor="#ff88ff"]{
-- (Parser from ParseLib with Hugs)
   type Parser a = StateT String [] a
      ==> StateT (String -> [(a,String)])
-- For example, item can be written as:
   	item = do (x:xs) <- get
		  put xs
		  return x

   type BoringState s a = StateT s Indentity a
      ==> StateT (s -> Identity (a,s))

   type StateWithIO s a = StateT s IO a
      ==> StateT (s -> IO (a,s))

   type StateWithErr s a = StateT s Maybe a
      ==> StateT (s -> Maybe (a,s))
}

\begin{code}
instance (Monad m) => Functor (StateT s m) where
      -- fmap :: (a -> b) -> StateT s m a -> StateT s m b
      fmap f p = StateT (\ s ->
              do (x,s') <- runStateT p s
                 return (f x,s'))
 
instance (Monad m) => Monad (StateT s m) where
   return v  = StateT (\ s -> return (v,s))
   p  >>= f  = StateT (\ s -> do (r,s') <- runStateT p s
                                 runStateT (f r) s')
   fail str  = StateT (\ s -> fail str)

instance (MonadPlus m) => MonadPlus (StateT s m) where
      mzero       = StateT (\ s -> mzero)
      p `mplus` q = StateT (\ s -> runStateT p s `mplus` runStateT q s)

instance (Monad m) => MonadState s (StateT s m) where
      get   = StateT (\ s -> return (s,s))
      put v = StateT (\ _ -> return ((),v))

instance MonadTrans (StateT s) where
   lift f = StateT ( \ s -> do { r <- f ; runStateT (return r) s })

instance (MonadFix m) => MonadFix (StateT s m) where
	mfix f = StateT (\ s -> mfix (\ ~(a',_) ->
				do (a,s') <- runStateT (f a') s
				   return (a,s')))
\end{code}

\begin{code}
mapStateT :: (m (a,s) -> n (b,s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT (f . runStateT m)
\end{code}

\begin{code}
withStateT :: (Monad m) => (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT (\ s -> runStateT m (f s))
\end{code}

\begin{code}
evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s =  
      do (r,_) <- runStateT m s
         return r
\end{code}

\begin{code}
execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s =  
      do (_,s) <- runStateT m s
         return s
\end{code}


