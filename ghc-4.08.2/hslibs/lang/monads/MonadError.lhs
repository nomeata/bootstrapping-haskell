\haskell:module{
  \Name{MonadError}
  \Version{0.1}
  \Description{
	Declaration of \haskell:expr{MonadError} class.}
  \License{
        Copyright &copy 2000  Michael Weber <michael.weber@post.rwth-aachen.de>

        This library is free software and is distributed as
        free software under the license in the file "License", which
	is included in the distribution.}
  \Author{
	Rendered by Michael Weber <michael.weber@post.rwth-aachen.de>,
	inspired by the Haskell Monad Template Library from
	 \A[HREF="http://www.cse.ogi.edu/~andy"]{Andy Gill}}
  \Restrictions{	
	needs multi-parameter type classes.
  }
  \Tested{Hugs98, GHC 4.07}
}
\begin{code}
module MonadError where
\end{code}
 
\begin{code}
import MonadTrans
import MonadState
\end{code}

\haskell:class{
  \Name{MonadError}
  \throwError{throws an exception inside the monad and thus interrupts
      normal execution order, until an error handler is reached}
  \catchError{catches an exception inside the monad (that was previously
      thrown by throwError}
}

\begin{code}
class (Monad m) => MonadError e m where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a
\end{code}


\h2{A Parameterizable Error Monad}

Our parameterizable error monad, with an inner monad.
\begin{code}
newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }
\end{code}

The ErrorT Monad structure is parameterized over two things:
\ul{
 \li{e - The error type.}
 \li{m - The inner monad.}
}

Here are some examples of use:

\haskell:code[bgcolor="#ff88ff"]{
   type ErrorWithIO e a = ErrorT e IO a
      ==> ErrorT (IO (Either e a))

   type ErrorAndStateWithIO e s a = ErrorT e (StateT s IO) a
      ==> ErrorT (StateT s IO (Either e a))
      ==> ErrorT (StateT (s -> IO (Either e a,s)))
}

\begin{code}
instance (Monad m) => Functor (ErrorT e m) where
      -- fmap :: (a -> b) -> ErrorT e m a -> ErrorT e m b
      fmap f p = ErrorT (runErrorT p >>= \ e -> case e of
                            Right r -> return (Right (f r))
                            Left l  -> return (Left l)
			)


instance (Monad m) => Monad (ErrorT e m) where
    return  = ErrorT . return . Right
    m >>= f = ErrorT (runErrorT m >>= \ res -> case res of
                         Right r -> runErrorT (f r)
			 Left l  -> return (Left l)
		     )


instance (Monad m) => MonadError e (ErrorT e m) where
    throwError       = ErrorT . return . Left
    m `catchError` h = ErrorT (runErrorT m >>= \ e -> case e of
                                  Right r -> return (Right r)
				  Left l  -> runErrorT (h l)
			      )


instance MonadTrans (ErrorT e) where
   lift f = ErrorT (do { r <- f ; runErrorT (return r) })


instance MonadState s m => MonadState s (ErrorT e m) where
    get = lift get
    put = lift . put
\end{code}
