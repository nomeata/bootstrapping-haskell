\haskell:module{
  \Name{MonadIdentity}
  \Version{0.2}
  \Description{
	Declaration of the Identity monad.}
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
	This works with all Haskell 98 compilers.
  }
  \Tested{Hugs98, GHC 4.03}
}
\begin{code}
module MonadIdentity (
       Identity,               -- abstract
       runIdentity,
       ) where
\end{code}

\haskell:datatype{
  \Purpose{Identity wrapper}
  \Description{
	Abstraction for wrapping up a object.
	It is exported abstract.}
  \Example{
	If you have an monadic function, say:
	\haskell:code[bgcolor="#ff88ff"]{
	  example :: Int -> IdentityMonad Int
	  example x = return (x*x)
	}
	you can "run" it, using
	\haskell:code[bgcolor="#ff88ff"]{
	Main> runIdentity (example 42)
	1764 :: Int
	}
  }   
}
\begin{code}
newtype Identity a = Identity { runIdentity :: a }
\end{code}

\haskell:instance{
  \Purpose{Identity instances for Functor and Monad}
}

\begin{code}
instance Functor Identity where
	fmap f xs = Identity (f (runIdentity xs))
instance Monad Identity where
	return v = Identity v
	x >>= f  = f (runIdentity x)
\end{code}
