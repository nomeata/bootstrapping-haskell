\h1{MonadEither}

\haskell:module{
  \Name{MonadEither}
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
	This requires the ability to overload at complex
	types.
	}
  \Tested{Hugs98, GHC 4.03}
}

\begin{code}
module MonadEither where

import Monad

import MonadFix

class Error a where
      noMsg  :: a
      strMsg :: String -> a

      noMsg = error "Error: no msg"
      strMsg = error

instance Functor (Either a) where
      fmap f (Right a) = Right (f a)
      fmap f (Left  a) = Left a

instance Error a => Monad (Either a) where
      return v  = Right v
      p >>= f  = case p of
                    Left a -> Left a
                    Right a -> f a
      fail str = Left (strMsg str)

instance (Error a) => MonadPlus (Either a) where
      mzero       = Left (noMsg)
      p `mplus` q = case p of
                      Left a -> q
                      Right a -> Right a


-- This is unusual, because the return succ/fail
-- can not be depend of the fixpoint value.
instance (Error a) => MonadFix (Either a) where
	mfix f = let v = f x
		     x = case v of
			   Right x -> x
			   Left msg -> error "empty mfix argument"
                 in v

instance Error [Char] where
      noMsg = ""
      strMsg = id
\end{code}
