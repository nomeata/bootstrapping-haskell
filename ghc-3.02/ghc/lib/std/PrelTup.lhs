%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelTup]{Module @PrelTup@}

This modules defines the typle data types.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelTup where

import {-# SOURCE #-} PrelErr ( error )
import PrelBase
\end{code}


%*********************************************************
%*							*
\subsection{Other tuple types}
%*							*
%*********************************************************

\begin{code}
data (,) a b = (,) a b   deriving (Eq, Ord, Bounded)
data (,,) a b c = (,,) a b c deriving (Eq, Ord, Bounded)
data (,,,) a b c d = (,,,) a b c d deriving (Eq, Ord, Bounded)
data (,,,,) a b c d e = (,,,,) a b c d e deriving (Eq, Ord, Bounded)
data (,,,,,) a b c d e f = (,,,,,) a b c d e f
data (,,,,,,) a b c d e f g = (,,,,,,) a b c d e f g
data (,,,,,,,) a b c d e f g h = (,,,,,,,) a b c d e f g h
data (,,,,,,,,) a b c d e f g h i = (,,,,,,,,) a b c d e f g h i
data (,,,,,,,,,) a b c d e f g h i j = (,,,,,,,,,) a b c d e f g h i j
data (,,,,,,,,,,) a b c d e f g h i j k = (,,,,,,,,,,) a b c d e f g h i j k
data (,,,,,,,,,,,) a b c d e f g h i j k l = (,,,,,,,,,,,) a b c d e f g h i j k l
data (,,,,,,,,,,,,) a b c d e f g h i j k l m = (,,,,,,,,,,,,) a b c d e f g h i j k l m
data (,,,,,,,,,,,,,) a b c d e f g h i j k l m n = (,,,,,,,,,,,,,) a b c d e f g h i j k l m n
data (,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o = (,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o
data (,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p = (,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p
data (,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q
 = (,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q
data (,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r
 = (,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r
data (,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s
 = (,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s
data (,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t
 = (,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t
data (,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u
 = (,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u
data (,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v
 = (,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v
data (,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w
 = (,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w
data (,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x
 = (,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x
data (,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y
 = (,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y
data (,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z
 = (,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z
data (,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_
\end{code}

@Show@ instances for just the first few.

\begin{code}
instance  (Show a, Show b) => Show (a,b)  where
    showsPrec p (x,y) = showChar '(' . shows x . showChar ',' .
                                       shows y . showChar ')'
    showList	= showList__ (showsPrec 0) 

instance (Show a, Show b, Show c) => Show (a, b, c) where
    showsPrec p (x,y,z) = showChar '(' . showsPrec 0 x . showChar ',' .
					 showsPrec 0 y . showChar ',' .
					 showsPrec 0 z . showChar ')'
    showList	= showList__ (showsPrec 0) 

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
    showsPrec p (w,x,y,z) = showChar '(' . showsPrec 0 w . showChar ',' .
					   showsPrec 0 x . showChar ',' .
					   showsPrec 0 y . showChar ',' .
					   showsPrec 0 z . showChar ')'

    showList	= showList__ (showsPrec 0) 

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
    showsPrec p (v,w,x,y,z) = showChar '(' . showsPrec 0 v . showChar ',' .
					     showsPrec 0 w . showChar ',' .
					     showsPrec 0 x . showChar ',' .
					     showsPrec 0 y . showChar ',' .
					     showsPrec 0 z . showChar ')'
    showList	= showList__ (showsPrec 0) 
\end{code}


%*********************************************************
%*							*
\subsection{Standard functions over tuples}
*							*
%*********************************************************

\begin{code}
fst			:: (a,b) -> a
fst (x,y)		=  x

snd			:: (a,b) -> b
snd (x,y)		=  y

-- curry converts an uncurried function to a curried function;
-- uncurry converts a curried function to a function on pairs.
curry                   :: ((a, b) -> c) -> a -> b -> c
curry f x y             =  f (x, y)

uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p             =  f (fst p) (snd p)
\end{code}

