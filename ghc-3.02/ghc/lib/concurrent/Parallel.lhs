%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[Parallel]{Parallel Constructs}

\begin{code}
module Parallel (par, seq -- re-exported
#if defined(__GRANSIM__)
	, parGlobal, parLocal, parAt, parAtAbs, parAtRel, parAtForNow     
#endif
    ) where

import PrelConc	( par )

#if defined(__GRANSIM__)
import PrelBase
import PrelErr   ( parError )
import PrelGHC   ( parGlobal#, parLocal#, parAt#, parAtAbs#, parAtRel#, parAtForNow# )

{-# INLINE parGlobal #-}
{-# INLINE parLocal #-}
{-# INLINE parAt #-}
{-# INLINE parAtAbs #-}
{-# INLINE parAtRel #-}
{-# INLINE parAtForNow #-}
parGlobal   :: Int -> Int -> Int -> Int -> a -> b -> b
parLocal    :: Int -> Int -> Int -> Int -> a -> b -> b
parAt	    :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAtAbs    :: Int -> Int -> Int -> Int -> Int -> a -> b -> b
parAtRel    :: Int -> Int -> Int -> Int -> Int -> a -> b -> b
parAtForNow :: Int -> Int -> Int -> Int -> a -> b -> c -> c

parGlobal (I# w) (I# g) (I# s) (I# p) x y = case (parGlobal# x w g s p y) of { 0# -> parError; _ -> y }
parLocal  (I# w) (I# g) (I# s) (I# p) x y = case (parLocal#  x w g s p y) of { 0# -> parError; _ -> y }

parAt       (I# w) (I# g) (I# s) (I# p) v x y = case (parAt#       x v w g s p y) of { 0# -> parError; _ -> y }
parAtAbs    (I# w) (I# g) (I# s) (I# p) (I# q) x y = case (parAtAbs#  x q w g s p y) of { 0# -> parError; _ -> y }
parAtRel    (I# w) (I# g) (I# s) (I# p) (I# q) x y = case (parAtRel#  x q w g s p y) of { 0# -> parError; _ -> y }
parAtForNow (I# w) (I# g) (I# s) (I# p) v x y = case (parAtForNow# x v w g s p y) of { 0# -> parError; _ -> y }

#endif

-- Maybe parIO and the like could be added here later.
\end{code}
