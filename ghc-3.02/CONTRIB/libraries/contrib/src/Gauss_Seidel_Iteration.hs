-- $Id: Gauss_Seidel_Iteration.hs,v 1.1 1998/02/03 12:51:01 simonm Exp $

module Gauss_Seidel_Iteration where

-- Perform Gauss-Seidel Iteration to solve "Ax = b"

-------------

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920804
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

-- K&C - Numerical Analysis - David Kincaid & Ward Cheney - Brooks/Cole 1991

-------------

import Maybe
import Vector_Ops(vector_sub)
import Array -- 1.3
infix 1 =:
(=:) a b = (a,b)

-------------

gauss_seidel_iteration 
  :: (Ix a, Fractional b, Enum a, Num a)
  => (Array (a, a) b) -> (Array a b) -> (Array a b) -> Array a b

gauss_seidel_iteration a b x = x'
  where
    x' = array bs [ i =: g i | i <- range bs ]
    g i = (b!i - (f i x' m (i-1)) - (f i x (i+1) n))/a!(i,i)
    f i p l u = sum [ (p!j)*a!(i,j) | j <- [l..u]]
    bs@(m,n) = bounds x

-- The heart of the Gauss-Seidel method.  This produces one iteration of the
-- Gauss-Seidel method.
-- B&F pp 427, K&C p185

-- A possible speed up would be to do the (/a!(i,i)) part once at the start
-- However, this only makes sense if you are doing multiple iterations
-- and so this optimisation would have to be pulled out into the list or
-- tolerance version.  Could a partial evaluator do this?

-------------

gauss_seidel_list 
  :: (Ix a, Fractional b, Enum a, Num a)
  => (Array (a, a) b) -> (Array a b) -> (Array a b) -> [Array a b]

gauss_seidel_list a b x = iterate (gauss_seidel_iteration a b) x

-- an infinite list of Gauss-Seidel approximations to the solution of "Ax = b"

-------------

gauss_seidel_to_tolerance
  :: (Ord c, Ix a, Num b)
  => ((Array a b) -> c) -> c -> [Array a b] -> Maybe (Array a b)

gauss_seidel_to_tolerance norm tol [] = Nothing
gauss_seidel_to_tolerance norm tol (x:r@(x':_)) = 
  if norm (vector_sub x x') < tol
  then Just x'
  else gauss_seidel_to_tolerance norm tol r

-- perform Gauss-Seidel iteration until the result is within the required
-- tolerance _tol_ using _norm_ as the vector normaliser
-- (probably vector_inf_norm accoding to B&F pp 427)

-------------

{-
An example.

    [ 10 -1  1  2 ]       [   6 ]         [ 0 ]
A = [ -1 11 -1  3 ]   b = [  25 ]    x0 = [ 0 ]
    [  2 -1 10  1 ]       [ -11 ]         [ 0 ]
    [  0  3 -1  8 ]       [  15 ]         [ 0 ]

This converts into :-

  a = listArray ((1,1),(4,4)) [10,-1,2,0,-1,11,-1,3,2,-1,10,-1,0,3,-1,8]
  b = listArray (1,4) [6,25,-11,15]
  x = listArray (1,4) [0,0,0,0]

and a one step evalution :-

  gauss_seidel_iteration a b x 

should give :-

[ 5.9999999999999998e-1
, 2.3272727272727276
, -9.8727272727272730e-1
, 4 =: 8.7886363636363629e-1
]

  (gauss_seidel_list a b x) !! 10

should give :-

[ 9.9999999998681166e-1
, 1.9999999998595697
, -9.9999999997639066e-1
, 1.0000000000556126
]

-}

-- eof
