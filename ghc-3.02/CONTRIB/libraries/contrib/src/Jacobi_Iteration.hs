-- $Id: Jacobi_Iteration.hs,v 1.1 1998/02/03 12:51:05 simonm Exp $

module Jacobi_Iteration where

-- Perform Jacobi Iteration to solve "Ax = b"

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

jacobi_iteration
  :: (Ix a, Fractional b, Enum a, Num a)
  => (Array (a, a) b) -> (Array a b) -> (Array a b) -> Array a b 

jacobi_iteration a b x = x'
  where
    x' = array bs [ i =:  g i | i <- range bs ]
    g i = (b!i - (f i m (i-1) + f i (i+1) n))/a!(i,i)
    f i l u = sum [ (x!j)*a!(i,j) | j <- [l..u]]
    bs@(m,n) = bounds x

-- The heart of the Jacobi method.  This produces one iteration of the
-- Jacobi method.
-- B&F pp 427, K&C p185

-- see Gauss-Seidel method for a description of a possible speed up.

-------------

jacobi_list
  :: (Ix a, Fractional b, Enum a, Num a)
  => (Array (a, a) b) -> (Array a b) -> (Array a b) -> [Array a b] 

jacobi_list a b x = iterate (jacobi_iteration a b) x

-- an infinite list of Jacobi approximations to the solution of "Ax = b"

-------------

jacobi_to_tolerance
  :: (Ord c, Ix a, Num b)
  => ((Array a b) -> c) -> c -> [Array a b] -> Maybe (Array a b)

jacobi_to_tolerance norm tol [] = Nothing
jacobi_to_tolerance norm tol (x:r@(x':_)) = 
  if norm (vector_sub x x') < tol
  then Just x'
  else jacobi_to_tolerance norm tol r

-- perform Jacobi iteration until the result is within the required
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

  jacobi_iteration a b x 

should give :-

[ 5.9999999999999998e-1
, 2 =: 2.2727272727272729
, 3 =: -1.1000000000000001
, 4 =: 1.8750000000000000
]

  (jacobi_list a b x) !! 10

should give :-

[  1.0001185986914152
,  1.9997679470100354
, -9.9982814287447630e-1
,  9.9978597846005013e-1
]

-}

-- eof
