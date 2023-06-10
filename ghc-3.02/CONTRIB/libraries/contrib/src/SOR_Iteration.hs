-- $Id: SOR_Iteration.hs,v 1.1 1998/02/03 12:51:18 simonm Exp $

module SOR_Iteration where

-- Perform SOR (Successive Over-Relaxation) Iteration to solve "Ax = b"

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

import Array -- 1.3
import Maybe
import Vector_Ops(vector_sub)

-------------

sor_iteration a b w x = x'
  where
    x' = array bs [ (i, g i) | i <- range bs ]
    g i = (1 - w)*x!i + w*(b!i - (f i x' m (i-1)) - (f i x (i+1) n))/a!(i,i)
    f i p l u = sum [ (p!j)*a!(i,j) | j <- [l..u]]
    bs@(m,n) = bounds x

-- The heart of the SOR method.  This produces one iteration of the
-- SOR method.
-- B&F pp 435, K&C p192

-------------

sor_list a b w x = iterate (sor_iteration a b w) x

-- an infinite list of SOR approximations to the solution of "Ax = b"

-------------

sor_to_tolerance norm tol [] = Nothing
sor_to_tolerance norm tol (x:r@(x':_)) = 
  if norm (vector_sub x x') < tol
  then Just x'
  else sor_to_tolerance norm tol r

-- perform SOR iteration until the result is within the required
-- tolerance _tol_ using _norm_ as the vector normaliser

-------------

{-
An example.

    [ 4  3  0 ]       [  24 ]         [ 1 ]
A = [ 3  4  4 ]   b = [  30 ]    x0 = [ 1 ]   w = 1.25
    [ 0 -1  4 ]       [ -24 ]         [ 1 ]


This converts into :-

  a = listArray ((1,1),(3,3)) [4,3,0,3,4,-1,0,-1,4]
  b = listArray (1,3) [24,30,-24]
  x = listArray (1,3) [1,1,1]

and a one step evalution :-

  sor_iteration a b 1.25 x 

should give :-

[6.3125000000000000, 3.5195312500000000, -6.6501464843750000]


  (sor_list a b x) !! 10

should give :-

[3.0000498036721481, 4.0002585779309898, -5.0003486480130794]

To find the values to a given tolerance :-

  sor_to_tolerance vector_inf_norm 4e-4 (sor_list a b 1.25 x)

[3.0000024592678818, 4.0000149781919498, 5.0000222146211835]

-}

-- eof
