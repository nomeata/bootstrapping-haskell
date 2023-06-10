-- $Id: Neville_Iter_Interp.hs,v 1.1 1998/02/03 12:51:10 simonm Exp $

module Neville_Iterated_Interpolation where

import Ix    -- 1.3
import Array -- 1.3

-- Neville's Iterated Interpolation algorithm
-- This evaluates an interpolating polynomial P on the (n+1) distinct
-- numbers x0, ..., xn at the number _x_ for a given function _f_

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920802
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

-------------

neville_iterated_interpolation
   :: (Fractional a)
   => (a -> a)
   -> Array Int a
   -> a
   -> Array (Int, Int) a

neville_iterated_interpolation f cs x = q
  where
    (0,n) = bounds cs
    qb = ((0,0),(n,n))
    g (i, 0) = f (cs!i)
    g (i, j) = ((x - cs!(i-j)) * q!(i, j-1) - (x - cs!i) * q!(i-1,j-1))/(cs!i - cs!(i-j))
    q = array qb [ ((i,j), g (i,j)) | i <- [0..n], j <- [0..i]]

-- B&F pp 97

-------------

nii_bl :: (Ix a, Enum a) => (Array (a, a) b) -> [[b]]
nii_bl q = 
  case bounds q of
    ((a,b), (c,d)) -> [[q!(y,x) | x <- [b..y]] | y <- [a..c]]
    _ -> []

-- returns the bottom left of an square array as a list of lists.
-- if the array is not square, it returns []

-------------

nii_test :: (Fractional a) => [[a]]
nii_test = nii_bl (neville_iterated_interpolation f cs 1.5)
  where
    f 1.0 = 0.7651977
    f 1.3 = 0.6200860
    f 1.6 = 0.4554022
    f 1.9 = 0.2818186
    f 2.2 = 0.1103623
    cs = listArray (0,4) [1.0, 1.3, 1.6, 1.9, 2.2]

-- Test function for Neville's method.  All being well, it should 
-- produce the following output :-

{-

[ [7.6519769999999998e-1]
, [6.2008600000000003e-1, 5.2334486666666669e-1]
, [4.5540219999999998e-1, 5.1029679999999999e-1, 5.1247147777777780e-1]
, [2.8181859999999997e-1, 5.1326340000000004e-1, 5.1128566666666664e-1, 5.1181269382716044e-1]
, [1.1036230000000000e-1, 5.1042699999999974e-1, 5.1373613333333346e-1, 5.1183021481481483e-1, 5.1181999423868307e-1]
]

-}
