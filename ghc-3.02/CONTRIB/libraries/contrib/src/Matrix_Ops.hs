-- $Id: Matrix_Ops.hs,v 1.1 1998/02/03 12:51:09 simonm Exp $

module Matrix_Ops where

import Ix    -- 1.3
import Array -- 1.3

-- Misc. operations on matrices

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920804
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

-- K&C - Numerical Analysis - David Kincaid & Ward Cheney - Brooks/Cole 1991

-------------

matrix_inf_norm :: (Ix a, Ord b, Num b, Enum a) => (Array (a, a) b) -> b
matrix_inf_norm a = min
  where
    min = maximum [sum [abs (a!(i,j)) | j <- [m..n]] | i <- [m..n]]
    ((m,_),(n,_))= bounds a


subordinate_matrix_norm
  :: (Ix a, Ord b, Num b, Enum a)
  => (Array (a, a) b) -> b

subordinate_matrix_norm = matrix_inf_norm

-- B&F pp 415, K&C 164

-------------

matrix_2_norm :: (Ix a, Ord b, Floating b, Enum a) => (Array (a, a) b) -> b

matrix_2_norm a = m2n
  where
    m2n = maximum [sqrt (sum [abs (a!(i,j)) | j <- [m..n]]) | i <- [m..n]]
    ((m,_),(n,_))= bounds a

-- B&F pp 415

-------------

sub_matrix
  :: (Ix a, Num a, Enum a)
  => (Array (a, a) b) -> a -> a -> Array (a, a) b

sub_matrix a i j = sm
  where
    sm = array nb [ ( (f k i, f l j), a!(k,l) )
                  | k <- [m..n], k /= i, l <- [m..n], l /= j
                  ]
    f a b = if a < b then a else a-1
    ((m,_),(n,_)) = bounds a
    nb = ((m,m),(n-1,n-1))

-- creates a (m-1)*(n-1) consisting of the matrix _a_ without row _i_
-- and column _j_

-------------

minor_matrix 
  :: (Ix a, Num a, Enum a)
  => ((Array (a, a) b) -> c) -> (Array (a, a) b) -> a -> a -> c

minor_matrix f a i j = f  (sub_matrix a i j)

-- Minor matrix is the determinant of the (n-1)*(n-1) submatrix of
-- an n*n matrix obtained by deleting the _i_th row and _j_th column.
-- _f_ is the function that calculates the determinant of a matrix.

-- B&F pp 320

-------------

-- partain: wrong
--matrix_cofactor
--  :: (Num c, Integral a)
--  => ((Array (a, a) b) -> c) -> (Array (a, a) b) -> a -> a -> c

matrix_cofactor f a i j = (minor_matrix f a i j)*(-1)^(i+j)

-- B&F pp 320

-------------

-- partain:wrong
--matrix_row_det
--  :: (Integral a, Num b)
--  => (Array (a, a) b) -> ((Array (a, a) b) -> a -> a -> a) -> b

matrix_row_det a g =
  if m == n
  then a!(m,m)
  else mrd
  where
    i   = g a m n
    mrd = sum [ f j | j <- [m..n]]
    f j = a!(i,j)*(matrix_cofactor (flip matrix_row_det g) a i j)
    ((m,_),(n,_)) = bounds a

-- Calculate the determinant of a matrix around a given row.
-- slightly unusual in that the second argument is a function that allows
-- the row for the sub-determinants to selected dynamically.
-- Is this flexibility required, or will the usual argument just be :-
--   (\ _ _ _ -> 1)
-- Would it be useful to be able to swap back and forth between calculating
-- by row/column for sub-determinants?

-- B&F pp 320

-------------

-- partain:wrong:
--matrix_col_det
--  :: (Integral a, Num b)
--  => (Array (a, a) b) -> ((Array (a, a) b) -> a -> a -> a) -> b

matrix_col_det a g =
  if m == n
  then a!(m,m)
  else mrd
  where
    i   = g a m n
    mrd = sum [ f j | j <- [m..n]]
    f j = a!(i,j)*(matrix_cofactor (flip matrix_col_det g) a i j)
    ((m,_),(n,_)) = bounds a

-- Calculate the determinant of a matrix around a given column.
-- arguments are as for "matrix_row_det"

--------------
