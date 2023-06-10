-- $Id: Least_Squares_Fit.hs,v 1.1 1998/02/03 12:51:07 simonm Exp $

module Least_Squares_Fit where

import Array -- 1.3
infix 1 =:
(=:) a b = (a,b)

-- Calculate the gradient and intersection for a least squares best fit line
-- through a set of points.

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920805
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

--------------

naive_vector_least_squares_fit
  :: (Fractional a)
  => (Array Integer a)
  -> (Array Integer a)
  -> (a, a)             -- (gradient, intersection)

naive_vector_least_squares_fit xs ys = (a, b)
  where
  a = ((fromInteger n)*p - q*r)/((fromInteger n)*s - q^2)
  b = (s*r - p*q)/(s*(fromInteger n) - q^2)
  p = sum [ xs!i*ys!i | i <- inds ]
  q = sum [ xs!i | i <- inds ]
  r = sum [ ys!i | i <- inds ]
  s = sum [ (xs!i)^2 | i <- inds ]
  bs@(1,n) = bounds xs
  inds = range bs

-- An naive array based version.  Naive in the sense that it processes the
-- lists more times than is strictly necessary.
-- B&F pp 364

-------------

vector_least_squares_fit
  :: (Fractional a) 
  => (Array Integer a) -> (Array Integer a) -> (a, a)

vector_least_squares_fit xs ys = (a, b)
  where
    a = ((fromInteger n)*xy_t - x_t*y_t)/((fromInteger n)*x2_t - x_t^2)
    b = (x2_t*y_t - xy_t*x_t)/(x2_t*(fromInteger n) - x_t^2)
    (x_t, y_t, xy_t, x2_t, _) = loop n (0, 0, 0, 0, 0)
    (1,n) = bounds xs
    loop 0 r = r
    loop i (x_t, y_t, xy_t, x2_t, n) =
      loop (i-1) (x_t', y_t', xy_t', x2_t', n')
      where
        x      = xs!i
        y      = ys!i
        x_t'   = x_t + x
        y_t'   = y_t + y
        xy_t'  = xy_t + x*y
        x2_t'  = x2_t + x^2
        n'     = n + 1

-- Performs a least squares fit on the two vectors of numbers.
-- Returns (gradient, y-intersection)
-- Assumes that the two vectors are the same length.

--------------

list_least_squares_fit :: (Fractional a) => [a] -> [a] -> (a, a)

list_least_squares_fit xs ys = (a, b)
  where
    a = ((fromInteger n)*xy_t - x_t*y_t)/((fromInteger n)*x2_t - x_t^2)
    b = (x2_t*y_t - xy_t*x_t)/(x2_t*(fromInteger n) - x_t^2)
    (x_t, y_t, xy_t, x2_t, n) = foldl f (0, 0, 0, 0, 0) (zip xs ys)
    f (x_t, y_t, xy_t, x2_t, n) (x,y) = (x_t', y_t', xy_t', x2_t', n')
      where
        x_t'   = x_t + x
        y_t'   = y_t + y
        xy_t'  = xy_t + x*y
        x2_t'  = x2_t + x^2
        n'     = n + 1

-- Performs a least squares fit on the two lists of numbers.
-- Returns (gradient, y-intersection)
-- Assumes that the two lists are the same length.

-------------

{-
An example :-

  xs = listArray (1,11) [1..11]
  ys = listArray (1,11) [1.3,3.5,4.2,5.0,7.0,8.8,10.1,12.5,13.0,15.6,16.1]

  naive_vector_least_squares xs ys

should produce :-

  (1.5172727272727293, -2.7636363636364358e-1)

-}
