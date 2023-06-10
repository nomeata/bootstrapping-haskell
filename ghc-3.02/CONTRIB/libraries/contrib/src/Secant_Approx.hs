-- $Id: Secant_Approx.hs,v 1.1 1998/02/03 12:51:19 simonm Exp $

module Secant_Approx where

import Maybe

-- Secant method of finding roots of "f(x) = 0"

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920801
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

-------------

secant_approx
  :: (Fractional a, Ord a, Num b)
  => (a -> a)   -- function to approximate
  -> a          -- tolerance
  -> b          -- maximum number of iterations
  -> a          -- lower initial approx
  -> a          -- upper initial approx
  -> Maybe a

secant_approx f t m ia ib = loop (m-1) ia ib (f ia) (f ib)
  where
  loop n ia ib qa qb =
    if abs (i - ib) < t
    then Just i
    else loop (n-1) ib i qb (f i)
    where
      qa = f ia
      qb = f ib
      i = ib - qb * (ib - ia) / (qb - qa)

-- B&F pp 47

-------------
