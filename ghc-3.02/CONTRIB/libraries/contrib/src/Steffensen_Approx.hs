-- $Id: Steffensen_Approx.hs,v 1.1 1998/02/03 12:51:22 simonm Exp $

module Steffensen_Approx where

import Maybe

-- Steffensen's method for finding roots of "f(x) = 0"

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920801
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

-------------

steffensen_approx 
  :: (Fractional a, Ord a, Num b)
  => (a -> a) -- function to approximate
  -> a        -- tolerance
  -> b        -- maximum number of iterations
  -> a        -- initial approximation
  -> Maybe a

steffensen_approx f t m p0 = loop m p0
  where
  loop 0 _ = Nothing
  loop n p = 
    if abs (p' - p) < t
    then Just p'
    else loop (n-1) p'
    where
      p1 = f p
      p2 = f p1
      p' = p - ((p1 - p)^2)/(p2 - 2*p1 + p)

-- B&F pp 64

-------------

steffensen :: (Fractional a) => (a -> a) -> a -> a

steffensen f p = p'
  where
    p1 = f p
    p2 = f p1
    p' = p - ((p1 - p)^2)/(p2 - 2*p1 + p)

-- the heart of Steffensens' algorithm

-------------

steffensen_approx_values :: (Fractional a) => (a -> a) -> a -> [a]

steffensen_approx_values f p0 = iterate (steffensen f) p0

-- generates an infinite list of approximations for _f_
-- this is useful as it allows n approximations to be generated
-- in a simple way i.e. just use _take_ to keep as many as are
-- considered interesting.

-------------


-- Sample equation is :-
--
--    x^3 + 4*x^2 - 10 = 0
-- 
-- taking the following, one of 5 different forms for the above
--
--  f x = sqrt (10/(4+x))
--
-- converges to 1.3652300134165856 in 3 iterations
-- Any more iterations will probably cause div. by 0 error.
