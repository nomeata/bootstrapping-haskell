-- $Id: Fixed_Point_Approx.hs,v 1.1 1998/02/03 12:50:59 simonm Exp $

module Fixed_Point_Approx where

import Maybe

-- Fixed-point method for finding roots of "f(x) = 0"

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920801
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

-------------

fixed_point_approx_t
   :: (Ord a, Num a, Num b)
   => (a -> a)  -- function to approximate
   -> a         -- tolerance
   -> b         -- maximum number of iterations
   -> a         -- initial approximation
   -> Maybe a

fixed_point_approx_t f t m p0 = loop m p0
  where
    loop 0 _ = Nothing
    loop n p0 =
      if p - p0 < t
      then Just p
      else loop (n-1) p
      where p = f p0

-- B&F 36

-------------


fixed_point_approx_values f p0 = iterate f p0

-- generates an infinite list of approximations for _f_
-- this is useful as it allows n approximations to be generated
-- in a simple way i.e. just use _take_ to keep as many as are
-- considered interesting.

-------------

-- Sample equation is :-
--
--    x^3 + 4*x^2 - 10 = 0
--
-- this can be rewritten in 5 different ways to get it into the form x = g(x)
--
--   f1 x = x - x^3 - 4*x^2 + 10
--   f2 x = sqrt ((10/x - 4*x) :+ 0)
--   f3 x = (sqrt (10 - x^3))/2
--   f4 x = sqrt (10/(4+x))
--   f5 x = x - (x^3+4*x^2-10)/(3*x^2+8*x)
--
-- 1. diverges badly (way off in 4 iterations)
-- 2. diverges badly (goes complex in 4 iterations)
-- 3. converges around 1.365230013 in 30 iterations
-- 4. converges on the above in about 10 iterations
-- 5. converges on the above in about 4 iterations

-- B&F claim that this method is better than the Bisection Method
-- as it converges quicker.

-------------
