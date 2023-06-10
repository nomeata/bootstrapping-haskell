-- $Id: Newton_Raphson_Approx.hs,v 1.1 1998/02/03 12:51:14 simonm Exp $

module Newton_Raphson_Approx where

import Maybe

-- Newton-Raphson method for finding roots of "f(x) = 0"

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920802
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

---------------

newton_raphson f f' i = i - (f i) / (f' i)

-- the heart of the Newton-Raphson method

-------------

newton_raphson_approx_t
  :: (Fractional a, Ord a, Integral b)
  => (a -> a)  -- function
  -> (a -> a)  -- derivative
  -> a         -- tolerance
  -> b         -- maximun number of iterations to try
  -> a         -- initial approximation
  -> Maybe a

newton_raphson_approx_t f f' t m i = loop m i
  where
  loop 0 i = Nothing
  loop n i = if abs(i' - i) < t
             then Just i'
             else loop (n-1) i'
             where i' = newton_raphson f f' i

-- B&F pp 42-43
-- a traditional "loopy" definition of the Newton-Raphson method

---------------

newton_raphson_approx_values
  :: (Fractional a)
  => (a -> a)  -- function
  -> (a -> a)  -- first derivative
  -> a         -- initial value
  -> [a]

newton_raphson_approx_values f f' = iterate (newton_raphson f f')

-- generates an infinite list of approximations for _f_ and _f'_
-- this is useful as it allows n approximations to be generated
-- in a simple way i.e. just use _take_ to keep as many as are
-- considered interesting.

-------------


newton_raphson_approx_control
  :: (Integral a, Ord b, Num b)
  => a        -- maximum number of approximations to check
  -> b        -- tolerance
  -> [b]      -- approximations
  -> Maybe b

newton_raphson_approx_control _ _ []  = Nothing
newton_raphson_approx_control 0 _ _   = Nothing
newton_raphson_approx_control n t (x:y:r) =
  if abs(y - x) < t
  then Just y
  else newton_raphson_approx_control (n-1) t (y:r)


newton_raphson_approx
  :: (Fractional a, Ord a, Integral b)
  => (a -> a)  -- function
  -> (a -> a)  -- derivative
  -> a         -- tolerance
  -> b         -- maximun number of iterations to try
  -> a         -- initial approximation
  -> Maybe a

newton_raphson_approx f f' t m i =
  newton_raphson_approx_control m t (newton_raphson_approx_values f f' i)


---------------

-- Sample function :-
--
--   f  x = cos x - x
--   f' x = (- sin x) - 1
--
-- With an initial value of pi/4 this should reach 0.79390851332 in about
-- 5 iterations
