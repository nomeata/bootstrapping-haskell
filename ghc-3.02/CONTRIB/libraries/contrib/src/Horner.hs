-- $Id: Horner.hs,v 1.1 1998/02/03 12:51:03 simonm Exp $

module Horner where

-- Horner's algorithm for evaluating a polynomial of the form :-
--
--   P(x) = a_n*x^n + a_(n-1)*x^(n-1) + ... + a_1*x + a_0
--
-- and its derivative at x0

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920802
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

-------------

horner :: (Num a)
       => [a]       -- coefficients (ordering is n down to 0)
       -> a         -- value to evaluate polynomial and derivative at
       -> (a, a)    -- (p x0, p' x0)

horner [] _ = error "missing coefficients"
horner (a:r) x0 = loop a a r
  where
    loop y z []    = error "missing coefficients"
    loop y z [a]   = (y*x0 + a, z)
    loop y z (a:r) = loop y' z' r
      where y' = (x0*y + a)
            z' = (x0*z + y')

-- B&F pp 68-69

--------------

-- Sample equation :-
--
--   f  x = 2*x^4 - 3*x^2 + 3*x -4
--
-- > horner [2,0,-3,3,-4] (-2)
-- (10, -49)

-- eof
