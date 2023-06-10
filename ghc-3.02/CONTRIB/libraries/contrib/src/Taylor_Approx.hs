-- $Id: Taylor_Approx.hs,v 1.1 1998/02/03 12:51:23 simonm Exp $

module Taylor_Approx where

-- Taylor methods for solving second-order differential equations

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920802
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

------------

euler f h (t, w) = (t', w')
  where
    t' = t + h
    w' = w + h*(f t w)

-- B&F pp 206
-- this is a first order Taylor approximation

------------

taylor_2 f f' h (t, w) = (t', w')
  where
    t' = t + h
    w' = w + (f t w) + (h*(f' t w))/2

-- B&F pp 216

-------------

taylor1_approx_values m f h t w0 = iterate (m f h) (t, w0)

-- generate an infinite list of solutions using the solution method _m_
-- for equation _f_ using: _h_ as the interval size, _t_ as the interval 
-- lower bound and w as the initial approximation.

-------------

-- Some sample equations to solve :-
--
--   y' t y = -y + t^2 + 1
--
-- Some examples :-
--
-- > snd ((taylor1_approx_values euler y' 0.1 0 1) !! 10)
-- 1.2375109638099997
--

-- eof
