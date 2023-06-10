-- $Id: Adams_Bashforth_Approx.hs,v 1.1 1998/02/03 12:50:53 simonm Exp $

module Adams_Bashforth_Approx where

-- Adams-Bashforth methods for solving second order differential equations.
-- These are multi-step methods.

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920802
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

-------------

adams_bashforth_2 f h (t,w0,w1) = (t',w1,w')
  where
    t'  = t + h
    w'  = w + h/2*(3*(f t w) - (f t_1 w_1))
    w   = w1
    w_1 = w0
    t_1 = t - h

-- Adams-Bashforth Two-Step Method - B&F pp 241
-- this version trades re-calculation of t_X against passing them as values

-------------

adams_bashforth_3 f h (t,w0,w1,w2) = (t',w1,w2,w')
  where
    t'  = t + h
    w'  = w + h/12*(23*(f t w) - 16*(f t_1 w_1) + 5*(f t_2 w_2))
    w   = w2
    w_1 = w1
    w_2 = w0
    t_1 = t   - h
    t_2 = t_1 - h

-- Adams-Bashforth Three-Step Method - B&F pp 242
-- this version trades re-calculation of t_X against passing them as values

--------------

adams_bashforth_4 f h (t,w0,w1,w2,w3) = (t',w1,w2,w3,w')
  where
    t' = t + h
    w' = w + h/24*(55*(f t w) -59*(f t_1 w_1) + 37*(f t_2 w_2) -9*(f t_3 w_3))
    w  = w3
    w_1 = w2
    w_2 = w1
    w_3 = w0
    t_1 = t   - h
    t_2 = t_1 - h
    t_3 = t_2 - h

-- Adams-Bashforth Four-Step Method - B&F pp 242
-- this version trades re-calculation of t_X against passing them as values

-------------


adams_bashforth_2_list f h t w0 w1 =
   iterate (adams_bashforth_2 f h) (t, w0, w1)

adams_bashforth_3_list f h t w0 w1 w2 =
   iterate (adams_bashforth_3 f h) (t, w0, w1, w2)

adams_bashforth_4_list f h t w0 w1 w2 w3 =
   iterate (adams_bashforth_4 f h) (t, w0, w1, w2, w3)

adams_bashforth_2_ans (_, _, p)       = p
adams_bashforth_3_ans (_, _, _, p)    = p
adams_bashforth_4_ans (_, _, _, _, p) = p

-- An example :-
--
--   y' t y = -y + t + 1
--
-- Taking the initial conditions from the exact values of :-
--
--   y t = exp(-t) + t
-- 
-- > let as = (adams_bashforth_4_list y' 0.1 0.3 (y 0) (y 0.1) (y 0.2) (y 0.3))
-- > in adams_bashforth_4_ans (as !! 7)
-- 1.3678899579570314

-- eof
