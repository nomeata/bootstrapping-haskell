-- $Id: Adams_Predictor_Corrector.hs,v 1.1 1998/02/03 12:50:54 simonm Exp $

module Adams_Predictor_Corrector_Approx where

import Adams_Bashforth_Approx
import Runge_Kutta_Approx

-- Adams-Predictor-Corrector methods for solving second order
-- differential equations.  These are multi-step methods.

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920803
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

-- K&C - Numerical Analysis - David Kincaid & Ward Cheney - Brooks/Cole 1991

-------------

adams_predictor_corrector_4 f h (t,w0,w1,w2,w3) = (t',w1',w2',w3',w'')
  where
    (t',w1',w2',w3',w') = adams_bashforth_4 f h (t,w0,w1,w2,w3)
    w'' = w3 + h*(9*(f t' w') + 19*(f t3' w3) - 5*(f t2' w2) + (f t1' w1))/24
    t3' = t'  - h
    t2' = t3' - h
    t1' = t2' - h

-- Uses a fourth order Runge-Kutta method to derive initial values
-- then uses a fourth order Adams-Bashforth method to generate a 
-- tentative value for w'' (w') which is then fed into a fourth order
-- Adams-Moulton method which generates the correct value for w''

-- B&F 245

-------------

-- a fifth order version should go here.

-- K&C pp 511

-------------

adams_predictor_corrector_4_list f h t0 w0 = 
  iterate (adams_predictor_corrector_4 f h) (t3,w0,w1,w2,w3)
  where
    (t1,w1) = runge_kutta_4 f h (t0,w0)
    (t2,w2) = runge_kutta_4 f h (t1,w1)
    (t3,w3) = runge_kutta_4 f h (t2,w2)
   
adams_predictor_corrector_4_ans (_, _, _, _, p) = p

-------------

{-
An example.
Given the following :-

  y' t y = -y + t + 1     0 <= t <= 1    y(0) = 1

then the result of :-

  map adams_predictor_corrector_4_ans 
     (take 8 (adams_predictor_corrector_4_list y' 0.1 0 1))

should be :-

[ 1.0408184220011778
, 1.0703199182439460
, 1.1065302684102829
, 1.1488110325540919
, 1.1965845313758254
, 1.2493280604478492
, 1.3065686567931420
, 1.3678783660237561
]

-}

-- eof
