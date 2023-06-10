-- $Id: Newton_Cotes.hs,v 1.1 1998/02/03 12:51:12 simonm Exp $

module Newton_Cotes where

-- Newton-Cotes formulas for approximating an integral

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920802
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

-------------

-- Some Closed Formulas

-------------

newton_cotes_closed_1 f h x0 x1 = h*((f x0) + (f x1))/2
newton_cotes_trapezoidal = newton_cotes_closed_1

-- Trapezoidal Rule 
-- B&F pp 159

-------------

newton_cotes_closed_2 f h x0 x1 x2 = h*((f x0) + 4*(f x1) + (f x2))/2
newton_cotes_simpson = newton_cotes_closed_2

-- Simpson's Rule
-- B&F pp 159

-------------

newton_cotes_closed_3 f h x0 x1 x2 x3 =
  3*h((f x0) + 3*(f x1) + 3*(f x2) + (f x3))/8
newton_cotes_simpson_3_8 = newton_cotes_closed_3

-- Simpson's Three-Eighths Rule
-- B&F pp 159

-------------

newton_cotes_closed_4 f h x0 x1 x2 x3 x4 =
  2*h*(7*(f x0) + 32*(f x1) + 12*(f x2) + 32*(f x3) + 7*(f x4))/25

-------------

-- Some open formulas

-------------

newton_cotes_open_0 f h x0 = 2*h*(f x0)
newton_cotes_midpoint = newton_cotes_open_0

-- Midpoint Rule 
-- B&F pp 160

-------------

newton_cotes_open_1 f h x0 x1 = 3*h*((f x0) + (f x1))/2

-------------

newton_cotes_open_2 f h x0 x1 x2 = 4*h*(2*(f x0) - (f x1) + 2*(f x2))/3

-------------

newton_cotes_open_3 f h x0 x1 x2 x3 =
  5*h*(11*(f x0) + (f x1) + (f x2) + 11*(f x3))/24

-------------
