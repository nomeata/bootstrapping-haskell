-- $Id: Vector_Ops.hs,v 1.1 1998/02/03 12:51:24 simonm Exp $

module Vector_Ops where

import Ix    -- 1.3
import Array -- 1.3

-- Misc. operations on vectors

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920804
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

-- K&C - Numerical Analysis - David Kincaid & Ward Cheney - Brooks/Cole 1991

-------------

vector_1_norm :: (Ix a, Num b) => Array a b -> b
vector_1_norm v = sum [ abs (v!i) | i <- (range . bounds) v ]

-- B&F pp 421

--------------

vector_2_norm :: (Ix a, Floating b) => (Array a b) -> b
vector_2_norm v = sqrt (sum [ (v!i)^2 | i <- (range . bounds) v])

--euclidean_vector_norm = vector_2_norm

-- B&F pp 409

--------------

vector_inf_norm v = maximum [ abs (v!i) | i <- (range . bounds) v]

-- K&C pp 164, B&F pp 410
-- "inf" meant to represent the suffix "infinity"

--------------

scalar_x_vector s v = [ s*(v!i) | i <- (range . bounds) v]

-------------

vector_sub v v' = array bds [ (i, v!i - v'!i) | i <- range bds ]
  where bds = bounds v

-- should factor out the "-" and define a "zip" equivalent.

-------------
