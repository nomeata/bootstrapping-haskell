-- $Id: Hermite_Interpolation.hs,v 1.1 1998/02/03 12:51:02 simonm Exp $

module Hermite_Interpolation where

import Array -- 1.3
infix 1 =:
(=:) a b = (a,b)

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920805
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

---------------

hermite_approx xs q x = snd ap
  where
    (0,n) = bounds xs
    ap = foldl f (1,q!(0,0)) [1..2*n+1]
    f (c,t) i = (c',t')
      where
        t' = t + q!(i,i) * c'
        c' = c * v
        v  = (x - xs!(div (i-1) 2))

-- calculates the value of the Hermite polynomial over distinct values
-- _xs_ and with coefficients on the diagonal of _q_ at the position _x_

--------------

hermite_coefficients
  :: (Fractional a)
  => (a -> a)
  -> (a -> a)
  -> (Array Int a)
  -> Array (Int, Int) a

hermite_coefficients f f' x = q
  where
    q = array bds (g00:g10:g11:elems)
    elems = [ (i,j) =: g i j | i <- [2..2*n+1], j <- [0..i]]
    g00   = (0,0) =: f  (x!0)
    g10   = (1,0) =: f  (x!0)
    g11   = (1,1) =: f' (x!0)
    g i 0 = f (z i)
    g i 1 | odd i     = f' (z i)
          | otherwise = h i
    g i j = (q!(i,j-1) - q!(i-1,j-1))/((z i) - (z (i-j)))
    h i = (q!(i,0) - q!(i-1,0))/((z i) - (z (i-1)))
    z i = x!(div i 2)
    (0,n) = bounds x
    bds = ((0,0), (2*n+1,2*n+1))

-- Generates the coefficients of the Hermite interpolating polynomial
-- given the function _f_ its derivative _f'_ and distinct values _x_
-- using a Newton interpolatory divided difference formula.

--------------

hermite_coefficients_direct
  :: (Fractional a)
  => (a -> a)
  -> (a -> a)
  -> (Array Int a)
  -> Array (Int, Int) a

hermite_coefficients_direct f f' x = q'
  where
    q' = q//[ (i, j) =: g i j | i <- [2..2*n+1], j <- [2..i]]
    g i j = (q'!(i,j-1) - q'!(i-1,j-1))/((z i) - (z (i-j)))
    z i = x!(div i 2)
    q = foldl h q0 [1..n]
    q0 = array bds [ (0,0) =: f (x!0), (1,0) =: f (x!0), (1,1) =: f' (x!0)]
    h q i = q'
      where
        q' = q//assocs
        assocs = [ (2*(i::Int), 0) =: f (x!i) -- the ::Int is to placate HBC
                 , (2*i+1, 0) =: f (x!i)
                 , (2*i+1, 1) =: f' (x!i)
                 , (2*i, 1) =: (f (x!i) - q!(2*i-1,0))/(x!i - x!(i-1))
                 ]
    (0,n) = bounds x
    bds = ((0,0), (2*n+1,2*n+1))

-- As above, but a more direct translation of the algorithm given in
-- B&F pp 114.

-------------

hi_test :: (Fractional a) => a -> (a , [a])
hi_test x = (pn, hi_diag2list qs)
  where
    f  1.3 = 0.6200860
    f  1.6 = 0.4554022
    f  1.9 = 0.2818186
    f' 1.3 = -0.5220232
    f' 1.6 = -0.5698959
    f' 1.9 = -0.5811571
    cs = listArray (0,2) [1.3, 1.6, 1.9]
    qs = hermite_coefficients f f' cs
    pn = hermite_approx cs qs x

hi_diag2list q =
  case bounds q of 
    ((a,b), (c,d)) | a == b && c == d -> [q!(i,i) | i <- [a..d]]
    _ -> []

{-

The result of 

  hi_test 1.5

should be :-

( 5.1182770172839509e-1,
, [ 6.2008600000000003e-1
  , -5.2202320000000002e-1
  , -8.9742666666666734e-2
  , 6.6365555555556163e-2
  , 2.6666666666621641e-3
  , -2.7746913579894407e-3
  ]
)

-}
