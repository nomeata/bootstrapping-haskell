module Newton_Interp_Div_Diff where

import Array -- 1.3
infix 1 =:
(=:) a b = (a,b)

-- Newton's interpolatory divided-difference formula

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920802
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

-------------

newton_forward_divided_difference xs q x = pnx
  where
    (0,n) = bounds xs
    pnx   = fst (foldl f (q!(0,0),1) [1..n])
    f (t,c) i = (t',c')
      where
        t' = t + q!(i,i) * c'
        c' = c * (x - xs!(i-1))

-- _xs_ = positions at which to calculate the polynomial coefficients.
-- _q_ = result of "newton_interpolatory_divided_difference_coefficients"
-- _x_ the value for which the value of the polynomial is required.

-------------

newton_interpolatory_divided_difference_coefficients
  :: (Fractional a)
  => (a -> a)            -- function
  -> Array Int a         -- coefficients
  -> Array (Int, Int) a

newton_interpolatory_divided_difference_coefficients f cs = q
  where
    (0,n) = bounds cs
    g i 0 = f (cs!i)
    g i j = (q!(i,j-1) - q!(i-1,j-1))/(cs!i - cs!(i-j))
    qb = ((0,0),(n,n))
    q  = array qb [ (i,j) =: g i j | i <- [0..n], j <- [0..i]]

-- q!(i,i) is f[x0, x1, ..., xn]
--
-- A slightly different interface than the more direct version below.
-- Also might be more efficient.

-------------

newton_interpolatory_divided_difference_coefficients_direct f coeffs = q
  where
    x' = listArray (0, n) coeffs
    q  = array ((0,0), (n, n))
      ((map (\ (i,v) -> (i, 0) =: f v) (zip [0..n] coeffs)) ++
      [ (i,j) =: (q!(i,j-1) - q!(i-1,j-1))/(x'!i - x'!(i-j))
      | i <- [1..n], j <- [1..i]
      ])
    n = (length coeffs) -1

-- Direct implementation of algorithm as given in B&F pp 102

-------------


nidd_diag x = ixmap (l,u) (\i -> (i,i)) x
  where ((l,l'),(u,u')) | l == l' && u == u' = bounds x

nidd_diag2list q =
  case bounds q of 
    ((a,b), (c,d)) | a == b && c == d -> [q!(i,i) | i <- [a..d]]
    _ -> []

-------------

nidd_test :: (Fractional a) => a -> (a , [a])
nidd_test x = (pn, nidd_diag2list qs)
  where
    f 1.0 = 0.7651977
    f 1.3 = 0.6200860
    f 1.6 = 0.4554022
    f 1.9 = 0.2818186
    f 2.2 = 0.1103623
    cs = listArray (0,4) [1.0, 1.3, 1.6, 1.9, 2.2]
    qs = newton_interpolatory_divided_difference_coefficients f cs
    pn = newton_forward_divided_difference cs qs x

-- a test for the routine.  The above should produce the following output :-

{-
  (nidd_test 1.5)

should produce :-

( 5.1181999423868330e-1
, [ 7.6519769999999998e-1
  , -4.8370566666666642e-1
  , -1.0873388888888935e-1
  , 6.5878395061728337e-2
  , 1.8251028806604353e-3
  ]
)

-}
