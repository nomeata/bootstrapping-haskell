-- $Id: Crout_Reduction.hs,v 1.1 1998/02/03 12:50:57 simonm Exp $

module Crout_Reduction where
import Array --1.3
infix 1 =:
(=:) a b = (a,b)


-- Crout reduction for tridiagnonal linear systems

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920805
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden and J. Douglas Faires
-- Prindle, Weber & Schmidt 1985

-------------

crout_reduction :: (Fractional b) => (Array (Int, Int) b) -> Array Int b

crout_reduction a = xs
  where
    ((m,_),(n,_)) = bounds a
    lu = lu_decomp a m n
    z  = solve_lzb a m n lu
    xs = solve_uzx m n lu z

-- Solves an n*n tridiagnonal linear system using Crout Reduction.
-- It assumes that there is a unique solution.
-- B&F pp 354

-- the _l_ and _u_ matrices in the description in B&F have been merged
-- into one here.  This was done in an attempt to reduce the overhead of
-- the non-monolithic array creation.

---------------

solve_lzb a m n lu = z
  where
    z1 = m =: a!(m,n+1)/a!(m,m)
    z = array (m,n) (z1:[ i =: f i | i <- [m+1..n]])
    f i = (a!(i,n+1) - lu!(i,i-1)*z!(i-1))/lu!(i,i)

-------------

solve_uzx m n lu z = x
  where
    xn = n =: z!n
    x = array (m,n) (xn:[ i =: f i | i <- reverse [m..n-1]])
    f i = z!i - lu!(i,i+1)*x!(i+1)

-------------

lu_decomp a m n = lu
  where
    ab = ((m,m), (n,n))
    lu = array ab (lu_elems lun)
    lu_elems = foldl f lu0 [m+1..n-1]
    lu0 v = ((m,m) =: a!(m,m)):(((m,m+1) =: a!(m,m+1)/a!(m,m)):v)
    lun = [(n,n-1) =: a!(n,n-1), (n,n) =: a!(n,n) - a!(n,n-1)*lu!(n-1,n)]
    f le i = le'
      where
        li1 = a!(i,i-1)
        li2 = a!(i,i) - li1*lu!(i-1,i)
        le' v = le (((i,i-1) =: li1):((i,i) =: li2):((i,i+1) =: a!(i,i+1)/li2):v)

------------

lu_decomp_direct a m n = lu
  where
    ab = ((m,m), (n,n))
    lu = lu1//[(n,n-1) =: a!(n,n-1), (n,n) =: a!(n,n) - a!(n,n-1)*lu!(n-1,n)]
    lu1 = foldl f lu0 [m+1..n-1]
    lu0  = array ab [(m,m) =: a!(m,m), (m,m+1) =: a!(m,m+1)/a!(m,m)]
    f lu i = lu'
      where
        li1 = a!(i,i-1)
        li2 = a!(i,i) - li1*lu!(i-1,i)
        lu' = lu//[(i,i-1) =: li1, (i,i) =: li2, (i,i+1) =: a!(i,i+1)/li2]

-- a "direct" translation of the decomposition section in B&F

-------------

{-
An example :-

      [  2 -1  0  0 | 1 ]
  A = [ -1  2 -1  0 | 0 ]
      [  0 -1  2 -1 | 0 ]
      [  0  0 -1  2 | 1 ]

should give the answer [1,1,1,1]

i.e. given :-

  a = listArray ((1,1),(4,5)) [2,-1,0,0,1,-1,2,-1,0,0,0,-1,2,-1,0,0,0,-1,2,1]

then :-

  crout_reduction a

should produce :-

[ 9.9999999999999989e-1
, 9.9999999999999978e-1
, 9.9999999999999989e-1
, 1.0000000000000000
]

-}
