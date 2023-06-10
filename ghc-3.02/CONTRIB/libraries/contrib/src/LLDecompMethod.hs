module LLDecompMethod(lldecomp, llsolution) where

import Array -- 1.3

-- LL, i.e. Choleski decomposition method for solution
-- of linear equations A x = b.
-- 		by Junxian Liu <jjl@doc.ic.ac.uk>

lldecomp :: Array (Int,Int) Double -> Array (Int,Int) Double
lldecomp a
   = l
     where
     l = makearray (bounds a) f
     f (i,j) | (i < j)  = 0.0
             | (i == j) = sqrt(a!(i,i) - 
                               sum[l!(i,k) * l!(i,k) | k <- [1..i-1]])
             | (i > j)  = (a!(i,j) - 
                           sum[l!(i,k) * l!(j,k) | k <- [1..j-1]])
                          / l!(j,j)
forward (l, b)
   = y
     where
     y   = makearray (bounds b) f
     f i = (b!i - sum[l!(i,j) * y!j | j <- [1..i-1]]) / l!(i,i)

backward (l, y)
   = x 
     where
     x   = makearray (1,n) f
     f i = (y!i - sum[l!(j,i) * x!j | j <- [i+1..n]]) / l!(i,i)
     (_, n) = bounds y

llsolution (a,b) 
   = backward (l, forward (l, b))
     where
     l = lldecomp a

makearray lub f 
   = array lub [ (i, f i) | i <- range lub ]

