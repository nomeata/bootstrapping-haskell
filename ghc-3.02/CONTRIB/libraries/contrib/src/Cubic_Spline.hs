-- $Id: Cubic_Spline.hs,v 1.1 1998/02/03 12:50:58 simonm Exp $

module Cubic_Spline where

import Array -- 1.3
infix 1 =:
(=:) a b = (a,b)

-- Cubic spline interpolants

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920806
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden and J. Douglas Faires
-- Prindle, Weber & Schmidt 1985

--------------

cubic_spline_approx f b c d xs j x =
  (f j) + b!j*(x - xs!j) + c!j*(x - xs!j)^2 + d!j*(x - xs!j)^3

-- calculate S(x) for any given x in the range [j..j+1] and where j
-- is in the range [0..n-1] where (0,n) = bounds xs
-- b, c, d are the coefficients returned by 
-- "xxx_cubic_spline_coefficients" and _f_ is the function to be evaluated.
--
-- The above could easily be factored, but I'm assuming that the
-- compiler can spot them :-)

--------------

clamped_cubic_spline_coefficients
  :: (Fractional a)
  => (Int -> a)
  -> a
  -> a
  -> (Array Int a)
  -> (Array Int a, Array Int a, Array Int a)

clamped_cubic_spline_coefficients f fpo fpn xs = (b, c, d)
  where
    (cs,bs,ds) = cbd_vectors c b d h f z u n  --     c
    c = array (0,n) ((n =: znv):(cs []))      --   r   i
    b = array (0,n-1) (bs [])                 -- a       r
    d = array (0,n-1) (ds [])                 --   l   c
                                              --     u
    znv = (al!n - h!(n-1)*z!(n-1))/lnv
      where lnv = h!(n-1)*(2 - u!(n-1))              --     c
    u = array (0,n-1) ((0 =: 0.5):(us []))           --   r   i
    z = array bds ((0 =: al0v/l0v):(zs [n =: znv]))  -- a       r
      where l0v = 2*h!0                              --   l   c
    (us,zs) = uz_vectors u z n xs h al               --     u 

    al = alpha_vector xs h f n al0
    al0v = 3*(f 1 - f 0)/(h!0 - 3*fpo)
    al0 = (\v -> (0 =: al0v):(n =: alnv): v)
      where alnv = 3*fpn - 3*((f n) - (f (n-1)))/h!(n-1)

    h = array (0,n-1) [i =: xs!(i+1) - xs!i | i <- [0..n-1]]
    -- could calc. h on the fly instead of storing it.
    bds@(0,n) = bounds xs


-- Constructs the coefficients for a clamped cubic spline interpolant S
-- of the function _f_ defined at the distinct points _xs_.
-- _fpo_ is the value of the derivative of _f_ at 0.
-- _fpn_ is the value of the derivative of _f_ at n, where n is the upper
-- bound of the array of data _xs_.

-- It uses Crout reduction to solve the tridiagonal linear system that pops
-- up during the course of the problem.
-- B&F pp 124

-- In an attempt to avoid using // to update arrays an element at
-- a time, circular programming has been used (see marked areas)
-- No tests have been done to see if this really is quicker!
-- Comments on this would be appreciated.

-------------

natural_cubic_spline_coefficients f xs = (b, c, d)
  where
    (cs,bs,ds) = cbd_vectors c b d h f z u n  --     c
    c = array (0,n) ((n =: 0):(cs []))        --   r   i
    b = array (0,n-1) (bs [])                 -- a       r
    d = array (0,n-1) (ds [])                 --   l   c
                                              --     u
    u = array (0,n-1) ((0 =: 0):(us []))      --   r c i
    z = array bds ((0 =: 0):(zs [n =: 0]))    -- a       r
    (us,zs) = uz_vectors u z n xs h al        --   l u c   
    al = alpha_vector xs h f n id

    h = array (0,n-1) [i =: xs!(i+1) - xs!i | i <- [0..n-1]]
    -- could calc. h on the fly instead of storing it.
    bds@(0,n) = bounds xs

-- Constructs the coefficients for a natural cubic spline interpolant S
-- for the function _f_ defined at the distinct points in _xs_

-- It uses Crout reduction to solve the tridiagonal linear system that pops
-- up during the course of the problem.
-- B&F pp 122

-------------

cbd_vectors c b d h f z u n = foldl g (id,id,id) (reverse [0..n-1])
  where
    g (cs,bs,ds) j = (cs',bs',ds')
      where
        cs' = cs . ((j =: z!j - (u!j)*c!(j+1)) : )
        bs' = bs . ((j =: (f (j+1) - (f j))/h!j - h!j*(c!(j+1) + 2*c!j)/3):)
        ds' = ds . ((j =: (c!(j+1) - c!j)/(3*h!j)) : )

--------------

alpha_vector xs h f n al0 = al
  where
    al = array (0,n) (al0 [i =: ali i | i <- [1..n-1]])
    ali i = 3*((f (i+1))*h!(i-1) - (f i)*(xs!(i+1) - xs!(i-1)) + (f (i-1))*h!i)/(h!(i-1)*h!i)

--------------

uz_vectors u z n xs h al = foldl f (id,id) [1..n-1]
  where
    f (us, zs) i = (us', zs')
      where
        liv = 2*(xs!(i+1) - xs!(i-1)) - h!(i-1)*u!(i-1)
        us' = us . ((i =: h!i/liv):)
        zs' = zs . ((i =: (al!i - h!(i-1)*z!(i-1))/liv):)

--------------

cubic_spline_test spline_alg j x = (v, b, c, d)
  where
    xs = listArray (0,8) [1,2,5,6,7,8,10,13,17]
    a  = listArray (0,8) [3.0,3.7,3.9,4.2,5.7,6.6,7.1,6.7,4.5]
    (b,c,d) = spline_alg (a!) xs
    v  = cubic_spline_approx (a!) b c d xs j x


clamped_cubic_spline_test j x = cubic_spline_test g j x 
  where
    g = (\ f xs -> clamped_cubic_spline_coefficients f 1.0 (-0.67) xs)


natural_cubic_spline_test j x = cubic_spline_test g j x
  where
    g = natural_cubic_spline_coefficients

-------------
