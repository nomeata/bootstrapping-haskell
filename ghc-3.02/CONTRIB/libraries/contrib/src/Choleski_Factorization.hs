-- $Id: Choleski_Factorization.hs,v 1.1 1998/02/03 12:50:55 simonm Exp $

module Choleski_Factorization where
import Array --1.3

-- Choleski's Algorithm for derviving the lower triangular matrix
-- i.e. produces L from A where LL$^{t}$ = A

--------------

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920804
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

--------------

choleski_factorization a = l
  where
    bs = bounds a
    l = array bs [ (b, v) | b <- range bs, v <- f b]
    f (i,j) =
      if i == j then
        [sqrt (a!(i,i) - sum [(l!(i,k))^2 | k <- [1..i-1]])]
      else if i > j then
        [(a!(i,j) - sum [l!(j,k)*l!(i,k) | k<-[1..j-1]])/l!(j,j)]
      else
        []

-- Improved version (of the following one) that doesn't cause so
-- much list concatenation.  No tests have been done to see if it
-- is actually any quicker!

---------------

choleski_factorization_direct a = l
  where
    l = array bs ((tl:lc) ++ x ++ [br])
    tl = ((m,m), sqrt (a!(m,m)))
    br = ((n,n), f n)
    diag i = ((i,i), f i)
    body i = [ ((j,i), g i j) | j <- [i+1..n]]
    lc = [((j,m), a!(j,m)/l!(m,m)) | j <- [m+1..n]]
    f i = sqrt (a!(i,i) - sum [(l!(i,k))^2 | k <- [m..i-1]])
    g i j = (a!(j,i) - sum [l!(j,k)*l!(i,k) | k<-[m..i-1]])/l!(i,i)
    bs@((m,_),(n,_)) = bounds a
    x = concat [ (diag i):(body i) | i <- [m+1..n-1] ]

-- Direct implementation of the algorithm as it appears in B&F pp 351
-- This is going to take a lot of partial evaluation to ever get anywhere
-- near the speed of an imperative version.

-------------

chl_ll :: (Ix a, Enum a) => (Array (a, a) b) -> [[b]]
chl_ll q = 
  case bounds q of
    ((a,b), (c,d)) -> [[q!(y,x) | x <- [b..y]] | y <- [a..c]]
    _ -> []

-- return L as a list of lists. 
-- useful for testing Choleski's algorithm interactively.

-------------

{-
An example :-
Given 

      [  4  -1   1   ]
  A = [ -1 4.25 2.75 ]
      [  1 2.75 3.5  ]

then Choleski's algorithm should produce :-

      [  2   0   0 ]
  L = [ -0.5 2   0 ]
      [ -0.5 1.5 1 ]

i.e. given :-

  a = listArray ((1,1),(3,3)) [4,-1,1,-1,4.25,2.75,1,2.75,3.5]

then

  chl_ll (choleski_factorization a)

should produce :-

[ [2.0000000000000000]
, [-5.0000000000000000e-1, 2.0000000000000000]
, [5.0000000000000000e-1, 1.5000000000000000, 1.0000000000000000]
]

-}
