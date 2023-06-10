-- $Id: Runge_Kutta_Approx.hs,v 1.1 1998/02/03 12:51:16 simonm Exp $

module Runge_Kutta_Approx where

-- Runge-Kutta methods solving second-order differential equations.

-- Stephen J. Bevan <bevan@cs.man.ac.uk> 19920802
-- No claims are made about the accuracy/precision/rounding-errors of
-- the routines, as they are almost direct translations of the algorithms
-- as described in the references.

-- B&F:
-- Numerical Analysis (third edition) - Richard L. Burden J. and Douglas Faires
-- Prindle, Weber & Schmidt 1985

-------------

runge_kutta_midpoint f h (t, w) = (t', w')
  where
    t' = t + h
    w' = w + h*(f (t + (h/2)) (w + (h*(f t w))/2))

-- B&F pp 223

-------------

runge_kutta_midpoint_t
   :: (Fractional a)
   => (a -> a -> a) -- function to solve
   -> a             -- interval size
   -> Integer       -- number of intervals
   -> a             -- interval lower bound
   -> a             -- initial condition
   -> a

-- pre n > 0

runge_kutta_midpoint_t f h = loop
  where
    loop 0 _ w = w
    loop i t w = loop (i-1) t' w'
      where
        t' = t + h
        w' = w + h*(f (t + (h/2)) (w + (h/2)*(f t w)))

-- traditional "loopy" version of the Runge-Kutta Midpoint Method.
-- try comparing this against the previous version to see how well
-- the compiler optimises away the lists.

-------------

runge_kutta_modified_euler f h (t, w) = (t', w')
  where
    t' = t + h
    w' = w + (h*((f t w) + (f (t+h) (w + h*(f t w))))/2)

-- B&F pp 223

-------------

runge_kutta_heun f h (t, w) = (t', w')
  where
    t' = t + h
    w' = w + (h*((f t w) + 3*(f (t + (2*h)/3) (w + (2*h*(f t w))/3))))/4

-- B&F pp 224

-------------

runge_kutta_4 f h (t, w) = (t', w')
  where
    t' = t + h
    w' = w + (k1 + 2*k2 + 2*k3 + k4)/6
    k1 = h*(f t w)
    k2 = h*(f (t + h/2) (w + k1/2))
    k3 = h*(f (t + h/2) (w + k2/2))
    k4 = h*(f (t + h) (w + k3))
    
-- B&F pp 225

--------------

runge_kutta_4_t
   :: (Fractional a)
   => (a -> a -> a) -- function to solve
   -> a             -- interval size
   -> a             -- interval lower bound
   -> Integer       -- number of divisions
   -> a             -- initial condition
   -> a

-- pre n > 0

runge_kutta_4_t f h a n w0 = loop n a w0
  where
  loop 0 t w = w
  loop i t w = loop (i-1) t' w'
    where
      k1 = h*(f t w)
      k2 = h*(f (t + h/2) (w + k1/2))
      k3 = h*(f (t + h/2) (w + k2/2))
      k4 = h*(f (t + h) (w + k3))
      w' = w + (k1 + 2*k2 + 2*k3 + k4)/6
      t' = t + h

-- traditional "loopy" version of a fourth order Runge-Kutta
-- B&F pp 225

-------------

runge_kutta_approx_values m f h t w0 = iterate (m f h) (t, w0)

-- generate an infinite list of solutions using the solution method _m_
-- for equation _f_ using: _h_ as the interval size, _t_ as the interval 
-- lower bound and w as the initial approximation.

-------------

runge_kutta_fehlberg
  :: (Floating a, Ord a)
  => (a -> a -> a)  -- function to approximate
  -> a              -- interval lower bound
  -> a              -- interval upper bound
  -> a              -- tolerance
  -> a              -- maximum step size
  -> a              -- minimum step size
  -> a              -- initial approximation
  -> [(a,a)]        -- [(interval,approximation)]

runge_kutta_fehlberg f a b tol h_max h_min a0 = loop a a0 h_max [(a,a0)]
  where
    loop t w h rs =
      if t >= b then
        rs
      else if h'' < h_min then
        rs'
      else if r <= tol then
        loop t' w' h'' rs'
      else
        loop t w h'' rs
      where
        k1 = h*(f t w)
        k2 = h*(f (t + h/4) (w + k1/4))
        k3 = h*(f (t + 3/8*h) (w + 3/32*k1 + 9/32*k2))
        k4 = h*(f (t + 12/13*h) (w + 1932/2197*k1 - 7200/2197*k2 + 7296/2197*k3))
        k5 = h*(f (t + h) (w + 439/216*k1 - 8*k2 + 3680/513*k3 - 845/4104*k4))
        k6 = h*(f (t + h/2) (w - 8/27*k1 + 2*k2 - 3544/2565*k3 + 1859/4104*k4 -11/40*k5))
        r  = (abs (1/360*k1 - 128/4275*k3 - 2197/75240*k4 + k5/50 + 2/55*k6))/h
        d  = 0.84*((tol/r)**0.25)
        t' = t + h
        w' = w + 25/216*k1 + 1408/2565*k3 + 2197/4104*k4 - k5/5
        rs' = if r <= tol then  (t', w'):rs else rs
        h'  = if d <= 0.1 then h/10 else if d >= 4 then 4*h else d*h
        h'' = if h' > h_max then h_max else h'

-- B&F pp 234
-- The answers come out backwards i.e. the closest approximation
-- will be the first in the output, moving to the initial approximation
-- which will be last in the list.
-- There is no observable difference between a normal finish and
-- the maximum _h_ being exceeded, though it should be easy to add
-- something if this is desired.

-------------

-- Some sample equations to solve :-
--
--   y1' t y = -y + t^2 + 1
--   y2' t y = -y + t + 1
--
-- Some examples :-
--
-- > snd ((runge_kutta_approx_values runge_kutta_heun y1' 0.1 0 1) !! 10)
-- 1.2651336760001470
--
-- > snd ((runge_kutta_approx_values runge_kutta_midpoint y1' 0.1 0 1) !! 10)
-- 1.2645797645833345
--
-- > head (runge_kutta_fehlberg y2' 0 1 5e-5 0.1 0.02 1)
-- (1.0999999999999999, 1.4328710262765685)

-- eof
