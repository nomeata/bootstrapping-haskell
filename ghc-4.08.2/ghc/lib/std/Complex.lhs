%
% (c) The AQUA Project, Glasgow University, 1994-1999
%

\section[Complex]{Module @Complex@}

\begin{code}
module Complex
	( Complex((:+))
	
	, realPart	-- :: (RealFloat a) => Complex a -> a
	, imagPart      -- :: (RealFloat a) => Complex a -> a
	, conjugate     -- :: (RealFloat a) => Complex a -> Complex a
	, mkPolar       -- :: (RealFloat a) => a -> a -> Complex a
	, cis           -- :: (RealFloat a) => a -> Complex a
	, polar         -- :: (RealFloat a) => Complex a -> (a,a)
	, magnitude     -- :: (RealFloat a) => Complex a -> a
	, phase         -- :: (RealFloat a) => Complex a -> a
	
	-- Complex instances:
	--
	--  (RealFloat a) => Eq         (Complex a)
	--  (RealFloat a) => Read       (Complex a)
	--  (RealFloat a) => Show       (Complex a)
	--  (RealFloat a) => Num        (Complex a)
	--  (RealFloat a) => Fractional (Complex a)
	--  (RealFloat a) => Floating   (Complex a)
	-- 
        -- Implementation checked wrt. Haskell 98 lib report, 1/99.

        )  where

import Prelude

infix  6  :+
\end{code}

%*********************************************************
%*							*
\subsection{The @Complex@ type}
%*							*
%*********************************************************

\begin{code}
data  (RealFloat a)     => Complex a = !a :+ !a  deriving (Eq, Read, Show)
\end{code}


%*********************************************************
%*							*
\subsection{Functions over @Complex@}
%*							*
%*********************************************************

\begin{code}
realPart, imagPart :: (RealFloat a) => Complex a -> a
realPart (x :+ _) =  x
imagPart (_ :+ y) =  y

conjugate	 :: (RealFloat a) => Complex a -> Complex a
conjugate (x:+y) =  x :+ (-y)

mkPolar		 :: (RealFloat a) => a -> a -> Complex a
mkPolar r theta	 =  r * cos theta :+ r * sin theta

cis		 :: (RealFloat a) => a -> Complex a
cis theta	 =  cos theta :+ sin theta

polar		 :: (RealFloat a) => Complex a -> (a,a)
polar z		 =  (magnitude z, phase z)

magnitude :: (RealFloat a) => Complex a -> a
magnitude (x:+y) =  scaleFloat k
		     (sqrt ((scaleFloat mk x)^(2::Int) + (scaleFloat mk y)^(2::Int)))
		    where k  = max (exponent x) (exponent y)
		          mk = - k

phase :: (RealFloat a) => Complex a -> a
phase (0 :+ 0)   = 0		-- SLPJ July 97 from John Peterson
phase (x:+y)	 = atan2 y x
\end{code}


%*********************************************************
%*							*
\subsection{Instances of @Complex@}
%*							*
%*********************************************************

\begin{code}
instance  (RealFloat a) => Num (Complex a)  where
    {-# SPECIALISE instance Num (Complex Float) #-}
    {-# SPECIALISE instance Num (Complex Double) #-}
    (x:+y) + (x':+y')	=  (x+x') :+ (y+y')
    (x:+y) - (x':+y')	=  (x-x') :+ (y-y')
    (x:+y) * (x':+y')	=  (x*x'-y*y') :+ (x*y'+y*x')
    negate (x:+y)	=  negate x :+ negate y
    abs z		=  magnitude z :+ 0
    signum 0		=  0
    signum z@(x:+y)	=  x/r :+ y/r  where r = magnitude z
    fromInteger n	=  fromInteger n :+ 0

instance  (RealFloat a) => Fractional (Complex a)  where
    {-# SPECIALISE instance Fractional (Complex Float) #-}
    {-# SPECIALISE instance Fractional (Complex Double) #-}
    (x:+y) / (x':+y')	=  (x*x''+y*y'') / d :+ (y*x''-x*y'') / d
			   where x'' = scaleFloat k x'
				 y'' = scaleFloat k y'
				 k   = - max (exponent x') (exponent y')
				 d   = x'*x'' + y'*y''

    fromRational a	=  fromRational a :+ 0

instance  (RealFloat a) => Floating (Complex a)	where
    {-# SPECIALISE instance Floating (Complex Float) #-}
    {-# SPECIALISE instance Floating (Complex Double) #-}
    pi             =  pi :+ 0
    exp (x:+y)     =  expx * cos y :+ expx * sin y
                      where expx = exp x
    log z          =  log (magnitude z) :+ phase z

    sqrt 0         =  0
    sqrt z@(x:+y)  =  u :+ (if y < 0 then -v else v)
                      where (u,v) = if x < 0 then (v',u') else (u',v')
                            v'    = abs y / (u'*2)
                            u'    = sqrt ((magnitude z + abs x) / 2)

    sin (x:+y)     =  sin x * cosh y :+ cos x * sinh y
    cos (x:+y)     =  cos x * cosh y :+ (- sin x * sinh y)
    tan (x:+y)     =  (sinx*coshy:+cosx*sinhy)/(cosx*coshy:+(-sinx*sinhy))
                      where sinx  = sin x
                            cosx  = cos x
                            sinhy = sinh y
                            coshy = cosh y

    sinh (x:+y)    =  cos y * sinh x :+ sin  y * cosh x
    cosh (x:+y)    =  cos y * cosh x :+ sin y * sinh x
    tanh (x:+y)    =  (cosy*sinhx:+siny*coshx)/(cosy*coshx:+siny*sinhx)
                      where siny  = sin y
                            cosy  = cos y
                            sinhx = sinh x
                            coshx = cosh x

    asin z@(x:+y)  =  y':+(-x')
                      where  (x':+y') = log (((-y):+x) + sqrt (1 - z*z))
    acos z         =  y'':+(-x'')
                      where (x'':+y'') = log (z + ((-y'):+x'))
                            (x':+y')   = sqrt (1 - z*z)
    atan z@(x:+y)  =  y':+(-x')
                      where (x':+y') = log (((1-y):+x) / sqrt (1+z*z))

    asinh z        =  log (z + sqrt (1+z*z))
    acosh z        =  log (z + (z+1) * sqrt ((z-1)/(z+1)))
    atanh z        =  log ((1+z) / sqrt (1-z*z))
\end{code}
