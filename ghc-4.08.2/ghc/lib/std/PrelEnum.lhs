%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelBounded]{Module @PrelBounded@}

Instances of Bounded for various datatypes.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelEnum(
	Bounded(..), Enum(..),
	boundedEnumFrom, boundedEnumFromThen,

	-- Instances for Bounded and Eum: (), Char, Int

   ) where

import {-# SOURCE #-} PrelErr ( error )
import PrelBase
import PrelTup	()	-- To make sure we look for the .hi file

default ()		-- Double isn't available yet
\end{code}


%*********************************************************
%*							*
\subsection{Class declarations}
%*							*
%*********************************************************

\begin{code}
class  Bounded a  where
    minBound, maxBound :: a

class  Enum a	where
    succ, pred		:: a -> a
    toEnum              :: Int -> a
    fromEnum            :: a -> Int
    enumFrom		:: a -> [a]		-- [n..]
    enumFromThen	:: a -> a -> [a]	-- [n,n'..]
    enumFromTo		:: a -> a -> [a]	-- [n..m]
    enumFromThenTo	:: a -> a -> a -> [a]	-- [n,n'..m]

    succ		   = toEnum . (`plusInt` oneInt)  . fromEnum
    pred		   = toEnum . (`minusInt` oneInt) . fromEnum
    enumFrom x       	   = map toEnum [fromEnum x ..]
    enumFromThen x y 	   = map toEnum [fromEnum x, fromEnum y ..]
    enumFromTo x y         = map toEnum [fromEnum x .. fromEnum y]
    enumFromThenTo x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

-- Default methods for bounded enumerations
boundedEnumFrom :: (Enum a, Bounded a) => a -> [a]
boundedEnumFrom n = map toEnum [fromEnum n .. fromEnum (maxBound `asTypeOf` n)]

boundedEnumFromThen :: (Enum a, Bounded a) => a -> a -> [a]
boundedEnumFromThen n1 n2 
  | i_n2 >= i_n1  = map toEnum [i_n1, i_n2 .. fromEnum (maxBound `asTypeOf` n1)]
  | otherwise     = map toEnum [i_n1, i_n2 .. fromEnum (minBound `asTypeOf` n1)]
  where
    i_n1 = fromEnum n1
    i_n2 = fromEnum n2
\end{code}


%*********************************************************
%*							*
\subsection{Tuples}
%*							*
%*********************************************************

\begin{code}
instance Bounded () where
    minBound = ()
    maxBound = ()

instance Enum () where
    succ _      = error "Prelude.Enum.().succ: bad argment"
    pred _      = error "Prelude.Enum.().pred: bad argument"

    toEnum x | x == zeroInt = ()
             | otherwise    = error "Prelude.Enum.().toEnum: bad argument"

    fromEnum () = zeroInt
    enumFrom () 	= [()]
    enumFromThen () () 	= [()]
    enumFromTo () () 	= [()]
    enumFromThenTo () () () = [()]
\end{code}

\begin{code}
instance (Bounded a, Bounded b) => Bounded (a,b) where
   minBound = (minBound, minBound)
   maxBound = (maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c) => Bounded (a,b,c) where
   minBound = (minBound, minBound, minBound)
   maxBound = (maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d) => Bounded (a,b,c,d) where
   minBound = (minBound, minBound, minBound, minBound)
   maxBound = (maxBound, maxBound, maxBound, maxBound)
\end{code}


%*********************************************************
%*							*
\subsection{Type @Bool@}
%*							*
%*********************************************************

\begin{code}
instance Bounded Bool where
  minBound = False
  maxBound = True

instance Enum Bool where
  succ False = True
  succ True  = error "Prelude.Enum.Bool.succ: bad argment"

  pred True  = False
  pred False  = error "Prelude.Enum.Bool.pred: bad argment"

  toEnum n | n == zeroInt = False
	   | n == oneInt  = True
	   | otherwise    = error "Prelude.Enum.Bool.toEnum: bad argment"

  fromEnum False = zeroInt
  fromEnum True  = oneInt

  -- Use defaults for the rest
  enumFrom     = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
\end{code}

%*********************************************************
%*							*
\subsection{Type @Ordering@}
%*							*
%*********************************************************

\begin{code}
instance Bounded Ordering where
  minBound = LT
  maxBound = GT

instance Enum Ordering where
  succ LT = EQ
  succ EQ = GT
  succ GT = error "Prelude.Enum.Ordering.succ: bad argment"

  pred GT = EQ
  pred EQ = LT
  pred LT = error "Prelude.Enum.Ordering.pred: bad argment"

  toEnum n | n == zeroInt = LT
	   | n == oneInt  = EQ
	   | n == twoInt  = GT
  toEnum _ = error "Prelude.Enum.Ordering.toEnum: bad argment"

  fromEnum LT = zeroInt
  fromEnum EQ = oneInt
  fromEnum GT = twoInt

  -- Use defaults for the rest
  enumFrom     = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
\end{code}

%*********************************************************
%*							*
\subsection{Type @Char@}
%*							*
%*********************************************************

\begin{code}
instance  Bounded Char  where
    minBound =  '\0'
    maxBound =  '\255'

instance  Enum Char  where
    succ (C# c#)
       | not (ord# c# ==# 255#) = C# (chr# (ord# c# +# 1#))
       | otherwise	        = error ("Prelude.Enum.Char.succ: bad argument")
    pred (C# c#)
       | not (ord# c# ==# 0#)   = C# (chr# (ord# c# -# 1#))
       | otherwise	        = error ("Prelude.Enum.Char.pred: bad argument")

    toEnum   = chr
    fromEnum = ord

    {-# INLINE enumFrom #-}
    enumFrom (C# x) = eftChar (ord# x) 255#
	-- Blarg: technically I guess enumFrom isn't strict!

    {-# INLINE enumFromTo #-}
    enumFromTo (C# x) (C# y) = eftChar (ord# x) (ord# y)
    
    {-# INLINE enumFromThen #-}
    enumFromThen (C# x1) (C# x2) = efdChar (ord# x1) (ord# x2)
    
    {-# INLINE enumFromThenTo #-}
    enumFromThenTo (C# x1) (C# x2) (C# y) = efdtChar (ord# x1) (ord# x2) (ord# y)

eftChar  = eftCharList
efdChar  = efdCharList
efdtChar = efdtCharList


{-# RULES
"eftChar"	forall x y.	eftChar x y	  = build (\c n -> eftCharFB c n x y)
"efdChar"	forall x1 x2.	efdChar x1 x2	  = build (\ c n -> efdCharFB c n x1 x2)
"efdtChar"	forall x1 x2 l.	efdtChar x1 x2 l  = build (\ c n -> efdtCharFB c n x1 x2 l)
"eftCharList"	eftCharFB  (:) [] = eftCharList
"efdCharList"	efdCharFB  (:) [] = efdCharList
"efdtCharList"	efdtCharFB (:) [] = efdtCharList
 #-}


-- We can do better than for Ints because we don't
-- have hassles about arithmetic overflow at maxBound
{-# INLINE eftCharFB #-}
eftCharFB c n x y = go x
		 where
		    go x | x ># y    = n
			 | otherwise = C# (chr# x) `c` go (x +# 1#)

eftCharList x y | x ># y    = [] 
	        | otherwise = C# (chr# x) : eftCharList (x +# 1#) y


-- For enumFromThenTo we give up on inlining
efdCharFB c n x1 x2
  | delta >=# 0# = go_up_char_fb c n x1 delta 255#
  | otherwise    = go_dn_char_fb c n x1 delta 0#
  where
    delta = x2 -# x1

efdCharList x1 x2
  | delta >=# 0# = go_up_char_list x1 delta 255#
  | otherwise    = go_dn_char_list x1 delta 0#
  where
    delta = x2 -# x1

efdtCharFB c n x1 x2 lim
  | delta >=# 0# = go_up_char_fb c n x1 delta lim
  | otherwise    = go_dn_char_fb c n x1 delta lim
  where
    delta = x2 -# x1

efdtCharList x1 x2 lim
  | delta >=# 0# = go_up_char_list x1 delta lim
  | otherwise    = go_dn_char_list x1 delta lim
  where
    delta = x2 -# x1

go_up_char_fb c n x delta lim
  = go_up x
  where
    go_up x | x ># lim  = n
	    | otherwise	= C# (chr# x) `c` go_up (x +# delta)

go_dn_char_fb c n x delta lim
  = go_dn x
  where
    go_dn x | x <# lim  = n
	    | otherwise	= C# (chr# x) `c` go_dn (x +# delta)

go_up_char_list x delta lim
  = go_up x
  where
    go_up x | x ># lim  = []
	    | otherwise	= C# (chr# x) : go_up (x +# delta)

go_dn_char_list x delta lim
  = go_dn x
  where
    go_dn x | x <# lim  = []
	    | otherwise	= C# (chr# x) : go_dn (x +# delta)
\end{code}


%*********************************************************
%*							*
\subsection{Type @Int@}
%*							*
%*********************************************************

Be careful about these instances.  
	(a) remember that you have to count down as well as up e.g. [13,12..0]
	(b) be careful of Int overflow
	(c) remember that Int is bounded, so [1..] terminates at maxInt

Also NB that the Num class isn't available in this module.
	
\begin{code}
instance  Bounded Int where
    minBound =  minInt
    maxBound =  maxInt

instance  Enum Int  where
    succ x  
       | x == maxBound  = error "Prelude.Enum.succ{Int}: tried to take `succ' of maxBound"
       | otherwise      = x `plusInt` oneInt
    pred x
       | x == minBound  = error "Prelude.Enum.pred{Int}: tried to take `pred' of minBound"
       | otherwise      = x `minusInt` oneInt

    toEnum   x = x
    fromEnum x = x

    {-# INLINE enumFrom #-}
    enumFrom (I# x) = eftInt x 2147483647#
	-- Blarg: technically I guess enumFrom isn't strict!

    {-# INLINE enumFromTo #-}
    enumFromTo (I# x) (I# y) = eftInt x y

    {-# INLINE enumFromThen #-}
    enumFromThen (I# x1) (I# x2) = efdInt x1 x2

    {-# INLINE enumFromThenTo #-}
    enumFromThenTo (I# x1) (I# x2) (I# y) = efdtInt x1 x2 y

eftInt 	= eftIntList
efdInt 	= efdIntList
efdtInt = efdtIntList

{-# RULES
"eftInt"	forall x y.	eftInt x y	 = build (\ c n -> eftIntFB c n x y)
"efdInt"	forall x1 x2.	efdInt x1 x2	 = build (\ c n -> efdIntFB c n x1 x2)
"efdtInt"	forall x1 x2 l.	efdtInt x1 x2 l	 = build (\ c n -> efdtIntFB c n x1 x2 l)

"eftIntList"	eftIntFB  (:) [] = eftIntList
"efdIntList"	efdIntFB  (:) [] = efdIntList
"efdtIntList"	efdtIntFB (:) [] = efdtIntList
 #-}


{-# INLINE eftIntFB #-}
eftIntFB c n x y | x ># y    = n	
		 | otherwise = go x
		 where
		   go x = I# x `c` if x ==# y then n else go (x +# 1#)
			-- Watch out for y=maxBound; hence ==, not >
	-- Be very careful not to have more than one "c"
	-- so that when eftInfFB is inlined we can inline
	-- whatver is bound to "c"

eftIntList x y | x ># y    = []
	       | otherwise = go x
	       where
		 go x = I# x : if x ==# y then [] else go (x +# 1#)


-- For enumFromThenTo we give up on inlining; so we don't worry
-- about duplicating occurrences of "c"
efdtIntFB c n x1 x2 y
  | delta >=# 0# = if x1 ># y then n else go_up_int_fb c n x1 delta lim
  | otherwise    = if x1 <# y then n else go_dn_int_fb c n x1 delta lim 
  where
    delta = x2 -# x1
    lim   = y -# delta

efdtIntList x1 x2 y
  | delta >=# 0# = if x1 ># y then [] else go_up_int_list x1 delta lim
  | otherwise    = if x1 <# y then [] else go_dn_int_list x1 delta lim
  where
    delta = x2 -# x1
    lim   = y -# delta

efdIntFB c n x1 x2
  | delta >=# 0# = go_up_int_fb c n x1 delta (  2147483647#  -# delta)
  | otherwise    = go_dn_int_fb c n x1 delta ((-2147483648#) -# delta)
  where
    delta = x2 -# x1

efdIntList x1 x2
  | delta >=# 0# = go_up_int_list x1 delta (  2147483647#  -# delta)
  | otherwise    = go_dn_int_list x1 delta ((-2147483648#) -# delta)
  where
    delta = x2 -# x1

-- In all of these, the (x +# delta) is guaranteed not to overflow

go_up_int_fb c n x delta lim
  = go_up x
  where
    go_up x | x ># lim  = I# x `c` n
	    | otherwise = I# x `c` go_up (x +# delta)

go_dn_int_fb c n x delta lim 
  = go_dn x
  where
    go_dn x | x <# lim  = I# x `c` n
	    | otherwise = I# x `c` go_dn (x +# delta)

go_up_int_list x delta lim
  = go_up x
  where
    go_up x | x ># lim  = [I# x]
	    | otherwise = I# x : go_up (x +# delta)

go_dn_int_list x delta lim 
  = go_dn x
  where
    go_dn x | x <# lim  = [I# x]
	    | otherwise = I# x : go_dn (x +# delta)
\end{code}

