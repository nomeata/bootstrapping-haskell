We add the option -fno-implicit-prelude here to tell the reader that
special names such as () and -> shouldn't be resolved to Prelude.()
and Prelude.-> (as they are normally). -- SDM 8/10/97

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Prelude (

	-- Everything corresponding to the Report's PreludeList
    module PrelList, 
    lines, words, unlines, unwords,
    sum, product,

        -- Everything corresponding to the Report's PreludeText
    ReadS, ShowS,
    Read(readsPrec, readList),
    Show(showsPrec, showList, show),
    reads, shows, read, lex, 
    showChar, showString, readParen, showParen,
    
        -- Everything corresponding to the Report's PreludeIO
    FilePath, IOError,
    ioError, userError, catch,
    putChar, putStr, putStrLn, print,
    getChar, getLine, getContents, interact,
    readFile, writeFile, appendFile, readIO, readLn,

    Bool(..),
    Maybe(..),
    Either(..),
    Ordering(..), 
    Char, String, Int, Integer, Float, Double, IO,
    Rational,
    []((:), []),
    
    module PrelTup,
        -- Includes tuple types + fst, snd, curry, uncurry
    ()(..),		-- The unit type
    (->),		-- functions
    
    Eq(..),
    Ord(..), 
    Enum(..),
    Bounded(..), 
    Num((+), (-), (*), negate, abs, signum, fromInteger),
	-- The fromInt method is exposed only by GlaExts
    Real(..),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
	-- The toInt method is exposed only by GlaExts
    Fractional(..),
    Floating(..),
    RealFrac(..),
    RealFloat(..),

	-- Monad stuff, from PrelBase, and defined here
    Monad(..),
    Functor(..), 
    mapM, mapM_, sequence, sequence_, (=<<),

    maybe, either,
    (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^), 
    fromIntegral, realToFrac,
    --exported by PrelTup: fst, snd, curry, uncurry,
    id, const, (.), flip, ($), until,
    asTypeOf, error, undefined,
    seq, ($!)

  ) where

import PrelBase
import PrelList
#ifndef USE_REPORT_PRELUDE
     hiding ( takeUInt_append )
#endif
import PrelIO
import PrelIOBase
import PrelException
import PrelRead
import PrelEnum
import PrelNum
import PrelReal
import PrelFloat
import PrelTup
import PrelMaybe
import PrelShow
import PrelConc
import PrelErr   ( error )

infixr 1 =<<
infixr 0 $!
\end{code}


%*********************************************************
%*							*
\subsection{Miscellaneous functions}
%*							*
%*********************************************************

\begin{code}
($!)    :: (a -> b) -> a -> b
f $! x  = x `seq` f x

-- It is expected that compilers will recognize this and insert error
-- messages which are more appropriate to the context in which undefined 
-- appears. 

undefined               :: a
undefined               =  error "Prelude.undefined"
\end{code}


%*********************************************************
%*							*
\subsection{List sum and product}
%*							*
%*********************************************************

List sum and product are defined here because PrelList is too far
down the compilation chain to "see" the Num class.

\begin{code}
-- sum and product compute the sum or product of a finite list of numbers.
{-# SPECIALISE sum     :: [Int] -> Int #-}
{-# SPECIALISE sum     :: [Integer] -> Integer #-}
{-# SPECIALISE product :: [Int] -> Int #-}
{-# SPECIALISE product :: [Integer] -> Integer #-}
sum, product            :: (Num a) => [a] -> a
#ifdef USE_REPORT_PRELUDE
sum                     =  foldl (+) 0  
product                 =  foldl (*) 1
#else
sum	l	= sum' l 0
  where
    sum' []     a = a
    sum' (x:xs) a = sum' xs (a+x)
product	l	= prod l 1
  where
    prod []     a = a
    prod (x:xs) a = prod xs (a*x)
#endif
\end{code}


%*********************************************************
%*							*
\subsection{Prelude monad functions}
%*							*
%*********************************************************

\begin{code}
{-# SPECIALISE (=<<) :: (a -> [b]) -> [a] -> [b] #-}
(=<<)           :: Monad m => (a -> m b) -> m a -> m b
f =<< x		= x >>= f

sequence       :: Monad m => [m a] -> m [a] 
{-# INLINE sequence #-}
sequence ms = foldr k (return []) ms
	    where
	      k m m' = do { x <- m; xs <- m'; return (x:xs) }

sequence_        :: Monad m => [m a] -> m () 
{-# INLINE sequence_ #-}
sequence_ ms     =  foldr (>>) (return ()) ms

mapM            :: Monad m => (a -> m b) -> [a] -> m [b]
{-# INLINE mapM #-}
mapM f as       =  sequence (map f as)

mapM_           :: Monad m => (a -> m b) -> [a] -> m ()
{-# INLINE mapM_ #-}
mapM_ f as      =  sequence_ (map f as)
\end{code}


%*********************************************************
%*							*
\subsection{Coercions}
%*							*
%*********************************************************

\begin{code}
{-# RULES
"fromIntegral/Int->Int"                     fromIntegral   = id :: Int     -> Int
"fromIntegral/Integer->Integer"             fromIntegral   = id :: Integer -> Integer
"fromIntegral/Int->Integer"                 fromIntegral   = int2Integer
"fromIntegral/Integer->Int"                 fromIntegral   = integer2Int
"fromIntegral/Int->Rational"     forall n . fromIntegral n = int2Integer n :% 1
"fromIntegral/Integer->Rational" forall n . fromIntegral n = n :% (1 :: Integer)
"fromIntegral/Int->Float"                   fromIntegral   = int2Float
"fromIntegral/Int->Double"                  fromIntegral   = int2Double
"fromIntegral/Integer->Float"    forall n . fromIntegral n = encodeFloat n 0 :: Float
"fromIntegral/Integer->Double"   forall n . fromIntegral n = encodeFloat n 0 :: Double
 #-}
fromIntegral	:: (Integral a, Num b) => a -> b
fromIntegral	=  fromInteger . toInteger

{-# RULES
"realToFrac/Float->Double"      realToFrac = floatToDouble
"realToFrac/Double->Float"      realToFrac = doubleToFloat
"realToFrac/Float->Float"       realToFrac = id      :: Float    -> Float
"realToFrac/Double->Double"     realToFrac = id      :: Double   -> Double
"realToFrac/Rational->Rational" realToFrac = id      :: Rational -> Rational
"realToFrac/Float->Rational"    realToFrac = rf2rat  :: Float    -> Rational
"realToFrac/Double->Rational"   realToFrac = rf2rat  :: Double   -> Rational
"realToFrac/Rational->Float"    realToFrac = fromRat :: Rational -> Float
"realToFrac/Rational->Double"   realToFrac = fromRat :: Rational -> Double
 #-}
realToFrac	:: (Real a, Fractional b) => a -> b
realToFrac	=  fromRational . toRational

doubleToFloat :: Double -> Float
doubleToFloat (D# d) = F# (double2Float# d)

floatToDouble :: Float -> Double
floatToDouble (F# f) = D# (float2Double# f)

{-# SPECIALIZE rf2rat ::
    Float  -> Rational,
    Double -> Rational
 #-}
rf2rat :: RealFloat a => a -> Rational
rf2rat x = if n >= 0 then (m * (b ^ n)) :% 1 else m :% (b ^ (-n))
   where (m,n) = decodeFloat x
         b     = floatRadix  x
\end{code}
