-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Sequence (
    -- class definition + method wrappers
    module Sequence,

    -- re-export view type from EdisonPrelude for convenience
    Maybe2(..)
) where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import Monad
import EdisonPrelude(Maybe2(..))

-- naming convention: instances of Sequence are named Seq whenever possible

class (Functor s, MonadPlus s) => Sequence s where
  -- in addition to Functor, Monad, and MonadPlus,
  -- sequences should also be instances of Eq and Show

----------------------------------------------------------------------
-- Constructors

  empty_    :: s a
  single_   :: a -> s a
    -- empty = <>
    -- single x = <x>

  cons_     :: a -> s a -> s a
  snoc_     :: s a -> a -> s a
  append_   :: s a -> s a -> s a
    -- cons x <x0,...,xn-1> = <x,x0,...,xn-1>
    -- snoc <x0,...,xn-1> x = <x0,...,xn-1,x>
    -- append <x0,...,xn-1> <y0,...,ym-1> = <x0,...,xn-1,y0,...,ym-1>

  fromList_ :: [a] -> s a
    -- fromList [x0,...,xn-1] = <x0,...,xn-1>

  -- initialize a sequence
  copy_     :: Int -> a -> s a          -- returns empty if size is negative
  tabulate_ :: Int -> (Int -> a) -> s a -- returns empty if size is negative
  -- copy n x = <x,...,x>  -- n copies
 -- tabulate f n = <f 0,...,f n-1>

----------------------------------------------------------------------
-- Destructors

  -- view the left element
  lview_    :: s a -> Maybe2 a (s a)
  lhead_    :: s a -> a          -- signals error if sequence is empty
  ltail_    :: s a -> s a        -- returns empty if sequence is empty
    -- lview <x0,...,xn-1> | n==0 = Nothing2
    --                     | n>0  = Just2 x0 <x1,...,xn-1>
    -- lhead <x0,...,xn-1> | n==0 = error "ModuleName.lhead: empty sequence"
    --                     | n>0  = x0
    -- ltail <x0,...,xn-1> | n==0 = <>
    --                     | n>0  = <x1,...,xn-1>


  -- view the right element
  rview_    :: s a -> Maybe2 (s a) a
  rhead_    :: s a -> a          -- signals error if sequence is empty
  rtail_    :: s a -> s a        -- returns empty if sequence is empty
    -- rview <x0,...,xn-1> | n==0 = Nothing2
    --                     | n>0  = Just2 <x0,...,xn-2> xn-1
    -- rhead <x0,...,xn-1> | n==0 = error "ModuleName.rhead: empty sequence"
    --                     | n>0  = xn-1
    -- rtail <x0,...,xn-1> | n==0 = <>
    --                     | n>0  = <x0,...,xn-2>

----------------------------------------------------------------------
-- Observers

  null_     :: s a -> Bool
  size_     :: s a -> Int
    -- null <x0,...,xn-1> = (n==0)
    -- size <x0,...,xn-1> = n

  toList_   :: s a -> [a]
    -- toList <x0,...,xn-1> = [x0,...,xn-1]

----------------------------------------------------------------------
-- Concat and revers

  -- flattening a sequence
  concat_   :: s (s a) -> s a
    -- concat xss = foldr append empty xss


  -- reversing a sequence
  reverse_  :: s a -> s a
  reverseOnto_ :: s a -> s a -> s a
    -- reverse <x0,...,xn-1> = <xn-1,...,x0>
    -- reverseOnto <x0,...,xn-1> <y0,...,ym-1> = <xn-1,...,x0,y0,...,ym-1>

----------------------------------------------------------------------
-- Maps and folds

  map_       :: (a -> b) -> s a -> s b
  concatMap_ :: (a -> s b) -> s a -> s b
    -- map f <x0,...,xn-1> = <f x0,...,f xn-1>
    -- concatMap f xs = concat (map f xs)

  foldr_    :: (a -> b -> b) -> b -> s a -> b
  foldl_    :: (b -> a -> b) -> b -> s a -> b
    -- foldr (+) c <x0,...,xn-1> = x0 + (x1 + ... + (xn-1 + c))
    -- foldl (+) c <x0,...,xn-1> = ((c + x0) + x1) + ... + xn-1

  foldr1_   :: (a -> a -> a) -> s a -> a  -- signals error if sequence is empty
  foldl1_   :: (a -> a -> a) -> s a -> a  -- signals error if sequence is empty
    -- foldr1 (+) <x0,...,xn-1>
    --   | n==0 = error "ModuleName.foldr1: empty sequence"
    --   | n>0  = x0 + (x1 + ... + xn-1)
    -- foldl1 (+) <x0,...,xn-1>
    --   | n==0 = error "ModuleName.foldl1: empty sequence"
    --   | n>0  = (x0 + x1) + ... + xn-1

  reducer_  :: (a -> a -> a) -> a -> s a -> a
  reducel_  :: (a -> a -> a) -> a -> s a -> a
  reduce1_  :: (a -> a -> a) -> s a -> a  -- signals error if sequence is empty
    -- reduce is similar to fold, but combines elements in a balanced fashion
    -- the combining function should usually be associative
    --
    -- reducer (+) x xs = reduce1 (+) (cons x xs)
    -- reducel (+) x xs = reduce1 (+) (snoc xs x)
    --
    -- reduce1 (+) <x> = x
    -- reduce1 (+) <x0,...,xn-1> = 
    --     (reduce1 (+) <x0,...,xi>) + (reduce1 (+) <xi+1,...,xn-1>)
    --   for some i such that 0 <= i && i < n-1
    --
    -- Although the exact value of i is unspecified it tends toward n/2
    -- so that the depth of calls to + is at most logarithmic

----------------------------------------------------------------------
-- Subsequences

  take_       :: Int -> s a -> s a
  drop_       :: Int -> s a -> s a
  splitAt_    :: Int -> s a -> (s a, s a)
    -- take i xs = fst (splitAt i xs)
    -- drop i xs = snd (splitAt i xs)
    --
    -- splitAt i xs
    --   | i < 0  = (<>           , <x0,...,xn-1>)
    --   | i < n  = (<x0,...,xi-1>, <xi,...,xn-1>)
    --   | i >= n = (<x0,...,xn-1>, <>           )
  
  subseq_     :: Int -> Int -> s a -> s a
    -- args are index/length rather than start index/end index
    --
    -- subseq i len xs = take len (drop i xs)

----------------------------------------------------------------------
-- Predicate-based operations

  filter_     :: (a -> Bool) -> s a -> s a
  partition_  :: (a -> Bool) -> s a -> (s a, s a)
    -- filter p xs = foldr pcons empty xs
    --   where pcons x xs = if p x then cons x xs else xs
    --
    -- partition p xs = (filter p xs, filter (not . p) xs)

  takeWhile_  :: (a -> Bool) -> s a -> s a
  dropWhile_  :: (a -> Bool) -> s a -> s a
  splitWhile_ :: (a -> Bool) -> s a -> (s a, s a)
    -- takeWhile p xs = fst (splitWhile p xs)
    -- dropWhile p xs = snd (splitWhile p xs)
    --
    -- splitWhile p <x0,...,xn-1> = (<x0,...,xi-1>, <xi,...,xn-1>)
    --   where i = min j such that p xj (or n if no such j)

----------------------------------------------------------------------
-- Index-based operations (zero-based)

  inBounds_ :: s a -> Int -> Bool
    -- inBounds <x0,...,xn-1> i = (0 <= i && i < n)

  lookup_   :: s a -> Int -> a         -- signals error if index out of bounds
  lookupM_  :: s a -> Int -> Maybe a
  lookupWithDefault_ :: a -> s a -> Int -> a
    -- lookup xs@<x0,...,xn-1> i 
    --   | inBounds xs = xi
    --   | otherwise = error "ModuleName.lookup: index out of bounds"
    -- lookupM xs@<x0,...,xn-1> i 
    --   | inBounds xs = Just xi
    --   | otherwise = Nothing
    -- lookupWithDefault d xs@<x0,...,xn-1> i 
    --   | inBounds xs = xi
    --   | otherwise = d

  update_   :: Int -> a -> s a -> s a
  adjust_   :: (a -> a) -> Int -> s a -> s a -- map a single element
    -- both return original sequence if index out of bounds
    --
    -- update i y xs@<x0,...,xn-1>
    --   | inBounds xs = <x0,...xi-1,y,xi+1,...,xn-1>
    --   | otherwise = xs
    -- adjust f i xs@<x0,...,xn-1>
    --   | inBounds xs = <x0,...xi-1,f xi,xi+1,...,xn-1>
    --   | otherwise = xs

  mapWithIndex_   :: (Int -> a -> b) -> s a -> s b
  foldrWithIndex_ :: (Int -> a -> b -> b) -> b -> s a -> b
  foldlWithIndex_ :: (b -> Int -> a -> b) -> b -> s a -> b
    -- mapWithIndex f <x0,...,xn-1> = <f 0 x0,...,f (n-1) xn-1>
    -- foldrWithIndex f c <x0,...,xn-1> = 
    --   f 0 x0 (f 1 x1 (... (f (n-1) xn-1 c)))
    -- foldlWithIndex f c <x0,...,xn-1> =
    --   f (...(f (f c 0 x0) 1 x1)...) (n-1) xn-1)

----------------------------------------------------------------------
-- Zips and unzips

  zip_        :: s a -> s b -> s (a,b)
  zip3_       :: s a -> s b -> s c -> s (a,b,c)
    -- zip <x0,...,xn-1> <y0,...,ym-1> = <(x0,y0),...,(xj-1,yj-1)>
    --   where j = min {n,m}
    -- zip3 <x0,...,xn-1> <y0,...,ym-1> <z0,...,zk-1> = 
    --     <(x0,y0,z0),...,(xj-1,yj-1,zj-1)>
    --   where j = min {n,m,k}

  zipWith_    :: (a -> b -> c) -> s a -> s b -> s c
  zipWith3_   :: (a -> b -> c -> d) -> s a -> s b -> s c -> s d
    -- zipWith f xs ys = map (uncurry f) (zip xs ys)
    -- zipWith3 f xs ys zs = map (uncurry f) (zip3 xs ys zs)

  unzip_      :: s (a,b) -> (s a, s b)
  unzip3_     :: s (a,b,c) -> (s a, s b, s c)
    -- unzip xs = (map fst xs, map snd xs)
    -- unzip3 xs = (map fst3 xs, map snd3 xs, map thd3 xs)
    --   where fst3 (x,y,z) = x
    --         snd3 (x,y,z) = y
    --         thd3 (x,y,z) = z

  unzipWith_  :: (a -> b) -> (a -> c) -> s a -> (s b, s c)
  unzipWith3_ :: (a -> b) -> (a -> c) -> (a -> d) -> s a -> (s b, s c, s d)
    -- unzipWith f g xs = (map f xs, map g xs)
    -- unzipWith3 f g h xs = (map f xs, map g xs, map h xs)

----------------------------------------------------------------------
-- Documentation

  instanceName_ :: s a -> String
    -- The name of the module implementing s.

----------------------------------------------------------------------
-- Other possible operations not currently included
{-
  insertAt :: Int -> a -> s a -> s a
    -- adds to front or rear if index out of bounds
    --
    -- insertAt i y xs@<x0,...,xn-1>
    --   | i < 0  = cons y xs
    --   | i >= n = snoc xs y
    --   | otherwise = <x0,...,xi-1,y,xi,...,xn-1> 

  deleteAt :: Int -> s a -> s a
    -- returns original sequence if index out of bounds
    --
    -- deleteAt i xs@<x0,...,xn-1>
    --   | i < 0  = xs
    --   | i >= n = xs
    --   | otherwise = <x0,...,xi-1,xi+1,...,xn-1> 

  insertAt i x s = append before (cons x after)
    where (before, after) = splitAt i s

  deleteAt i s = if i < 0 then s else append before (ltail after)
    where (before, after) = splitAt i s
-}


{-
  Hugs doesn't support the use of qualified names in instance declarations,
  which really does rather spoil the party here. So, as a workaround, we
  use non-clashing names in the class declaration and re-map them to the
  ones that's intended to be used by the programmer here.. 

  Ugly, don't you agree.
-}
empty    :: (Sequence s) => s a
empty = empty_
single   :: (Sequence s) => a -> s a
single x = single_ x
cons     :: (Sequence s) => a -> s a -> s a
cons x y = cons_ x y
snoc     :: (Sequence s) => s a -> a -> s a
snoc x y = snoc_ x y
append   :: (Sequence s) => s a -> s a -> s a
append x y = append_ x y

fromList :: (Sequence s) => [a] -> s a
fromList x = fromList_ x

copy     :: (Sequence s) => Int -> a -> s a          -- returns empty if size is negative
copy x y = copy_ x y
tabulate :: (Sequence s) => Int -> (Int -> a) -> s a -- returns empty if size is negative
tabulate x y = tabulate_ x y

lview    :: (Sequence s) => s a -> Maybe2 a (s a)
lview x = lview_ x
lhead    :: (Sequence s) => s a -> a          -- signals error if sequence is empty
lhead x = lhead_ x
ltail    :: (Sequence s) => s a -> s a        -- returns empty if sequence is empty
ltail x = ltail_ x

rview    :: (Sequence s) => s a -> Maybe2 (s a) a
rview x = rview_ x
rhead    :: (Sequence s) => s a -> a          -- signals error if sequence is empty
rhead x = rhead_ x
rtail    :: (Sequence s) => s a -> s a        -- returns empty if sequence is empty
rtail x = rtail_ x

null     :: (Sequence s) => s a -> Bool
null x  = null_ x
size     :: (Sequence s) => s a -> Int
size x  = size_ x

toList   :: (Sequence s) => s a -> [a]
toList x = toList_ x

concat   :: (Sequence s) => s (s a) -> s a
concat x = concat_ x

reverse  :: (Sequence s) => s a -> s a
reverse x = reverse_ x
reverseOnto :: (Sequence s) => s a -> s a -> s a
reverseOnto x y = reverseOnto_ x y

map      :: (Sequence s) => (a -> b) -> s a -> s b
map x y = map_ x y
concatMap:: (Sequence s) => (a -> s b) -> s a -> s b
concatMap x y = concatMap_ x y

foldr    :: (Sequence s) => (a -> b -> b) -> b -> s a -> b
foldr x y z = foldr_ x y z
foldl    :: (Sequence s) => (b -> a -> b) -> b -> s a -> b
foldl x y z = foldl_ x y z

foldr1   :: (Sequence s) => (a -> a -> a) -> s a -> a  -- signals error if sequence is empty
foldr1 x y = foldr1_ x y
foldl1   :: (Sequence s) => (a -> a -> a) -> s a -> a  -- signals error if sequence is empty
foldl1 x y = foldl1_ x y

reducer  :: (Sequence s) => (a -> a -> a) -> a -> s a -> a
reducer x y z = reducer_ x y z
reducel  :: (Sequence s) => (a -> a -> a) -> a -> s a -> a
reducel x y z = reducel_ x y z
reduce1  :: (Sequence s) => (a -> a -> a) -> s a -> a  -- signals error if sequence is empty
reduce1 x y = reduce1_ x y

take       :: (Sequence s) => Int -> s a -> s a
take x y = take_ x y
drop       :: (Sequence s) => Int -> s a -> s a
drop x y = drop_ x y
splitAt    :: (Sequence s) => Int -> s a -> (s a, s a)
splitAt x y = splitAt_ x y
  
subseq     :: (Sequence s) => Int -> Int -> s a -> s a
subseq x y z = subseq_ x y z

filter     :: (Sequence s) => (a -> Bool) -> s a -> s a
filter x y = filter_ x y
partition  :: (Sequence s) => (a -> Bool) -> s a -> (s a, s a)
partition x y = partition_ x y

takeWhile  :: (Sequence s) => (a -> Bool) -> s a -> s a
takeWhile x y = takeWhile_ x y
dropWhile  :: (Sequence s) => (a -> Bool) -> s a -> s a
dropWhile x y = dropWhile_ x y
splitWhile :: (Sequence s) => (a -> Bool) -> s a -> (s a, s a)
splitWhile x y = splitWhile_ x y

inBounds :: (Sequence s) => s a -> Int -> Bool
inBounds x y = inBounds_ x y

lookup   :: (Sequence s) => s a -> Int -> a         -- signals error if index out of bounds
lookup x y = lookup_ x y
lookupM  :: (Sequence s) => s a -> Int -> Maybe a
lookupM x y = lookupM_ x y
lookupWithDefault :: (Sequence s) => a -> s a -> Int -> a
lookupWithDefault x y z = lookupWithDefault_ x y z

update   :: (Sequence s) => Int -> a -> s a -> s a
update x y z = update_ x y z
adjust   :: (Sequence s) => (a -> a) -> Int -> s a -> s a -- map a single element
adjust x y z = adjust_ x y z

mapWithIndex   :: (Sequence s) => (Int -> a -> b) -> s a -> s b
mapWithIndex x y = mapWithIndex_ x y
foldrWithIndex :: (Sequence s) => (Int -> a -> b -> b) -> b -> s a -> b
foldrWithIndex x y z = foldrWithIndex_ x y z
foldlWithIndex :: (Sequence s) => (b -> Int -> a -> b) -> b -> s a -> b
foldlWithIndex x y z = foldlWithIndex_ x y z

zip        :: (Sequence s) => s a -> s b -> s (a,b)
zip x y = zip_ x y
zip3       :: (Sequence s) => s a -> s b -> s c -> s (a,b,c)
zip3 x y z = zip3_ x y z

zipWith    :: (Sequence s) => (a -> b -> c) -> s a -> s b -> s c
zipWith x y z = zipWith_ x y z
zipWith3   :: (Sequence s) => (a -> b -> c -> d) -> s a -> s b -> s c -> s d
zipWith3 x y z w = zipWith3_ x y z w

unzip      :: (Sequence s) => s (a,b) -> (s a, s b)
unzip x = unzip_ x
unzip3     :: (Sequence s) => s (a,b,c) -> (s a, s b, s c)
unzip3 x = unzip3_ x

unzipWith  :: (Sequence s) => (a -> b) -> (a -> c) -> s a -> (s b, s c)
unzipWith x y z = unzipWith_ x y z
unzipWith3 :: (Sequence s) => (a -> b) -> (a -> c) -> (a -> d) -> s a -> (s b, s c, s d)
unzipWith3 x y z w = unzipWith3_ x y z w

instanceName :: (Sequence s) => s a -> String
instanceName x = instanceName_ x

