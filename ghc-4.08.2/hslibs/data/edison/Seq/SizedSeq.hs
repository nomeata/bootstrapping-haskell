-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module SizedSeq (
    -- generic adaptor for sequences to keep track of the current size
    Sized, -- Sized s instance of Sequence, Functor, Monad, MonadPlus

    -- sequence operations
    empty,single,cons,snoc,append,lview,lhead,ltail,rview,rhead,rtail,
    null,size,concat,reverse,reverseOnto,fromList,toList,
    map,concatMap,foldr,foldl,foldr1,foldl1,reducer,reducel,reduce1,
    copy,tabulate,inBounds,lookup,lookupM,lookupWithDefault,update,adjust,
    mapWithIndex,foldrWithIndex,foldlWithIndex,
    take,drop,splitAt,subseq,filter,partition,takeWhile,dropWhile,splitWhile,
    zip,zip3,zipWith,zipWith3,unzip,unzip3,unzipWith,unzipWith3,

    -- documentation
    moduleName,instanceName,

    -- other supported operations
    fromSeq,toSeq,

    -- re-export view type from EdisonPrelude for convenience
    Maybe2(Just2,Nothing2)
) where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import EdisonPrelude(Maybe2(Just2,Nothing2))
import Sequence ( Sequence(..) )
import qualified Sequence as S
import SequenceDefaults -- only used by concatMap
import Monad
import QuickCheck

-- This module defines a sequence adaptor Sized s.
-- If s is a sequence type constructor, then Sized s
-- is a sequence type constructor that is identical to s,
-- except that it also keeps track of the current size of
-- each sequence.

-- signatures for exported functions
moduleName     :: String
instanceName   :: Sequence s => Sized s a -> String
empty          :: Sequence s => Sized s a
single         :: Sequence s => a -> Sized s a
cons           :: Sequence s => a -> Sized s a -> Sized s a
snoc           :: Sequence s => Sized s a -> a -> Sized s a
append         :: Sequence s => Sized s a -> Sized s a -> Sized s a
lview          :: Sequence s => Sized s a -> Maybe2 a (Sized s a)
lhead          :: Sequence s => Sized s a -> a
ltail          :: Sequence s => Sized s a -> Sized s a
rview          :: Sequence s => Sized s a -> Maybe2 (Sized s a) a
rhead          :: Sequence s => Sized s a -> a
rtail          :: Sequence s => Sized s a -> Sized s a
null           :: Sequence s => Sized s a -> Bool
size           :: Sequence s => Sized s a -> Int
concat         :: Sequence s => Sized s (Sized s a) -> Sized s a
reverse        :: Sequence s => Sized s a -> Sized s a
reverseOnto    :: Sequence s => Sized s a -> Sized s a -> Sized s a
fromList       :: Sequence s => [a] -> Sized s a
toList         :: Sequence s => Sized s a -> [a]
map            :: Sequence s => (a -> b) -> Sized s a -> Sized s b
concatMap      :: Sequence s => (a -> Sized s b) -> Sized s a -> Sized s b
foldr          :: Sequence s => (a -> b -> b) -> b -> Sized s a -> b
foldl          :: Sequence s => (b -> a -> b) -> b -> Sized s a -> b
foldr1         :: Sequence s => (a -> a -> a) -> Sized s a -> a
foldl1         :: Sequence s => (a -> a -> a) -> Sized s a -> a
reducer        :: Sequence s => (a -> a -> a) -> a -> Sized s a -> a
reducel        :: Sequence s => (a -> a -> a) -> a -> Sized s a -> a
reduce1        :: Sequence s => (a -> a -> a) -> Sized s a -> a
copy           :: Sequence s => Int -> a -> Sized s a
tabulate       :: Sequence s => Int -> (Int -> a) -> Sized s a
inBounds       :: Sequence s => Sized s a -> Int -> Bool
lookup         :: Sequence s => Sized s a -> Int -> a
lookupM        :: Sequence s => Sized s a -> Int -> Maybe a
lookupWithDefault :: Sequence s => a -> Sized s a -> Int -> a
update         :: Sequence s => Int -> a -> Sized s a -> Sized s a
adjust         :: Sequence s => (a -> a) -> Int -> Sized s a -> Sized s a
mapWithIndex   :: Sequence s => (Int -> a -> b) -> Sized s a -> Sized s b
foldrWithIndex :: Sequence s => (Int -> a -> b -> b) -> b -> Sized s a -> b
foldlWithIndex :: Sequence s => (b -> Int -> a -> b) -> b -> Sized s a -> b
take           :: Sequence s => Int -> Sized s a -> Sized s a
drop           :: Sequence s => Int -> Sized s a -> Sized s a
splitAt        :: Sequence s => Int -> Sized s a -> (Sized s a, Sized s a)
subseq         :: Sequence s => Int -> Int -> Sized s a -> Sized s a
filter         :: Sequence s => (a -> Bool) -> Sized s a -> Sized s a
partition      :: Sequence s => (a -> Bool) -> Sized s a -> (Sized s a, Sized s a)
takeWhile      :: Sequence s => (a -> Bool) -> Sized s a -> Sized s a
dropWhile      :: Sequence s => (a -> Bool) -> Sized s a -> Sized s a
splitWhile     :: Sequence s => (a -> Bool) -> Sized s a -> (Sized s a, Sized s a)
zip            :: Sequence s => Sized s a -> Sized s b -> Sized s (a,b)
zip3           :: Sequence s => Sized s a -> Sized s b -> Sized s c -> Sized s (a,b,c)
zipWith        :: Sequence s => (a -> b -> c) -> Sized s a -> Sized s b -> Sized s c
zipWith3       :: Sequence s => (a -> b -> c -> d) -> Sized s a -> Sized s b -> Sized s c -> Sized s d
unzip          :: Sequence s => Sized s (a,b) -> (Sized s a, Sized s b)
unzip3         :: Sequence s => Sized s (a,b,c) -> (Sized s a, Sized s b, Sized s c)
unzipWith      :: Sequence s => (a -> b) -> (a -> c) -> Sized s a -> (Sized s b, Sized s c)
unzipWith3     :: Sequence s => (a -> b) -> (a -> c) -> (a -> d) -> Sized s a -> (Sized s b, Sized s c, Sized s d)

-- bonus functions, not in Sequence signature
fromSeq        :: Sequence s => s a -> Sized s a
toSeq          :: Sequence s => Sized s a -> s a



moduleName = "SizedSeq"
instanceName (N n s) = "SizedSeq(" ++ S.instanceName s ++ ")"

data Sized s a = N !Int (s a)

fromSeq xs = N (S.size xs) xs
toSeq (N n xs) = xs

empty = N 0 S.empty
single x = N 1 (S.single x)
cons x (N n xs) = N (n+1) (S.cons x xs)
snoc (N n xs) x = N (n+1) (S.snoc xs x)
append (N m xs) (N n ys) = N (m+n) (S.append xs ys)

lview (N n xs) = case S.lview xs of
                   Nothing2 -> Nothing2
                   Just2 x xs -> Just2 x (N (n-1) xs)

lhead (N n xs) = S.lhead xs

ltail (N 0 xs) = empty
ltail (N n xs) = N (n-1) (S.ltail xs)

rview (N n xs) = case S.rview xs of
                   Nothing2 -> Nothing2
                   Just2 xs x -> Just2 (N (n-1) xs) x
 
rhead (N n xs) = S.rhead xs

rtail (N 0 xs) = empty
rtail (N n xs) = N (n-1) (S.rtail xs)

null (N n xs) = n == 0
size (N n xs) = n
concat (N n xss) = fromSeq (S.concat (S.map toSeq xss))
reverse (N n xs) = N n (S.reverse xs)
reverseOnto (N m xs) (N n ys) = N (m+n) (S.reverseOnto xs ys)
fromList = fromSeq . S.fromList
toList (N n xs) = S.toList xs
map f (N n xs) = N n (S.map f xs)

concatMap = concatMapUsingFoldr -- only function that uses a default

foldr f e (N n xs) = S.foldr f e xs
foldl f e (N n xs) = S.foldl f e xs
foldr1 f (N n xs) = S.foldr1 f xs
foldl1 f (N n xs) = S.foldl1 f xs
reducer f e (N n xs) = S.reducer f e xs
reducel f e (N n xs) = S.reducel f e xs
reduce1 f (N n xs) = S.reduce1 f xs

copy n x 
    | n <= 0 = empty
    | otherwise = N n (S.copy n x)

tabulate n f
    | n <= 0 = empty
    | otherwise = N n (S.tabulate n f)

inBounds (N n xs) i = (i >= 0) && (i < n)
lookup (N n xs) = S.lookup xs
lookupM (N n xs) = S.lookupM xs
lookupWithDefault d (N n xs) = S.lookupWithDefault d xs
update i x (N n xs) = N n (S.update i x xs)
adjust f i (N n xs) = N n (S.adjust f i xs)
mapWithIndex f (N n xs) = N n (S.mapWithIndex f xs)
foldrWithIndex f e (N n xs) = S.foldrWithIndex f e xs
foldlWithIndex f e (N n xs) = S.foldlWithIndex f e xs

take i original@(N n xs)
  | i <= 0 = empty
  | i >= n = original
  | otherwise = N i (S.take i xs)

drop i original@(N n xs)
  | i <= 0 = original
  | i >= n = empty
  | otherwise = N (n-i) (S.drop i xs)

splitAt i original@(N n xs)
  | i <= 0 = (empty, original)
  | i >= n = (original, empty)
  | otherwise = let (ys,zs) = S.splitAt i xs
                in (N i ys, N (n-i) zs)

subseq i len original@(N n xs)
  | i <= 0 = take len original
  | i >= n || len <= 0 = empty
  | i+len >= n = N (n-i) (S.drop i xs)
  | otherwise = N len (S.subseq i len xs)

filter p = fromSeq . S.filter p . toSeq

partition p (N n xs) = (N m ys, N (n-m) zs)
  where (ys,zs) = S.partition p xs
        m = S.size ys

takeWhile p = fromSeq . S.takeWhile p . toSeq
dropWhile p = fromSeq . S.dropWhile p . toSeq

splitWhile p (N n xs) = (N m ys, N (n-m) zs)
  where (ys,zs) = S.splitWhile p xs
        m = S.size ys

zip (N m xs) (N n ys) = N (min m n) (S.zip xs ys)
zip3 (N l xs) (N m ys) (N n zs) = N (min l (min m n)) (S.zip3 xs ys zs)

zipWith f (N m xs) (N n ys) = N (min m n) (S.zipWith f xs ys)
zipWith3 f (N l xs) (N m ys) (N n zs) = N (min l (min m n)) (S.zipWith3 f xs ys zs)

unzip (N n xys) = (N n xs, N n ys)
  where (xs,ys) = S.unzip xys

unzip3 (N n xyzs) = (N n xs, N n ys, N n zs)
  where (xs,ys,zs) = S.unzip3 xyzs

unzipWith f g (N n xys) = (N n xs, N n ys)
  where (xs,ys) = S.unzipWith f g xys

unzipWith3 f g h (N n xyzs) = (N n xs, N n ys, N n zs)
  where (xs,ys,zs) = S.unzipWith3 f g h xyzs

-- instances

instance Sequence s => Sequence (Sized s) where
  {empty_ = empty; single_ = single; cons_ = cons; snoc_ = snoc;
   append_ = append; lview_ = lview; lhead_ = lhead; ltail_ = ltail;
   rview_ = rview; rhead_ = rhead; rtail_ = rtail; null_ = null;
   size_ = size; concat_ = concat; reverse_ = reverse; 
   reverseOnto_ = reverseOnto; fromList_ = fromList; toList_ = toList;
   map_ = map; concatMap_ = concatMap; foldr_ = foldr; foldl_ = foldl;
   foldr1_ = foldr1; foldl1_ = foldl1; reducer_ = reducer; 
   reducel_ = reducel; reduce1_ = reduce1; copy_ = copy; 
   tabulate_ = tabulate; inBounds_ = inBounds; lookup_ = lookup;
   lookupM_ = lookupM; lookupWithDefault_ = lookupWithDefault;
   update_ = update; adjust_ = adjust; mapWithIndex_ = mapWithIndex;
   foldrWithIndex_ = foldrWithIndex; foldlWithIndex_ = foldlWithIndex;
   take_ = take; drop_ = drop; splitAt_ = splitAt; subseq_ = subseq;
   filter_ = filter; partition_ = partition; takeWhile_ = takeWhile;
   dropWhile_ = dropWhile; splitWhile_ = splitWhile; zip_ = zip;
   zip3_ = zip3; zipWith_ = zipWith; zipWith3_ = zipWith3; unzip_ = unzip;
   unzip3_ = unzip3; unzipWith_ = unzipWith; unzipWith3_ = unzipWith3;
   instanceName_ = instanceName}

instance Sequence s => Functor (Sized s) where
  fmap = map

instance Sequence s => Monad (Sized s) where
  return = single
  xs >>= k = concatMap k xs

instance Sequence s => MonadPlus (Sized s) where
  mplus = append
  mzero = empty


instance Eq (s a) => Eq (Sized s a) where
  (N m xs) == (N n ys) = (m == n) && (xs == ys)
  -- this is probably identical to the code that would be
  -- generated by "deriving (Eq)", but I wanted to be *sure*
  -- that the sizes were compared before the inner sequences

instance (Sequence s, Show (s a)) => Show (Sized s a) where
  show xs = show (toSeq xs)

instance (Sequence s, Arbitrary (s a)) => Arbitrary (Sized s a) where
  arbitrary = do xs <- arbitrary
                 return (fromSeq xs)

  coarbitrary xs = coarbitrary (toSeq xs)

