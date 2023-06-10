-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Collection (
{-
    -- non-observable classes
    CollX(..),
    OrdCollX(..),
    SetX(..),
    OrdSetX(..),

    -- observable classes
    Coll(..),
    OrdColl(..),
    Set(..),
    OrdSet(..),

    -- specialize all the sequence operations to lists
    fromList,
    insertList,
    unionList,
    deleteList,
    unsafeFromOrdList,
    toList,
    lookupList,
    toOrdList,
    fromListWith,
    insertListWith,
    unionListWith,
-}
    module Collection,

    -- re-export view type from EdisonPrelude for convenience
    Maybe2(..)
) where

import Prelude hiding (null,foldr,foldl,foldr1,foldl1,lookup,filter)
import EdisonPrelude(Maybe2(..))
import Sequence(Sequence)
import ListSeq()

class Eq a => CollX c a where
  empty_         :: c a

    -- the empty collection

  single_        :: a -> c a

    -- create a singleton collection

  fromSeq_       :: Sequence seq => seq a -> c a

    -- convert a sequence to a collection.  For sets, it is unspecified
    -- which element is kept in case of duplicates.

  insert_        :: a -> c a -> c a
  insertSeq_     :: Sequence seq => seq a -> c a -> c a

    -- insert an element or a sequence of elements into a collection.  For
    -- sets, insert keeps the new element in case of duplicates, but
    -- insertSeq keeps an unspecified element.

  union_         :: c a -> c a -> c a
  unionSeq_      :: Sequence seq => seq (c a) -> c a

    -- merge two collections or a sequence of collections.  For sets, it
    -- is unspecified which element is kept in case of duplicates.

  delete_        :: a -> c a -> c a
  deleteAll_     :: a -> c a -> c a

    -- delete a single occurrence or all occurrences of the given
    -- element from a collection.  For sets, these will be the same,
    -- but for bags they may be different.  For delete on bags, it
    -- is unspecified which of several duplicate elements is deleted.

  deleteSeq_     :: Sequence seq => seq a -> c a -> c a

    -- delete a single occurrence of each of the given elements from
    -- a collection.  For bags, there may be multiple occurrences of a
    -- given element in the collection, in which case it is unspecified
    -- which is deleted.

  null_          :: c a -> Bool
  size_          :: c a -> Int

    -- test whether the collection is empty, or return the number of
    -- elements in the collection.

  member_        :: c a -> a -> Bool
  count_         :: c a -> a -> Int

    -- test whether the given element is in the collection, or how many
    -- duplicates are in the collection.  (For sets, count will always
    -- return 0 or 1.)

  instanceName_  :: c a -> String

    -- the name of the module implementing c

class (CollX c a, Ord a) => OrdCollX c a where

  deleteMin_         :: c a -> c a
  deleteMax_         :: c a -> c a

    -- delete the minimum or maximum element from the collection.
    -- If there is more than one minimum or maximum, it is unspecified which
    -- is deleted.

  unsafeInsertMin_   :: a -> c a -> c a
  unsafeInsertMax_   :: c a -> a -> c a

    -- insert an element that is guaranteed to be <= or >= any existing
    -- elements in the collection.  (For sets, this precondition is
    -- strengthened to < or >.)

  unsafeFromOrdSeq_  :: Sequence seq => seq a -> c a

    -- convert a sequence in non-decreasing order into a collection.
    -- (For sets, the sequence must be in increasing order.)

  unsafeAppend_      :: c a -> c a -> c a

    -- union two collections where every element in the first
    -- collection is <= every element in the second collection.
    -- (For sets, this precondition is strengthened to <.)

  filterLT_          :: a -> c a -> c a
  filterLE_          :: a -> c a -> c a
  filterGT_          :: a -> c a -> c a
  filterGE_          :: a -> c a -> c a

    -- filterLT x xs = filter (< x) xs
    -- filterLE x xs = filter (<= x) xs
    -- filterGT x xs = filter (> x) xs
    -- filterGE x xs = filter (>= x) xs

  partitionLT_GE_    :: a -> c a -> (c a, c a)
  partitionLE_GT_    :: a -> c a -> (c a, c a)
  partitionLT_GT_    :: a -> c a -> (c a, c a)

    -- partitionLT_GE x xs = partition (< x) xs
    -- partitionLE_GT x xs = partition (<= x) xs
    -- partitionLT_GT x xs = (filterLT x xs, filterGT x xs)

class CollX c a => SetX c a where

  intersect_  :: c a -> c a -> c a
  difference_ :: c a -> c a -> c a

    -- return the intersection or difference of two sets.  For intersect,
    -- it is unspecified which of the two elements is kept.

  subset_     :: c a -> c a -> Bool    
  subsetEq_   :: c a -> c a -> Bool

    -- test whether the first set is a proper subset of the second,
    -- or whether it is a (possibly improper) subset.

class (OrdCollX c a, SetX c a) => OrdSetX c a
  -- no methods


class CollX c a => Coll c a where
  toSeq_     :: Sequence seq => c a -> seq a

    -- list the elements of the collection in an unspecified order

  lookup_    :: c a -> a -> a
  lookupM_   :: c a -> a -> Maybe a
  lookupAll_ :: Sequence seq => c a -> a -> seq a
  lookupWithDefault_ :: a -> c a -> a -> a

    -- lookup one or more elements equal to the given element.
    -- if there is none, then lookup signals an error, lookupM returns 
    -- Nothing, lookupAll returns empty, and lookupWithDefault d returns d.
    -- if there are mulitiple copies, then lookup/lookupM/lookupWithDefault
    -- return an unspecified one, and lookupAll returns them all, but
    -- in an unspecified order.

  fold_      :: (a -> b -> b) -> b -> c a -> b
  fold1_     :: (a -> a -> a) -> c a -> a

    -- fold over all the elements in a collection in unspecified order.
    -- (fold1 signals an error if the collection is empty.)

  filter_    :: (a -> Bool) -> c a -> c a
  partition_ :: (a -> Bool) -> c a -> (c a, c a)

    -- filter removes all elements not satisfying the predicate.
    -- partition returns two collections, one containing all the
    -- elements satisfying the predicate, and one containing all the
    -- elements not satisfying the predicate.

class (Coll c a, OrdCollX c a) => OrdColl c a where

  minView_   :: c a -> Maybe2 a (c a)
  minElem_   :: c a -> a

    -- return the minimum element in the collection, together with
    -- the collection without that element in the case of minView.
    -- If there are multiple copies of the minimum element, it is
    -- unspecified which is chosen.  Note that minView, minElem, and
    -- deleteMin may make different choices!

  maxView_   :: c a -> Maybe2 (c a) a
  maxElem_   :: c a -> a

    -- return the maximum element in the collection, together with
    -- the collection without that element in the case of maxView.
    -- If there are multiple copies of the maximum element, it is
    -- unspecified which is chosen.  Note that maxView, maxElem, and
    -- deleteMax may make different choices!

  foldr_     :: (a -> b -> b) -> b -> c a -> b
  foldl_     :: (b -> a -> b) -> b -> c a -> b

    -- fold across the elements in non-decreasing order.
    -- (For sets, this will always be increasing order.)

  foldr1_    :: (a -> a -> a) -> c a -> a
  foldl1_    :: (a -> a -> a) -> c a -> a

    -- fold across the elements in non-decreasing order, or signal an
    -- error if the collection is empty.  (For sets, this will always be 
    -- increasing order.)

  toOrdSeq_  :: Sequence seq => c a -> seq a

    -- list the elements in non-decreasing order.

class (Coll c a, SetX c a) => Set c a where

  -- WARNING: Each of the following "With" functions is unsafe.  The combining
  -- functions are required to satisfy the precondition that, given two
  -- equal elements, they return a third element equal to the other two.

  fromSeqWith_    :: Sequence seq => (a -> a -> a) -> seq a -> c a

    -- same as fromSeq but with a combining function to resolve duplicates.
    -- Usually, the combining function should be associative.  If not,
    -- the elements will be combined left-to-right, but with an
    -- unspecified associativity.  For example, if x == y == z,
    -- then fromSeqWith (+) [x,y,z] equals either
    --     single (x + (y + z))
    -- or
    --     single ((x + y) + z)

  insertWith_     :: (a -> a -> a) -> a -> c a -> c a
  insertSeqWith_  :: Sequence seq => (a -> a -> a) -> seq a -> c a -> c a

    -- same as insert/insertSeq but with a combining function to resolve 
    -- duplicates.  The comments about associativity apply to insertSeqWith.

  unionl_         :: c a -> c a -> c a
  unionr_         :: c a -> c a -> c a

    -- unionl = unionWith (\x y -> x)
    -- unionr = unionWith (\x y -> y)
    
  unionWith_      :: (a -> a -> a) -> c a -> c a -> c a
  unionSeqWith_   :: Sequence seq => (a -> a -> a) -> seq (c a) -> c a

    -- same as union/unionSeq but with a combining function to resolve
    -- duplicates.  The comments about associativity apply to unionSeqWith.

  intersectWith_  :: (a -> a -> a) -> c a -> c a -> c a

    -- same as intersect but with a combining function to resolve duplicates.

class (OrdColl c a, Set c a) => OrdSet c a
  -- no methods


-- specialize all the sequence operations to lists

fromList          :: CollX c a => [a] -> c a
insertList        :: CollX c a => [a] -> c a -> c a
unionList         :: CollX c a => [c a] -> c a
deleteList        :: CollX c a => [a] -> c a -> c a
unsafeFromOrdList :: OrdCollX c a => [a] -> c a
toList            :: Coll c a => c a -> [a]
lookupList        :: Coll c a => c a -> a -> [a]
toOrdList         :: OrdColl c a => c a -> [a]
fromListWith      :: Set c a => (a -> a -> a) -> [a] -> c a
insertListWith    :: Set c a => (a -> a -> a) -> [a] -> c a -> c a
unionListWith     :: Set c a => (a -> a -> a) -> [c a] -> c a

fromList = fromSeq
insertList = insertSeq
unionList = unionSeq
deleteList = deleteSeq
unsafeFromOrdList = unsafeFromOrdSeq
toList = toSeq
lookupList = lookupAll
toOrdList = toOrdSeq
fromListWith = fromSeqWith
insertListWith = insertSeqWith
unionListWith = unionSeqWith

{-
 See comment in Seq/Sequence.hs for why this kind of wrappery
 is (unfortunately) needed.
-}
empty         :: CollX c a => c a
empty = empty_

single        :: CollX c a => a -> c a
single x = single_ x

fromSeq       :: (CollX c a, Sequence seq) => seq a -> c a
fromSeq x = fromSeq_ x

insert        :: CollX c a => a -> c a -> c a
insert x y = insert_ x y

insertSeq     :: (CollX c a, Sequence seq) => seq a -> c a -> c a
insertSeq x y = insertSeq_ x y

union         :: CollX c a => c a -> c a -> c a
union x y = union_ x y
unionSeq      :: (CollX c a, Sequence seq) => seq (c a) -> c a
unionSeq x = unionSeq_ x

delete        :: CollX c a => a -> c a -> c a
delete x y = delete_ x y
deleteAll     :: CollX c a => a -> c a -> c a
deleteAll x y = deleteAll_ x y

deleteSeq     :: (CollX c a, Sequence seq) => seq a -> c a -> c a
deleteSeq x y = deleteSeq_ x y

null          :: CollX c a => c a -> Bool
null x = null_ x

size          :: CollX c a => c a -> Int
size x = size_ x

member        :: CollX c a => c a -> a -> Bool
member x y = member_ x y
count         :: CollX c a => c a -> a -> Int
count x y = count_ x y

instanceName  :: CollX c a => c a -> String
instanceName x = instanceName_ x

-- OrdCollX
deleteMin         :: OrdCollX c a => c a -> c a
deleteMin x = deleteMin_ x

deleteMax         :: OrdCollX c a => c a -> c a
deleteMax x = deleteMax_ x 

unsafeInsertMin   :: OrdCollX c a => a -> c a -> c a
unsafeInsertMin x y = unsafeInsertMin_ x y

unsafeInsertMax   :: OrdCollX c a => c a -> a -> c a
unsafeInsertMax x y = unsafeInsertMax_ x y

unsafeFromOrdSeq  :: (OrdCollX c a, Sequence seq) => seq a -> c a
unsafeFromOrdSeq x = unsafeFromOrdSeq_ x

unsafeAppend      :: OrdCollX c a => c a -> c a -> c a
unsafeAppend x y = unsafeAppend_ x y

filterLT          :: OrdCollX c a => a -> c a -> c a
filterLT x y = filterLT_ x y

filterLE          :: OrdCollX c a => a -> c a -> c a
filterLE x y = filterLE_ x y

filterGT          :: OrdCollX c a => a -> c a -> c a
filterGT x y = filterGT_ x y

filterGE          :: OrdCollX c a => a -> c a -> c a
filterGE x y = filterGE_ x y

partitionLT_GE    :: OrdCollX c a => a -> c a -> (c a, c a)
partitionLT_GE x y = partitionLT_GE_ x y

partitionLE_GT    :: OrdCollX c a => a -> c a -> (c a, c a)
partitionLE_GT x y = partitionLE_GT_ x y

partitionLT_GT    :: OrdCollX c a => a -> c a -> (c a, c a)
partitionLT_GT x y = partitionLT_GT_ x y

-- Set
intersect  :: SetX c a  => c a -> c a -> c a
intersect x y = intersect_ x y

difference :: SetX c a  => c a -> c a -> c a
difference x y = difference_ x y

subset     :: SetX c a  => c a -> c a -> Bool    
subset x y = subset_ x y
subsetEq   :: SetX c a  => c a -> c a -> Bool
subsetEq x y = subsetEq_ x y

--Coll c a 
toSeq     :: (Coll c a, Sequence seq) => c a -> seq a
toSeq x = toSeq_ x

lookup    :: Coll c a => c a -> a -> a
lookup x y = lookup_ x y

lookupM   :: Coll c a => c a -> a -> Maybe a
lookupM x y = lookupM_ x y 

lookupAll :: (Coll c a, Sequence seq) => c a -> a -> seq a
lookupAll x y = lookupAll_ x y

lookupWithDefault :: Coll c a => a -> c a -> a -> a
lookupWithDefault x y z = lookupWithDefault_ x y z

fold      :: Coll c a => (a -> b -> b) -> b -> c a -> b
fold x y z = fold_ x y z

fold1     :: Coll c a => (a -> a -> a) -> c a -> a
fold1 x y = fold1_ x y

filter    :: Coll c a => (a -> Bool) -> c a -> c a
filter x y = filter_ x y

partition :: Coll c a => (a -> Bool) -> c a -> (c a, c a)
partition x y = partition_ x y

-- OrdColl

minView   :: OrdColl c a => c a -> Maybe2 a (c a)
minView x = minView_ x 

minElem   :: OrdColl c a => c a -> a
minElem x = minElem_ x 


maxView   :: OrdColl c a => c a -> Maybe2 (c a) a
maxView x  = maxView_ x

maxElem   :: OrdColl c a => c a -> a
maxElem x = maxElem_ x

foldr     :: OrdColl c a => (a -> b -> b) -> b -> c a -> b
foldr x y z = foldr_ x y z

foldl     :: OrdColl c a => (b -> a -> b) -> b -> c a -> b
foldl x y z = foldl_ x y z

foldr1    :: OrdColl c a => (a -> a -> a) -> c a -> a
foldr1 x y = foldr1_ x y

foldl1    :: OrdColl c a => (a -> a -> a) -> c a -> a
foldl1 x y = foldl1_ x y

toOrdSeq  :: (OrdColl c a, Sequence seq) => c a -> seq a
toOrdSeq x = toOrdSeq_ x


fromSeqWith  :: (Set c a, Sequence seq) => (a -> a -> a) -> seq a -> c a
fromSeqWith x y = fromSeqWith_ x y

insertWith     :: Set c a => (a -> a -> a) -> a -> c a -> c a
insertWith x y z = insertWith_ x y z

insertSeqWith  :: (Set c a, Sequence seq) => (a -> a -> a) -> seq a -> c a -> c a
insertSeqWith x y z = insertSeqWith_ x y z

unionl         :: Set c a => c a -> c a -> c a
unionl x y = unionl_ x y

unionr         :: Set c a => c a -> c a -> c a
unionr x y = unionr_ x y
    
unionWith      :: Set c a => (a -> a -> a) -> c a -> c a -> c a
unionWith x y = unionWith_ x y

unionSeqWith   :: (Set c a , Sequence seq) => (a -> a -> a) -> seq (c a) -> c a
unionSeqWith x y = unionSeqWith_ x y

intersectWith  :: Set c a => (a -> a -> a) -> c a -> c a -> c a
intersectWith x y z = intersectWith_ x y z

