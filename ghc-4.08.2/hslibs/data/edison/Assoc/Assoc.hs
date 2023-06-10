-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Assoc ( -- associative collections
{-
    -- non-observable classes
    AssocX(..),
    OrdAssocX(..),
    FiniteMapX(..),
    OrdFiniteMapX(..),

    -- observable classes
    Assoc(..),
    OrdAssoc(..),
    FiniteMap(..),
    OrdFiniteMap(..),

    -- specialize sequence operations to lists
    fromList,
    insertList,
    unionList,
    deleteList,
    lookupList,
    elementsList,
    unsafeFromOrdList,
    fromListWith,
    fromListWithKey,
    insertListWith,
    insertListWithKey,
    unionListWith,
    toList,
    keysList,
    toOrdList,
    unionListWithKey,
-}
    module Assoc,

    -- re-export view types from EdisonPrelude for convenience
    Maybe2(..),
    Maybe3(..)
) where

import Prelude hiding (null,map,lookup,foldr,foldl,foldr1,foldl1,filter)

import EdisonPrelude(Maybe2(..), Maybe3(..))

import Sequence(Sequence)
import ListSeq()

-- class (Eq k, Functor (m k)) => AssocX m k
class Eq k => AssocX m k where
  empty_         :: m k a

  single_        :: k -> a -> m k a
  fromSeq_       :: Sequence seq => seq (k,a) -> m k a

  insert_        :: k -> a -> m k a -> m k a
  insertSeq_     :: Sequence seq => seq (k,a) -> m k a -> m k a

  union_         :: m k a -> m k a -> m k a
  unionSeq_      :: Sequence seq => seq (m k a) -> m k a

  delete_        :: k -> m k a -> m k a
  deleteAll_     :: k -> m k a -> m k a
  deleteSeq_     :: Sequence seq => seq k -> m k a -> m k a

  null_          :: m k a -> Bool
  size_          :: m k a -> Int

  member_        :: m k a -> k -> Bool
  count_         :: m k a -> k -> Int

  lookup_        :: m k a -> k -> a
  lookupM_       :: m k a -> k -> Maybe a
  lookupAll_     :: Sequence seq => m k a -> k -> seq a
  lookupWithDefault_ :: a -> m k a -> k -> a

  adjust_        :: (a -> a) -> k -> m k a -> m k a
  adjustAll_     :: (a -> a) -> k -> m k a -> m k a

  -- only because can't yet put Functor as superclass
  map_           :: (a -> b) -> m k a -> m k b

  fold_          :: (a -> b -> b) -> b -> m k a -> b
  fold1_         :: (a -> a -> a) -> m k a -> a

  filter_        :: (a -> Bool) -> m k a -> m k a
  partition_     :: (a -> Bool) -> m k a -> (m k a, m k a)
  
  elements_      :: Sequence seq => m k a -> seq a

  instanceName_  :: m k a -> String

class (AssocX m k, Ord k) => OrdAssocX m k where
  minView_           :: m k a -> Maybe2 a (m k a)
  minElem_           :: m k a -> a
  deleteMin_         :: m k a -> m k a
  unsafeInsertMin_   :: k -> a -> m k a -> m k a

  maxView_           :: m k a -> Maybe2 (m k a) a
  maxElem_           :: m k a -> a
  deleteMax_         :: m k a -> m k a
  unsafeInsertMax_   :: m k a -> k -> a -> m k a

  foldr_             :: (a -> b -> b) -> b -> m k a -> b
  foldl_             :: (b -> a -> b) -> b -> m k a -> b

  foldr1_            :: (a -> a -> a) -> m k a -> m k a
  foldl1_            :: (a -> a -> a) -> m k a -> m k a

  unsafeFromOrdSeq_  :: Sequence seq => seq (k,a) -> m k a
  unsafeAppend_      :: m k a -> m k a -> m k a

  filterLT_          :: k -> m k a -> m k a
  filterLE_          :: k -> m k a -> m k a
  filterGT_          :: k -> m k a -> m k a
  filterGE_          :: k -> m k a -> m k a

  partitionLT_GE_    :: k -> m k a -> (m k a, m k a)
  partitionLE_GT_    :: k -> m k a -> (m k a, m k a)
  partitionLT_GT_    :: k -> m k a -> (m k a, m k a)

class AssocX m k => FiniteMapX m k where
  fromSeqWith_       :: Sequence seq => (a -> a -> a) -> seq (k,a) -> m k a
  fromSeqWithKey_    :: Sequence seq => (k -> a -> a -> a) -> seq (k,a) -> m k a

  insertWith_        :: (a -> a -> a) -> k -> a -> m k a -> m k a
  insertWithKey_     :: (k -> a -> a -> a) -> k -> a -> m k a -> m k a

  insertSeqWith_     :: Sequence seq => 
                           (a -> a -> a) -> seq (k,a) -> m k a -> m k a
  insertSeqWithKey_  :: Sequence seq => 
                           (k -> a -> a -> a) -> seq (k,a) -> m k a -> m k a

  unionl_            :: m k a -> m k a -> m k a
  unionr_            :: m k a -> m k a -> m k a
  unionWith_         :: (a -> a -> a) -> m k a -> m k a -> m k a

  unionSeqWith_      :: Sequence seq => (a -> a -> a) -> seq (m k a) -> m k a

  intersectWith_     :: (a -> b -> c) -> m k a -> m k b -> m k c

  difference_        :: m k a -> m k b -> m k a

  subset_            :: m k a -> m k b -> Bool    
  subsetEq_          :: m k a -> m k b -> Bool    

class (OrdAssocX m k, FiniteMapX m k) => OrdFiniteMapX m k
  -- no methods?


class AssocX m k => Assoc m k where
  toSeq_            :: Sequence seq => m k a -> seq (k,a)
  keys_             :: Sequence seq => m k a -> seq k
  
  mapWithKey_       :: (k -> a -> b) -> m k a -> m k b
  foldWithKey_      :: (k -> a -> b -> b) -> b -> m k a -> b

  filterWithKey_    :: (k -> a -> Bool) -> m k a -> m k a
  partitionWithKey_ :: (k -> a -> Bool) -> m k a -> (m k a, m k a)

class (Assoc m k, OrdAssocX m k) => OrdAssoc m k where
  minViewWithKey_ :: m k a -> Maybe3 k a (m k a)
  minElemWithKey_ :: m k a -> (k,a)

  maxViewWithKey_ :: m k a -> Maybe3 (m k a) k a
  maxElemWithKey_ :: m k a -> (k,a)

  foldrWithKey_   :: (k -> a -> b -> b) -> b -> m k a -> b
  foldlWithKey_   :: (b -> k -> a -> b) -> b -> m k a -> b

  toOrdSeq_       :: Sequence seq => m k a -> seq (k,a)

class (Assoc m k, FiniteMapX m k) => FiniteMap m k where
  unionWithKey_     :: (k -> a -> a -> a) -> m k a -> m k a -> m k a
  unionSeqWithKey_  :: Sequence seq => (k -> a -> a -> a) -> seq (m k a) -> m k a

  intersectWithKey_ :: (k -> a -> b -> c) -> m k a -> m k b -> m k c

class (OrdAssoc m k, FiniteMap m k) => OrdFiniteMap m k
  -- no methods


-- specialize sequence operations to lists

fromList          :: AssocX m k => [(k,a)] -> m k a
insertList        :: AssocX m k => [(k,a)] -> m k a -> m k a
unionList         :: AssocX m k => [m k a] -> m k a
deleteList        :: AssocX m k => [k] -> m k a -> m k a
lookupList        :: AssocX m k => m k a -> k -> [a]
elementsList      :: AssocX m k => m k a -> [a]
unsafeFromOrdList :: OrdAssocX m k => [(k,a)] -> m k a
fromListWith      :: FiniteMapX m k => (a -> a -> a) -> [(k,a)] -> m k a
fromListWithKey   :: FiniteMapX m k => (k -> a -> a -> a) -> [(k,a)] -> m k a
insertListWith    :: FiniteMapX m k => 
                         (a -> a -> a) -> [(k,a)] -> m k a -> m k a
insertListWithKey :: FiniteMapX m k => 
                         (k -> a -> a -> a) -> [(k,a)] -> m k a -> m k a
unionListWith     :: FiniteMapX m k => (a -> a -> a) -> [m k a] -> m k a
toList            :: Assoc m k => m k a -> [(k,a)]
keysList          :: Assoc m k => m k a -> [k]
toOrdList         :: OrdAssoc m k => m k a -> [(k,a)]
unionListWithKey  :: FiniteMap m k => (k -> a -> a -> a) -> [m k a] -> m k a

fromList = fromSeq
insertList = insertSeq
unionList = unionSeq
deleteList = deleteSeq
lookupList = lookupAll
elementsList = elements
unsafeFromOrdList = unsafeFromOrdSeq
fromListWith = fromSeqWith
fromListWithKey = fromSeqWithKey
insertListWith = insertSeqWith
insertListWithKey = insertSeqWithKey
unionListWith = unionSeqWith
toList = toSeq
keysList = keys
toOrdList = toOrdSeq
unionListWithKey = unionSeqWithKey


{-
Leave out until somebody asks for:
witness????
compose????

  nub           :: m k a -> m k a  -- ???
  nubWith       :: (a -> a -> a) -> m k a -> m k a
  nubWithKey :: (k -> a -> a -> a) -> m k a -> m k a

  group         :: m k a -> m k [a] -- ???
?????  unsafeMapMonotonim k :: (a -> a) -> m k a -> m k a


-- adjustPartial??? (adjustOrDelete???)
-- adjustAll       :: (a -> a) -> k -> m k a -> m k a
-- unionMap???
-- mapPartial???

  anyViewKey :: m k a -> Maybe3 k a (m k a)
  anyKeyElem :: m k a -> (k,a) -- signals error if collection is empty
  deleteAny :: m k a -> m k a -- could go in AssocX but no point
    -- anyKeyElem and deleteAny must be consistent
    -- do they need to be consistent with anyView?

-- unionMap???
-- mapPartial???

  deleteAllList :: [k] -> m k a -> m k a

  disjoint      :: m k a -> m k b -> Bool

-}

{-
 See Seq/Sequence.hs for comments as to why these wrappers are needed.
-}
empty         :: AssocX m k => m k a
empty = empty_

single        :: AssocX m k => k -> a -> m k a
single x y = single_ x y

fromSeq       :: (AssocX m k, Sequence seq) => seq (k,a) -> m k a
fromSeq x = fromSeq_ x

insert        :: AssocX m k => k -> a -> m k a -> m k a
insert x y z = insert_ x y z

insertSeq     :: (AssocX m k, Sequence seq) => seq (k,a) -> m k a -> m k a
insertSeq x y = insertSeq_ x y

union         :: AssocX m k => m k a -> m k a -> m k a
union x y = union_ x y

unionSeq      :: (AssocX m k, Sequence seq) => seq (m k a) -> m k a
unionSeq x = unionSeq_ x 

delete        :: AssocX m k => k -> m k a -> m k a
delete x y = delete_ x y

deleteAll     :: AssocX m k => k -> m k a -> m k a
deleteAll x y = deleteAll_ x y

deleteSeq     :: (AssocX m k, Sequence seq) => seq k -> m k a -> m k a
deleteSeq x y = deleteSeq_ x y

null          :: AssocX m k => m k a -> Bool
null x = null_ x

size          :: AssocX m k => m k a -> Int
size x = size_ x

member        :: AssocX m k => m k a -> k -> Bool
member x y = member_ x y

count         :: AssocX m k => m k a -> k -> Int
count x y = count_ x y

lookup        :: AssocX m k => m k a -> k -> a
lookup x y = lookup_ x y

lookupM       :: AssocX m k => m k a -> k -> Maybe a
lookupM x y = lookupM_ x y

lookupAll     :: (AssocX m k, Sequence seq) => m k a -> k -> seq a
lookupAll x y = lookupAll_ x y

lookupWithDefault :: AssocX m k => a -> m k a -> k -> a
lookupWithDefault x y z = lookupWithDefault_ x y z

adjust        :: AssocX m k => (a -> a) -> k -> m k a -> m k a
adjust x y z = adjust_ x y z

adjustAll     :: AssocX m k => (a -> a) -> k -> m k a -> m k a
adjustAll x y z = adjustAll_ x y z

map           :: AssocX m k => (a -> b) -> m k a -> m k b
map x y = map_ x y

fold          :: AssocX m k => (a -> b -> b) -> b -> m k a -> b
fold x y z = fold_ x y z

fold1         :: AssocX m k => (a -> a -> a) -> m k a -> a
fold1 x y = fold1_ x y

filter        :: AssocX m k => (a -> Bool) -> m k a -> m k a
filter x y = filter_ x y

partition     :: AssocX m k => (a -> Bool) -> m k a -> (m k a, m k a)
partition x y = partition_ x y

elements      :: (AssocX m k, Sequence seq) => m k a -> seq a
elements x = elements_ x

instanceName  :: AssocX m k => m k a -> String
instanceName x = instanceName_ x


-- OrdAssocX

minView           :: OrdAssocX m k => m k a -> Maybe2 a (m k a)
minView x = minView_ x

minElem           :: OrdAssocX m k => m k a -> a
minElem x = minElem_ x

deleteMin         :: OrdAssocX m k => m k a -> m k a
deleteMin x = deleteMin_ x


unsafeInsertMin   :: OrdAssocX m k => k -> a -> m k a -> m k a
unsafeInsertMin x y z = unsafeInsertMin_ x y z


maxView           :: OrdAssocX m k => m k a -> Maybe2 (m k a) a
maxView x = maxView_ x

maxElem           :: OrdAssocX m k => m k a -> a
maxElem x = maxElem_ x

deleteMax         :: OrdAssocX m k => m k a -> m k a
deleteMax x = deleteMax_ x 

unsafeInsertMax   :: OrdAssocX m k => m k a -> k -> a -> m k a
unsafeInsertMax x y z = unsafeInsertMax_ x y z


foldr             :: OrdAssocX m k => (a -> b -> b) -> b -> m k a -> b
foldr x y z = foldr_ x y z

foldl             :: OrdAssocX m k => (b -> a -> b) -> b -> m k a -> b
foldl x y z = foldl_ x y z

foldr1            :: OrdAssocX m k => (a -> a -> a) -> m k a -> m k a
foldr1 x y = foldr1_ x y

foldl1            :: OrdAssocX m k => (a -> a -> a) -> m k a -> m k a
foldl1 x y = foldl1_ x y

unsafeFromOrdSeq  :: (OrdAssocX m k, Sequence seq) => seq (k,a) -> m k a
unsafeFromOrdSeq x = unsafeFromOrdSeq_ x

unsafeAppend      :: OrdAssocX m k => m k a -> m k a -> m k a
unsafeAppend x y = unsafeAppend_ x y 

filterLT          :: OrdAssocX m k => k -> m k a -> m k a
filterLT x y = filterLT_ x y

filterLE          :: OrdAssocX m k => k -> m k a -> m k a
filterLE x y = filterLE_ x y

filterGT          :: OrdAssocX m k => k -> m k a -> m k a
filterGT x y = filterGT_ x y 

filterGE          :: OrdAssocX m k => k -> m k a -> m k a
filterGE x y = filterGE_ x y

partitionLT_GE    :: OrdAssocX m k => k -> m k a -> (m k a, m k a)
partitionLT_GE x y = partitionLT_GE_ x y

partitionLE_GT    :: OrdAssocX m k => k -> m k a -> (m k a, m k a)
partitionLE_GT x y = partitionLE_GT_ x y

partitionLT_GT    :: OrdAssocX m k => k -> m k a -> (m k a, m k a)
partitionLT_GT x y = partitionLT_GT_ x y

-- FiniteMapX m k 

fromSeqWith       :: (FiniteMapX m k, Sequence seq) => (a -> a -> a) -> seq (k,a) -> m k a
fromSeqWith x y = fromSeqWith_ x y

fromSeqWithKey    :: (FiniteMapX m k, Sequence seq) => (k -> a -> a -> a) -> seq (k,a) -> m k a
fromSeqWithKey x y = fromSeqWithKey_ x y

insertWith        :: FiniteMapX m k => (a -> a -> a) -> k -> a -> m k a -> m k a
insertWith x y z w = insertWith_ x y z w

insertWithKey     :: FiniteMapX m k => (k -> a -> a -> a) -> k -> a -> m k a -> m k a
insertWithKey x y z w = insertWithKey_ x y z w

insertSeqWith     :: (FiniteMapX m k, Sequence seq) => 
                           (a -> a -> a) -> seq (k,a) -> m k a -> m k a
insertSeqWith x y z = insertSeqWith_ x y z

insertSeqWithKey  :: (FiniteMapX m k, Sequence seq) => 
                           (k -> a -> a -> a) -> seq (k,a) -> m k a -> m k a
insertSeqWithKey x y z = insertSeqWithKey_ x y z

unionl            :: FiniteMapX m k => m k a -> m k a -> m k a
unionl x y = unionl_ x y

unionr            :: FiniteMapX m k => m k a -> m k a -> m k a
unionr x y = unionr_ x y

unionWith         :: FiniteMapX m k => (a -> a -> a) -> m k a -> m k a -> m k a
unionWith x y z = unionWith_ x y z

unionSeqWith      :: (FiniteMapX m k, Sequence seq) => (a -> a -> a) -> seq (m k a) -> m k a
unionSeqWith x y = unionSeqWith_ x y

intersectWith     :: FiniteMapX m k => (a -> b -> c) -> m k a -> m k b -> m k c
intersectWith x y z = intersectWith_ x y z


difference        :: FiniteMapX m k => m k a -> m k b -> m k a
difference x y = difference_ x y

subset            :: FiniteMapX m k => m k a -> m k b -> Bool    
subset x y = subset_ x y

subsetEq          :: FiniteMapX m k => m k a -> m k b -> Bool    
subsetEq x y = subsetEq_ x y


-- Assoc m k 
toSeq            :: (Assoc m k, Sequence seq) => m k a -> seq (k,a)
toSeq x = toSeq_ x

keys             :: (Assoc m k, Sequence seq) => m k a -> seq k
keys x = keys_ x  

mapWithKey       :: Assoc m k => (k -> a -> b) -> m k a -> m k b
mapWithKey x y = mapWithKey_ x y

foldWithKey      :: Assoc m k => (k -> a -> b -> b) -> b -> m k a -> b
foldWithKey x y z = foldWithKey_ x y z

filterWithKey    :: Assoc m k => (k -> a -> Bool) -> m k a -> m k a
filterWithKey x y = filterWithKey_ x y

partitionWithKey :: Assoc m k => (k -> a -> Bool) -> m k a -> (m k a, m k a)
partitionWithKey x y = partitionWithKey_ x y

-- OrdAssoc m k

minViewWithKey :: OrdAssoc m k => m k a -> Maybe3 k a (m k a)
minViewWithKey x = minViewWithKey_ x

minElemWithKey :: OrdAssoc m k => m k a -> (k,a)
minElemWithKey x = minElemWithKey_ x

maxViewWithKey :: OrdAssoc m k => m k a -> Maybe3 (m k a) k a
maxViewWithKey x = maxViewWithKey_ x

maxElemWithKey :: OrdAssoc m k => m k a -> (k,a)
maxElemWithKey x = maxElemWithKey_ x

foldrWithKey   :: OrdAssoc m k => (k -> a -> b -> b) -> b -> m k a -> b
foldrWithKey x y z = foldrWithKey_ x y z

foldlWithKey   :: OrdAssoc m k => (b -> k -> a -> b) -> b -> m k a -> b
foldlWithKey x y z = foldlWithKey_ x y z


toOrdSeq       :: (OrdAssoc m k, Sequence seq) => m k a -> seq (k,a)
toOrdSeq x = toOrdSeq_ x

-- FiniteMap m k
unionWithKey     :: FiniteMap m k => (k -> a -> a -> a) -> m k a -> m k a -> m k a
unionWithKey x y z = unionWithKey_ x y z

unionSeqWithKey  :: (FiniteMap m k, Sequence seq) => (k -> a -> a -> a) -> seq (m k a) -> m k a
unionSeqWithKey x y = unionSeqWithKey_ x y

intersectWithKey :: FiniteMap m k => (k -> a -> b -> c) -> m k a -> m k b -> m k c
intersectWithKey x y z = intersectWithKey_ x y z

