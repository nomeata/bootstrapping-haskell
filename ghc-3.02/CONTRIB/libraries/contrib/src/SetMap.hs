-- SetMap.hs - sets and maps in Haskell

-- @(#)SetMap.hs	1.8 dated 92/02/24 at 17:36:58

-- Author:  Nick North
-- Address: DITC/93
--          National Physical Laboratory
--          Teddington
--          Middlesex, TW11 0LW
--          England
-- Email:   ndn@seg.npl.co.uk

-- This module implements abstract types for sets and maps in Haskell,
--   allowing reading and writing of sets in the form {a, b, c} and
--   of maps in the form {a -> 1, b -> 2} and performance of a whole
--   multitude of useful operations.
-- The names of the operations available are given in the interface.
-- I hope they are pretty self-explanatory. If not, the extensive
--   documentation will clarify things :-)
-- In case you are wondering, the operation names are taken from the
--   long-gestating standard for the formal description language VDM.
-- The Set section is standalone and could be extracted to form a
--   separate module, but the Map stuff relies on knowledge of the
--   innards of sets and therefore cannot be a separate module.

-- Strictly speaking, this is
-- Crown Copyright 1992
-- but I don't suppose Her Majesty will mind it being shared around.

module SetMap (
    Set,
        card, choose, diff, dinter, dunion, emptyset, element, exists,
        forall, insert, inter, list_set, powerset, set_list, subset,
	superset, union,
    Map,
	compose, dom, domrestrby, domrestrto, emptymap, list_map,
	map_list, mapapply, mapdmerge, mapinv, mapiter, mapunion,
	override, rng, rngrestrby, rngrestrto
    ) where

-- Sets are represented as ordered lists without duplicates.
-- The MkSet constructor allows us to invent our own syntax
data (Ord a) => Set a = MkSet [a]  deriving (Eq, Ord)

-- Allow sets to be represented as {a, b, c}
-- If you don't like the syntax, you can change the brackets and the
-- item separator by changing the following. Note that sets and maps
-- use the same brackets and item separator.
smb    =  "{"
sme    =  "}"
smsep  =  ","
instance (Ord a, Show a) => Show (Set a) where
    showsPrec d (MkSet [])      =  showString smb . showString sme
    showsPrec d (MkSet (x:xs))  =
        showString smb . shows x . foldr g (showString sme) xs
	where
	g x' s  =  showString (smsep ++ " ") . shows x' . s

instance (Ord a, Read a) => Read (Set a) where
    readsPrec p  =
	readParen False
	    (\r -> [(list_set xs, t) | (tok,s) <- lex r, tok == smb,
				       (xs,t)  <- readset s])
	where
	readset s  =  [([],t)   | (tok,t) <- lex s, tok == sme] ++
		      [(x:xs,u) | (x,t)   <- reads s, (xs,u) <- readrest t]
	readrest s  =  [([],t)   | (tok,t) <- lex s, tok == sme] ++
		       [(x:xs,v) | (tok,t) <- lex s, tok == smsep,
				   (x,u)   <- reads t,
				   (xs,v)  <- readrest u]

-- Set functions

-- card xs
-- is the cardinality of the set xs
--partain:removed sig:card :: (Ord a, Integral n) => Set a -> n
card (MkSet xs)  =  length xs

-- choose xs
-- is a an element, chosen from the set xs
choose :: (Ord a) => Set a -> a
choose (MkSet (x:xs))  =  x
choose (MkSet [])      =  error "Attempt to choose from emptyset"

-- diff xs ys
-- is the set of elements of the set xs not in the set ys
diff :: (Ord a) => Set a -> Set a -> Set a
diff (MkSet xs) (MkSet ys)  =  MkSet (diff_list xs ys)

diff_list [] _                    =  []
diff_list xs@(_:_) []             =  xs
diff_list (x:xs) (y:ys) | x < y   =  x : diff_list xs (y:ys)
		        | x == y  =  diff_list xs ys
		        | x > y   =  diff_list (x:xs) ys

-- dinter xss
-- is the distributed intersection of the set of sets xss
dinter :: (Ord a) =>  Set (Set a) -> Set a
dinter (MkSet [])        =  emptyset ()
dinter (MkSet (xs:xss))  =  foldr inter xs xss

-- dunion xss
-- is the distributed union of the set of sets xss
dunion :: (Ord a) => Set (Set a) -> Set a
dunion (MkSet xss)  =  foldr union (emptyset ()) xss

-- element x xs
-- is True iff x is an element of the set xs
element :: (Ord a) => a -> Set a -> Bool
element x xs  =  inter (MkSet [x]) xs /= emptyset ()

-- emptyset ()
-- is the empty set
-- This has to be a function because overloaded constants cannot be
-- exported (Haskell report section 4.4.2, page 34).
emptyset :: (Ord a) => () -> Set a
emptyset ()  =  MkSet []

-- exists xs pred
-- is True iff there is some element, x, of the set xs such that (pred x)
-- is True
exists :: (Ord a) => Set a -> (a -> Bool) -> Bool
exists (MkSet xs) pred  =  foldr (||) False (map pred xs)

-- forall xs pred
-- is True iff, for all elements, x, of xs, (pred x) is True
forall :: (Ord a) => Set a -> (a -> Bool) -> Bool
forall (MkSet xs) pred  =  foldr (&&) True (map pred xs)

-- insert x xs
-- adds the element x to the set xs
insert :: (Ord a) => a -> Set a -> Set a
insert x (MkSet xs)  =  MkSet (sort (x:xs))

-- inter xs ys
-- is the intersection of the sets xs and ys
inter :: (Ord a) => Set a -> Set a -> Set a
inter (MkSet xs) (MkSet ys)  =  MkSet (inter_list xs ys)

inter_list (x:xs) (y:ys) | x < y   =  inter_list xs (y:ys)
                         | x == y  =  x : inter_list xs ys
			 | x > y   =  inter_list (x:xs) ys
inter_list _ _                     =  []

-- list_set xs
-- is the set whose elements are the members of the list xs
list_set :: (Ord a) => [a] -> Set a
list_set xs  =  MkSet (sort xs)

-- definition of sort taken from the Miranda prelude
sort :: (Ord a) => [a] -> [a]
sort xs =  if n <= 1
           then xs
           else merge (sort (take n2 xs)) (sort (drop n2 xs))
	   where
	   n   =  length xs
	   n2  =  n `div` 2

-- merge will remove duplicate pairs; duplicate triples never arise
merge [] ys                   =  ys
merge xs@(_:_) []             =  xs
merge (x:xs) (y:ys) | x < y   =  x : merge xs (y:ys)
                    | x == y  =  x : merge xs ys
		    | x > y   =  y : merge (x:xs) ys

-- powerset xs
-- is the powerset of the set xs
powerset :: (Ord a) => Set a -> Set (Set a)
powerset (MkSet xs)  =  MkSet (emptyset () : [MkSet ys | ys <- power_list xs])

power_list []      =  []
power_list (x:xs)  =  [x] : ([x:ys | ys <- ylist] ++ ylist)
		      where
		      ylist  =  power_list xs

-- set_list xs
-- is a list whose members are the elements of the set xs
set_list :: (Ord a) => Set a -> [a]
set_list (MkSet xs)  =  xs

-- subset xs ys
-- is True iff the set xs is a subset of the set ys
subset :: (Ord a) => Set a -> Set a -> Bool
subset (MkSet xs) (MkSet ys)  =  inter_list xs ys == xs

-- superset xs ys
-- is True iff the set xs is a superset of the set ys
superset :: (Ord a) => Set a -> Set a -> Bool
superset xs ys  =  subset ys xs

-- union xs ys
-- is the union of the sets xs and ys
union :: (Ord a) => Set a -> Set a -> Set a
union (MkSet xs) (MkSet ys)  =  MkSet (merge xs ys)


-- Maps are represented as ordered lists without duplicates.
-- The MkMap constructor allows us to invent our own syntax

data (Ord a, Ord b) => Map a b = MkMap [(a, b)] deriving (Eq, Ord)

-- Allow maps to be represented as {x -> y, u -> v}
-- If you don't like the syntax, see sme, smb and smsep to change the
-- brackets and the item separator.
-- The maplet separator can be changed by changing mapletsep below
mapletsep  =  "->"
instance (Ord a, Show a, Ord b, Show b) => Show (Map a b) where
    showsPrec d (MkMap [])      =  showString smb . showString sme
    showsPrec d (MkMap (x:xm))  =
	showString smb . showmaplet x . foldr g (showString sme) xm
	where
	g x' s  =  showString (smsep ++ " ") . showmaplet x' . s
	showmaplet (x,y)  =  shows x . showString mapletsep . shows y

instance (Ord a, Read a, Ord b, Read b) => Read (Map a b) where
    readsPrec p  =
	readParen False
	    (\r -> [(list_map xm, t) | (tok,s) <- lex r, tok == smb,
				       (xm,t)  <- readmap s])
	where
	readmap s  =  [([],t)   | (tok,t) <- lex s, tok == sme] ++
		      [(x:xm,u) | (x,t)   <- readmaplet s,
				  (xm,u)  <- readrest t]
	readrest s  =  [([],t)   | (tok,t) <- lex s, tok == sme] ++
		       [(x:xm,v) | (tok,t) <- lex s, tok == smsep,
				   (x,u)   <- readmaplet t,
				   (xm,v)  <- readrest u]
	readmaplet  =
	    readParen False
		(\r -> [((x,y),u) | (x,s)   <- reads r,
				    (tok,t) <- lex s, tok == mapletsep,
				    (y,u)   <- reads t])

-- Map functions

-- compose xm ym
-- is the map given by (compose xm ym) y = mapapply xm (mapapply ym y)
-- This could be more efficient
compose :: (Ord a, Ord b, Ord c) => Map b c -> Map a b -> Map a c
compose (MkMap xm) (MkMap ym)  = 
    MkMap [(x,z') | (x,y) <- ym, (y',z') <- xm, y == y']

-- dom xm
-- is the domain of the map xm
-- Because map elements are ordered, their domain elements are ordered, so the
---  set does not need to be sorted.
dom :: (Ord a, Ord b) => Map a b -> Set a
dom (MkMap xm)  =  MkSet [x | (x,y) <- xm]

-- domrestrby xs xm
-- is the map xm with the set xs removed from its domain
domrestrby :: (Ord a, Ord b) => Set a -> Map a b -> Map a b
domrestrby (MkSet xs) (MkMap xm)  =  MkMap (domrestrby_list xs xm)

domrestrby_list [] xm                       =  xm
domrestrby_list _ []                        =  []
domrestrby_list (x:xs) ((y,z):ys) | x < y   =  domrestrby_list xs ((y,z):ys)
		                  | x == y  =  domrestrby_list xs ys
		                  | x > y   =  (y,z) : domrestrby_list (x:xs) ys

-- domrestrby xs xm
-- is the map xm with its domain retricted to the set xs
domrestrto :: (Ord a, Ord b) => Set a -> Map a b -> Map a b
domrestrto (MkSet xs) (MkMap xm)  =  MkMap (domrestrto_list xs xm)

domrestrto_list [] _                        =  []
domrestrto_list _ []                        =  []
domrestrto_list (x:xs) ((y,z):ys) | x < y   =  domrestrto_list xs ((y,z):ys)
		                  | x == y  =  (y,z) : domrestrto_list xs ys
		                  | x > y   =  domrestrto_list (x:xs) ys

-- emptymap ()
-- is the empty map
-- This has to be a function because overloaded constants cannot be
-- exported (Haskell report section 4.4.2, page 34).
emptymap :: (Ord a, Ord b) => () -> Map a b
emptymap ()  =  MkMap []

-- list_map xs
-- is the map whose maplets are the elements of the list of pairs xs
list_map :: (Ord a, Ord b) => [(a,b)] -> Map a b
list_map xs
    =  MkMap (foldr process [] (sort xs))
       where
       process (x,y) ((x',y'):xm) | (x,y) == (x',y')  =  (x',y') : xm
				  |     x == x'       =  error "Illegal map"
				  |      True         =  (x,y) : (x',y') : xm
       process (x,y) []                               =  [(x,y)]

-- map_list xm
-- is a list of pairs whose elements are the maplets of xm
map_list :: (Ord a, Ord b) => Map a b -> [(a,b)]
map_list (MkMap xm)  =  xm

-- mapapply xm x
-- is the result of applying the map xm to the element x
mapapply :: (Ord a, Ord b) => Map a b -> a -> b
mapapply (MkMap xm) x
    =  foldr (checkelt x) outside xm
       where
       checkelt x (x',y') z | x < x'   =  outside
			    | x == x'  =  y'
			    | x > x'   =  z
       outside  =  error "Map applied outside its domain"

-- mapdmerge xms
-- is the union of the set of maps xms
mapdmerge :: (Ord a, Ord b) => Set (Map a b) -> Map a b
mapdmerge xms  =  foldr mapunion (emptymap ()) (set_list xms)

-- mapinv xm
-- is the inverse of the map xm
mapinv :: (Ord a, Ord b) => Map a b -> Map b a
mapinv (MkMap xm)  =  list_map [(y,x) | (x,y) <- xm]

-- mapiter xm n
-- is the map xm iterated n times
-- Note that mapiter xm 0 is illegal, as the identity map cannot be
-- represented in this scheme
-- The type is that derived by the compiler when there is no type declaration.
mapiter :: (Ord a, Num n, Enum n) => Map a a -> n -> Map a a
mapiter xm n  =  foldr1 compose [xm | _ <- [1..n]]

-- mapunion xm ym
-- is the union of the maps xm and ym
mapunion :: (Ord a, Ord b) => Map a b -> Map a b -> Map a b
mapunion m@(MkMap xm) m'@(MkMap ym)
    | inter (dom m) (dom m') == emptyset ()  =  MkMap (merge xm ym)
    | True                                 =  error "Merge of overlapping maps"

-- override xm ym
-- is the map xm over-ridden by the map ym
override :: (Ord a, Ord b) => Map a b -> Map a b -> Map a b
override (MkMap xm) (MkMap ym)  =  MkMap (override_list xm ym)

override_list xm []  =  xm
override_list [] ym  =  ym
override_list m@((x,y):xm) m'@((x',y'):xm')
    | x < x'   =  (x,y)   : override_list xm m'
    | x == x'  =  (x',y') : override_list xm xm'
    | x > x'   =  (x',y') : override_list m xm'

-- rng xm
-- is the range of the map xm
rng :: (Ord a, Ord b) => Map a b -> Set b
rng (MkMap xm)  =  list_set [y | (x,y) <- xm]

-- rngrestrby xm xs
-- is the map xm with the set xs removed from its range
rngrestrby :: (Ord a, Ord b) => Map a b -> Set b -> Map a b
rngrestrby (MkMap xm) xs
    =  MkMap [(x,y) | (x,y) <- xm , not (element y xs)] 

-- rngrestrto xm xs
-- is the map xm with its range restricted to the set xs
-- range restriction to a set
rngrestrto :: (Ord a, Ord b) => Map a b -> Set b -> Map a b
rngrestrto (MkMap xm) xs
    =  MkMap [(x,y) | (x,y) <- xm , element y xs]
