-----------------------------------------------------------------------------
--  $Id: IArray.hs,v 1.1 2000/04/28 15:29:18 simonmar Exp $
--
-- (c) The GHC Team 2000
--

module IArray ( 

	-- class of immutable array types
	IArray {- a:*->*->*  e:* -} (
	   (!),    -- :: Ix ix => a ix e -> ix -> e
	   array   -- :: Ix ix => (ix,ix) -> [(ix,e)] -> a ix e	
	),

	HasBounds {- a:*->*->* -} (
	   bounds  -- :: Ix ix => a ix e -> (ix,ix)
	),

	-- non-strict polymorphic arrays
	Array.Array,

	-- strict, unboxed arrays
	UArray(..), -- non abstract because MArray needs to get at it

	assocs,    -- :: (Ix ix, IArray a e) => a ix e -> [(ix,e)]
	indices,   -- :: (Ix ix, IArray a e) => a ix e -> [ix]
	amap,      -- :: (Ix ix, IArray a x, IArray a y) => 
		   --      (x -> y) -> a ix x -> a ix y
        (//),      -- :: (Ix ix, IArray a e) => a ix e -> [(ix,e)] -> a ix e
        listArray, -- :: (Ix ix, IArray a e) => (ix,ix) -> [e] -> a ix e
        elems,     -- :: (Ix ix, IArray a e) => a ix e -> [e]
	accum,     -- :: (Ix ix, IArray a e) => 
		   --	   (e->f->e) -> a ix e -> [(ix,f)] -> a ix e
        accumArray,-- :: (Ix ix, IArray a e) => 
		   --	   (e->f->e) -> e -> (ix,ix) -> [(ix,f)] -> a ix e
        ixmap      -- :: (Ix ix, Ix iy) => 
		   --	   (ix,ix) -> (ix->iy) -> a iy e -> a ix e
  ) where

import ArrayBase

-- exts
import ByteArray
import Addr
import Word

-- std
import List
import Ix
import qualified Array

-- prel
import PrelST
import PrelFloat
import PrelBase

-----------------------------------------------------------------------------
-- standard array ops

{-# INLINE assocs #-}
assocs     :: (Ix ix, IArray a e) => a ix e -> [(ix,e)]
assocs a   =  [(i, a!i) | i <- indices a]

{-# INLINE indices #-}
indices    :: (Ix ix, HasBounds a) => a ix e -> [ix]
indices	   =  range . bounds

amap	   :: (Ix ix, IArray a x, IArray a y) => (x -> y) -> a ix x -> a ix y
amap f a   =  array b [(i, f (a!i)) | i <- range b]
           where b = bounds a

accum      :: (Ix ix, IArray a e) => (e->f->e) -> a ix e -> [(ix,f)] -> a ix e
accum f    =  foldl (\a (i,v) -> a // [(i,f (a!i) v)])

accumArray :: (Ix ix, IArray a e)
	   => (e->f->e) -> e -> (ix,ix) -> [(ix,f)] -> a ix e
accumArray f z b  =  accum f (array b [(i,z) | i <- range b])

-- ToDo: more efficiently, specialised versions?
(//)     :: (Ix ix, IArray a e)  => a ix e -> [(ix,e)] -> a ix e
a // us  =  array (bounds a)
            ([(i,a!i) | i <- indices a \\ [i | (i,_) <- us]]
                  ++ us)

{-# SPECIALISE listArray :: (Int,Int) -> [b] -> Array.Array Int b #-}
listArray      :: (Ix ix, IArray a e) => (ix,ix) -> [e] -> a ix e
listArray b vs =  array b (zip (range b) vs)

{-# INLINE elems #-}
elems	   :: (Ix ix, IArray a e) => a ix e -> [e]
elems a    =  [a!i | i <- indices a]

ixmap	   :: (Ix ix, Ix iy, IArray a e)
	   => (ix,ix) -> (ix->iy) -> a iy e -> a ix e
ixmap b f a =  array b [(i, a ! f i) | i <- range b]
