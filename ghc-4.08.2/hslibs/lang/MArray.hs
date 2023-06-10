-----------------------------------------------------------------------------
-- $Id: MArray.hs,v 1.1.2.1 2000/05/26 11:13:11 simonmar Exp $
--
-- (c) The GHC Team 2000
--

module MArray ( 

	-- Class of mutable arrays in a given monad
	MArray {- a:*->*->*  e:*  m:*->* -} ( 
	   get,    -- :: Ix ix => a ix e -> ix -> ST s e
           put,    -- :: Ix ix => a ix e -> ix -> e -> ST s ()
           marray  -- :: Ix ix => (ix,ix) -> ST s (a ix e)
          ),

	STArray,   -- instance HasBounds, MArray
	STUArray,  -- instance HasBounds, MArray (Char, Int, Word etc.)

	IOArray,   -- instance HasBounds, MArray
	IOUArray,  -- instance HasBounds, MArray (Char, Int, Word etc.)

	assocs,    -- :: (Ix ix, MArray a e m) => a s ix e -> m s [(ix,e)]
	indices,   -- :: (Ix ix, IArray a e) => a ix e -> [ix]

	(//),      -- :: (Ix ix, MArray a e m) => a ix e -> [(ix,e)] -> m ()

 	amap,      -- :: (Ix ix, MArray a x m, MArray a y m) => 
		   --	   (x->y) -> a ix x -> m (a ix y)

	listArray, -- :: (Ix ix, MArray a e m) => (ix,ix) -> [e] -> m (a ix e)

	elems,     -- :: (Ix ix, MArray a e m) => a ix e -> m [e]

	ixmap,     -- :: (Ix ix, Ix iy, MArray a e m) => 
		   --	   (ix,ix) -> (ix->iy) -> a iy e -> m (a ix e)

	freeze,    -- :: (Ix ix, MArray a e m, IArray b e) => 
		   --		a ix e -> m (b ix e)
	unsafeFreeze, -- :: (Ix ix, MArray a e m, IArray b e) => 
		      --		a ix e -> m (b ix e)

        thaw,       --  :: (Ix ix, IArray a e, MArray b e m) => 
		    --		a ix e -> m (b ix e)
	unsafeThaw, --  :: (Ix ix, IArray a e, MArray b e m) => 
		    --		a ix e -> m (b ix e)

	-- legacy non-overloaded byte array interface
	newCharArray,
	newIntArray,
	newWordArray,
	newAddrArray,
	newFloatArray,
	newDoubleArray,
	newStablePtrArray,

	readCharArray,
	readIntArray,
	readWordArray,
	readAddrArray,
	readFloatArray,
	readDoubleArray,
	readStablePtrArray,

	writeCharArray,
	writeIntArray,
	writeWordArray,
	writeAddrArray,
	writeFloatArray,
	writeDoubleArray,
	writeStablePtrArray,
  ) where

import ArrayBase

import IOExts
import Word
import Addr
import IArray (indices)
import qualified IArray
import Ix
import ST
import PrelST
import PrelByteArr (ByteArray(..))
import PrelArrExtra (freezeByteArray)
import PrelStable
import PrelAddr
import PrelFloat
import PrelBase
import PrelGHC

import Prelude hiding (read)

-----------------------------------------------------------------------------
-- Polymorphic non-strict mutable arrays (IO monad)

instance HasBounds IOArray where
   bounds     = boundsIOArray

instance MArray IOArray e IO where
   get        = readIOArray
   put        = writeIOArray
   marray ixs = newIOArray ixs io_bot

io_bot = error "IOArray: undefined element"
	--ToDo: throw (ArrayException UndefinedElement)

-----------------------------------------------------------------------------
-- Flat unboxed mutable arrays (IO monad)

newtype IOUArray ix e = IOUArray (STUArray RealWorld ix e)
-- could be a type synonym, but IOUArray will probably make
-- more sense in error messages.

boundsOfIOUArray (IOUArray a) = boundsSTUArray a

instance HasBounds IOUArray where
   bounds = boundsOfIOUArray

instance MArray IOUArray Char IO where
   get  (IOUArray a) i  = stToIO (readCharArray a i)
   put (IOUArray a) i e = stToIO (writeCharArray a i e)
   marray ixs = do { a <- stToIO (newCharArray ixs); return (IOUArray a) }

instance MArray IOUArray Int IO where
   get  (IOUArray a) i  = stToIO (readIntArray a i)
   put (IOUArray a) i e = stToIO (writeIntArray a i e)
   marray ixs = do { a <- stToIO (newIntArray ixs); return (IOUArray a) }

instance MArray IOUArray Word IO where
   get  (IOUArray a) i  = stToIO (readWordArray a i)
   put (IOUArray a) i e = stToIO (writeWordArray a i e)
   marray ixs = do { a <- stToIO (newWordArray ixs); return (IOUArray a) }

instance MArray IOUArray Addr IO where
   get  (IOUArray a) i  = stToIO (readAddrArray a i)
   put (IOUArray a) i e = stToIO (writeAddrArray a i e)
   marray ixs = do { a <- stToIO (newAddrArray ixs); return (IOUArray a) }

instance MArray IOUArray Float IO where
   get  (IOUArray a) i  = stToIO (readFloatArray a i)
   put (IOUArray a) i e = stToIO (writeFloatArray a i e)
   marray ixs = do { a <- stToIO (newFloatArray ixs); return (IOUArray a) }

instance MArray IOUArray Double IO where
   get  (IOUArray a) i  = stToIO (readDoubleArray a i)
   put (IOUArray a) i e = stToIO (writeDoubleArray a i e)
   marray ixs = do { a <- stToIO (newDoubleArray ixs); return (IOUArray a) }

-----------------------------------------------------------------------------
-- standard array ops

{-# INLINE assocs #-}
assocs :: (Ix ix, MArray a e m) => a ix e -> m [(ix,e)]
assocs a =  sequence [ do { e <- get a i; return (i,e) } | i <- indices a ]

(//) :: (Ix ix, MArray a e m) => a ix e -> [(ix,e)] -> m ()
a // upds = sequence_ [ put a i e | (i,e) <- upds ]

amap :: (Ix ix, MArray a x m, MArray a y m) => (x->y) -> a ix x -> m (a ix y)
amap f a = do
  new <- marray (bounds a)
  sequence_ [ do { e <- get a i; put new i (f e) } | i <- indices a ]
  return new
  
listArray :: (Ix ix, MArray a e m) => (ix,ix) -> [e] -> m (a ix e)
listArray b elems = do
  a <- marray b
  sequence_ [ put a i e | (i,e) <- zip (range b) elems ]
  return a

elems  :: (Ix ix, MArray a e m) => a ix e -> m [e]
elems a = sequence (map (get a) (indices a))

ixmap :: (Ix ix, Ix iy, MArray a e m) => 
		(ix,ix) -> (ix->iy) -> a iy e -> m (a ix e)
ixmap b f a = do
  new <- marray b
  sequence_ [ do { e <- get a (f i); put new i e } | i <- range b ]
  return new

-----------------------------------------------------------------------------
-- Freezing

freeze  :: (Ix ix, MArray a e m, IArray b e) => a ix e -> m (b ix e)
freeze a = do { elts <- assocs a; return (IArray.array (bounds a) elts) }

freezeSTUArray   :: Ix ix => STUArray s ix e -> ST s (UArray ix e)
-- This coercion of memcpy to the ST monad is safe, because memcpy
-- only modifies its destination operand, which is already MutableByteArray.
freezeSTUArray (STUArray l u arr) = ST $ \ s ->
	let n = sizeofMutableByteArray# arr in
	case (newCharArray# n s)                   of { (# s, newarr #) -> 
	case ((unsafeCoerce# memcpy) newarr arr n s) of { (# s, () #) ->
	case unsafeFreezeByteArray# newarr s       of { (# s, frozen #) ->
	(# s, UArray l u frozen #) }}}

foreign import "memcpy" unsafe 
  memcpy :: MutableByteArray# RealWorld -> ByteArray# -> Int# -> IO ()

{-# RULES
"freeze/STArray"    freeze = freezeSTArray
 #-}

{-# RULES
"freeze/Char"    freeze = freezeSTUArray
   :: Ix ix => STUArray s ix Char -> ST s (UArray ix Char)
"freeze/Int" freeze = freezeSTUArray
   :: Ix ix => STUArray s ix Int -> ST s (UArray ix Int)
"freeze/Float"  freeze = freezeSTUArray 
   :: Ix ix => STUArray s ix Float -> ST s (UArray ix Float)
"freeze/Double" freeze = freezeSTUArray
   :: Ix ix => STUArray s ix Double -> ST s (UArray ix Double)
 #-}

unsafeFreeze :: (Ix ix, MArray a e m, IArray b e) => a ix e -> m (b ix e)
unsafeFreeze = freeze

{-# RULES
"unsafeFreeze/STUArray"   unsafeFreeze = unsafeFreezeSTUArray
 #-}

-----------------------------------------------------------------------------
-- Thawing

thaw :: (Ix ix, IArray a e, MArray b e m) => a ix e -> m (b ix e)
thaw a = do
  new_a <- marray (IArray.bounds a)
  sequence_ [ put new_a i e | (i,e) <- IArray.assocs a ]
  return new_a

thawUArray :: Ix ix => UArray ix e -> ST s (STUArray s ix e)
thawUArray (UArray l u arr) = ST $ \ s ->
	let n = sizeofByteArray# arr in
	case (newCharArray# n s)                   of { (# s, newarr #) -> 
	case ((unsafeCoerce# memcpy) newarr arr n s) of { (# s, () #) ->
	(# s, STUArray l u newarr #) }}

{-# RULES
"thaw/STArray"    thaw = thawSTArray
 #-}

{-# RULES
"thaw/Char"    thaw = thawUArray
   :: Ix ix => UArray ix Char -> ST s (STUArray s ix Char)
"thaw/Int" thaw = thawUArray
   :: Ix ix => UArray ix Int -> ST s (STUArray s ix Int)
"thaw/Float"  thaw = thawUArray 
   :: Ix ix => UArray ix Float -> ST s (STUArray s ix Float)
"thaw/Double" thaw = thawUArray
   :: Ix ix => UArray ix Double -> ST s (STUArray s ix Double)
 #-}

{-
  in-place conversion of immutable arrays to mutable ones places
  a proof obligation on the user: no other parts of your code can
  have a reference to the array at the point where you unsafely
  thaw it (and, subsequently mutate it, I suspect.)
-}
unsafeThaw :: (Ix ix, IArray a e, MArray b e m) => a ix e -> m (b ix e)
unsafeThaw = thaw

unsafeThawUArray :: Ix ix => UArray ix e -> ST s (STUArray s ix e)
unsafeThawUArray (UArray l u barr)
  = return (STUArray l u (unsafeCoerce# barr))

{-# RULES
"unsafeThaw/UArray"   unsafeThaw = unsafeThawUArray
 #-}

