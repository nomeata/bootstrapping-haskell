%
% (c) AQUA Project, Glasgow University, 1998
%

Cheap and cheerful dynamic types.

The Dynamic interface is part of the Hugs/GHC standard
libraries, providing basic support for dynamic types.

Operations for injecting values of arbitrary type into
a dynamically typed value, Dynamic, are provided, together
with operations for converting dynamic values into a concrete
(monomorphic) type.

The Dynamic implementation provided is closely based on code
contained in Hugs library of the same name.

NOTE: test code at the end, but commented out.

\begin{code}
module Dynamic
	(
	-- dynamic type
	  Dynamic	-- abstract, instance of: Show, Typeable
	, toDyn		-- :: Typeable a => a -> Dynamic
	, fromDyn	-- :: Typeable a => Dynamic -> a -> a
	, fromDynamic	-- :: Typeable a => Dynamic -> Maybe a
	
	-- type representation

	, Typeable(
	     typeOf)	-- :: a -> TypeRep

	  -- Dynamic defines Typeable instances for the following
	-- Prelude types: [a], (), (a,b), (a,b,c), (a,b,c,d),
	-- (a,b,c,d,e), (a->b), (Array a b), Bool, Char,
	-- (Complex a), Double, (Either a b), Float, Handle,
	-- Int, Integer, (IO a), (Maybe a), Ordering

	, TypeRep	-- abstract, instance of: Eq, Show, Typeable
	, TyCon		-- abstract, instance of: Eq, Show, Typeable

	-- type representation constructors/operators:
	, mkTyCon	-- :: String  -> TyCon
	, mkAppTy	-- :: TyCon   -> [TypeRep] -> TypeRep
	, mkFunTy	-- :: TypeRep -> TypeRep   -> TypeRep
	, applyTy	-- :: TypeRep -> TypeRep   -> Maybe TypeRep

	-- 
	-- let fTy = mkTyCon "Foo" in show (mkAppTy (mkTyCon ",,")
	--                                 [fTy,fTy,fTy])
	-- 
	-- returns "(Foo,Foo,Foo)"
	--
	-- The TypeRep Show instance promises to print tuple types
	-- correctly. Tuple type constructors are specified by a 
	-- sequence of commas, e.g., (mkTyCon ",,,,") returns
	-- the 5-tuple tycon.
	) where

import IOExts		( unsafePerformIO,
			  IORef, newIORef, readIORef, writeIORef
			)

import Array		( Array )
import Complex		( Complex )
import IO		( Handle )

import Addr		( Addr, AddrOff )
import ForeignObj	( ForeignObj )
import Int		( Int8, Int16, Int32, Int64 )
import StablePtr	( StablePtr )
import ST		( ST )
import Word		( Word8, Word16, Word32, Word64 )

import ByteArray	( ByteArray )
import IArray		( UArray )
import IOExts		( IOArray )
import MArray		( IOUArray, STUArray )
import MutableArray	( MutableByteArray )
import PackedString	( PackedString )
import ST		( STArray )
import StableName	( StableName )
import Weak		( Weak )

#ifdef __HUGS__
import PrelPrim 	( primUnsafeCoerce
			, Dynamic(..) , TyCon(..) , TypeRep(..)
			)
			
unsafeCoerce :: a -> b
unsafeCoerce = primUnsafeCoerce
#else
import GlaExts
import PrelDynamic

unsafeCoerce :: a -> b
unsafeCoerce = unsafeCoerce#
#endif
\end{code}

The dynamic type is represented by Dynamic, carrying
the dynamic value along with its type representation:

\begin{code}
-- the instance just prints the type representation.
instance Show Dynamic where
   showsPrec _ (Dynamic t _) = 
          showString "<<" . 
	  showsPrec 0 t   . 
	  showString ">>"
\end{code}

Operations for going to and from Dynamic:

\begin{code}
toDyn :: Typeable a => a -> Dynamic
toDyn v = Dynamic (typeOf v) (unsafeCoerce v)

fromDyn :: Typeable a => Dynamic -> a -> a
fromDyn (Dynamic t v) def
  | typeOf def == t = unsafeCoerce v
  | otherwise       = def

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic (Dynamic t v) =
  case unsafeCoerce v of 
    r | t == typeOf r -> Just r
      | otherwise     -> Nothing
\end{code}

(Abstract) universal datatype:

\begin{code}
instance Show TypeRep where
  showsPrec p (App tycon tys) =
    case tys of
      [] -> showsPrec p tycon
      [x] | tycon == listTc    -> showChar '[' . shows x . showChar ']'
      xs  
        | isTupleTyCon tycon -> showTuple tycon xs
	| otherwise	     ->
	    showParen (p > 9) $
   	    showsPrec p tycon . 
	    showChar ' '      . 
	    showArgs tys

  showsPrec p (Fun f a) =
     showParen (p > 8) $
     showsPrec 9 f . showString " -> " . showsPrec 8 a
\end{code}

To make it possible to convert values with user-defined types
into type Dynamic, we need a systematic way of getting
the type representation of an arbitrary type. A type
class provides just the ticket,

\begin{code}
class Typeable a where
  typeOf :: a -> TypeRep
\end{code}

NOTE: The argument to the overloaded `typeOf' is only
used to carry type information, and Typeable instances
should *never* *ever* look at its value.

\begin{code}
isTupleTyCon :: TyCon -> Bool
isTupleTyCon (TyCon _ (',':_)) = True
isTupleTyCon _		       = False

instance Show TyCon where
  showsPrec _ (TyCon _ s) = showString s

\end{code}
 
If we enforce the restriction that there is only one
@TyCon@ for a type & it is shared among all its uses,
we can map them onto Ints very simply. The benefit is,
of course, that @TyCon@s can then be compared efficiently.

Provided the implementor of other @Typeable@ instances
takes care of making all the @TyCon@s CAFs (toplevel constants),
this will work. 

If this constraint does turn out to be a sore thumb, changing
the Eq instance for TyCons is trivial.

\begin{code}
mkTyCon :: String -> TyCon
mkTyCon str = unsafePerformIO $ do
   v <- readIORef uni
   writeIORef uni (v+1)
   return (TyCon v str)

{-# NOINLINE uni #-}
uni :: IORef Int
uni = unsafePerformIO ( newIORef 0 )
\end{code}

Some (Show.TypeRep) helpers:

\begin{code}
showArgs :: Show a => [a] -> ShowS
showArgs [] = id
showArgs [a] = showsPrec 10 a
showArgs (a:as) = showsPrec 10 a . showString " " . showArgs as 

showTuple :: TyCon -> [TypeRep] -> ShowS
showTuple (TyCon _ str) args = showChar '(' . go str args
 where
  go [] [a] = showsPrec 10 a . showChar ')'
  go _  []  = showChar ')' -- a failure condition, really.
  go (',':xs) (a:as) = showsPrec 10 a . showChar ',' . go xs as
  go _ _   = showChar ')'
\end{code}

\begin{code}
mkAppTy  :: TyCon   -> [TypeRep] -> TypeRep
mkAppTy tyc args = App tyc args

mkFunTy  :: TypeRep -> TypeRep   -> TypeRep
mkFunTy f a = Fun f a
\end{code}

Auxillary functions

\begin{code}
-- (f::(a->b)) `dynApply` (x::a) = (f a)::b
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dynamic t1 f) (Dynamic t2 x) =
  case applyTy t1 t2 of
    Just t3 -> Just (Dynamic t3 ((unsafeCoerce f) x))
    Nothing -> Nothing

dynApp :: Dynamic -> Dynamic -> Dynamic
dynApp f x = case dynApply f x of 
             Just r -> r
             Nothing -> error ("Type error in dynamic application.\n" ++
                               "Can't apply function " ++ show f ++
                               " to argument " ++ show x)

applyTy :: TypeRep -> TypeRep -> Maybe TypeRep
applyTy (Fun t1 t2) t3
  | t1 == t3    = Just t2
applyTy _ _     = Nothing

\end{code}

Prelude types

\begin{code}
listTc :: TyCon
listTc = mkTyCon "[]"

instance Typeable a => Typeable [a] where
  typeOf ls = mkAppTy listTc [typeOf ((undefined:: [a] -> a) ls)]

unitTc :: TyCon
unitTc = mkTyCon "()"

instance Typeable () where
  typeOf _ = mkAppTy unitTc []

tup2Tc :: TyCon
tup2Tc = mkTyCon ","

instance (Typeable a, Typeable b) => Typeable (a,b) where
  typeOf tu = mkAppTy tup2Tc [typeOf ((undefined :: (a,b) -> a) tu),
			      typeOf ((undefined :: (a,b) -> b) tu)]

tup3Tc :: TyCon
tup3Tc = mkTyCon ",,"

instance ( Typeable a , Typeable b , Typeable c) => Typeable (a,b,c) where
  typeOf tu = mkAppTy tup3Tc [typeOf ((undefined :: (a,b,c) -> a) tu),
			      typeOf ((undefined :: (a,b,c) -> b) tu),
			      typeOf ((undefined :: (a,b,c) -> c) tu)]

tup4Tc :: TyCon
tup4Tc = mkTyCon ",,,"

instance ( Typeable a
	 , Typeable b
	 , Typeable c
	 , Typeable d) => Typeable (a,b,c,d) where
  typeOf tu = mkAppTy tup4Tc [typeOf ((undefined :: (a,b,c,d) -> a) tu),
			      typeOf ((undefined :: (a,b,c,d) -> b) tu),
			      typeOf ((undefined :: (a,b,c,d) -> c) tu),
			      typeOf ((undefined :: (a,b,c,d) -> d) tu)]

tup5Tc :: TyCon
tup5Tc = mkTyCon ",,,,"

instance ( Typeable a
	 , Typeable b
	 , Typeable c
	 , Typeable d
	 , Typeable e) => Typeable (a,b,c,d,e) where
  typeOf tu = mkAppTy tup5Tc [typeOf ((undefined :: (a,b,c,d,e) -> a) tu),
			      typeOf ((undefined :: (a,b,c,d,e) -> b) tu),
			      typeOf ((undefined :: (a,b,c,d,e) -> c) tu),
			      typeOf ((undefined :: (a,b,c,d,e) -> d) tu),
			      typeOf ((undefined :: (a,b,c,d,e) -> e) tu)]

instance (Typeable a, Typeable b) => Typeable (a -> b) where
  typeOf f = mkFunTy (typeOf ((undefined :: (a -> b) -> a) f))
		     (typeOf ((undefined :: (a -> b) -> b) f))

arrayTc :: TyCon
arrayTc = mkTyCon "Array"

instance (Typeable a, Typeable b) => Typeable (Array a b) where
  typeOf a = mkAppTy arrayTc [typeOf ((undefined :: Array a b -> a) a),
			      typeOf ((undefined :: Array a b -> b) a)]

boolTc :: TyCon
boolTc = mkTyCon "Bool"

instance Typeable Bool where
  typeOf _ = mkAppTy boolTc []
  
charTc :: TyCon
charTc = mkTyCon "Char"

instance Typeable Char where
  typeOf _ = mkAppTy charTc []
  
complexTc :: TyCon
complexTc = mkTyCon "Complex"

instance Typeable a => Typeable (Complex a) where
  typeOf a = mkAppTy complexTc [typeOf ((undefined :: Complex a -> a) a)]

doubleTc :: TyCon
doubleTc = mkTyCon "Double"

instance Typeable Double where
  typeOf _ = mkAppTy doubleTc []

eitherTc :: TyCon
eitherTc = mkTyCon "Either"

instance (Typeable a, Typeable b) => Typeable (Either a b) where
  typeOf ei = mkAppTy eitherTc [typeOf ((undefined :: Either a b -> a) ei),
			        typeOf ((undefined :: Either a b -> b) ei)]

floatTc :: TyCon
floatTc = mkTyCon "Float"

instance Typeable Float where
  typeOf _ = mkAppTy floatTc []
  
handleTc :: TyCon
handleTc = mkTyCon "Handle"

instance Typeable Handle where
  typeOf _ = mkAppTy handleTc []

intTc :: TyCon
intTc = mkTyCon "Int"

instance Typeable Int where
  typeOf _ = mkAppTy intTc []
  
integerTc :: TyCon
integerTc = mkTyCon "Integer"

instance Typeable Integer where
  typeOf _ = mkAppTy integerTc []

iOTc :: TyCon
iOTc = mkTyCon "IO"

instance Typeable a => Typeable (IO a) where
  typeOf action = mkAppTy iOTc [typeOf ((undefined :: IO a -> a) action)]

maybeTc :: TyCon
maybeTc = mkTyCon "Maybe"

instance Typeable a => Typeable (Maybe a) where
  typeOf mb = mkAppTy maybeTc [typeOf ((undefined :: Maybe a -> a) mb)]

orderingTc :: TyCon
orderingTc = mkTyCon "Ordering"

instance Typeable Ordering where
  typeOf _ = mkAppTy orderingTc []
\end{code}

Hugs/GHC extension lib types

\begin{code}
addrTc :: TyCon
addrTc = mkTyCon "Addr"

instance Typeable Addr where
  typeOf _ = mkAppTy addrTc []

addrOffTc :: TyCon
addrOffTc = mkTyCon "AddrOff"

instance Typeable AddrOff where
  typeOf _ = mkAppTy addrOffTc []

dynamicTc :: TyCon
dynamicTc = mkTyCon "Dynamic"

instance Typeable Dynamic where
  typeOf _ = mkAppTy dynamicTc []

foreignObjTc :: TyCon
foreignObjTc = mkTyCon "ForeignObj"

instance Typeable ForeignObj where
  typeOf _ = mkAppTy foreignObjTc []

iORefTc :: TyCon
iORefTc = mkTyCon "IORef"

instance Typeable a => Typeable (IORef a) where
  typeOf ref = mkAppTy iORefTc [typeOf ((undefined :: IORef a -> a) ref)]

int8Tc :: TyCon
int8Tc = mkTyCon "Int8"

instance Typeable Int8 where
  typeOf _ = mkAppTy int8Tc []

int16Tc :: TyCon
int16Tc = mkTyCon "Int16"

instance Typeable Int16 where
  typeOf _ = mkAppTy int16Tc []

int32Tc :: TyCon
int32Tc = mkTyCon "Int32"

instance Typeable Int32 where
  typeOf _ = mkAppTy int32Tc []

int64Tc :: TyCon
int64Tc = mkTyCon "Int64"


instance Typeable Int64 where
  typeOf _ = mkAppTy int64Tc []

sTTc :: TyCon
sTTc = mkTyCon "ST"

instance (Typeable a, Typeable b) => Typeable (ST a b) where
  typeOf st = mkAppTy sTTc [typeOf ((undefined :: ST a b -> a) st),
			    typeOf ((undefined :: ST a b -> b) st)]

stablePtrTc :: TyCon
stablePtrTc = mkTyCon "StablePtr"

instance Typeable a => Typeable (StablePtr a) where
  typeOf sp = mkAppTy stablePtrTc [typeOf ((undefined :: StablePtr a -> a) sp)]

tyConTc :: TyCon
tyConTc = mkTyCon "TyCon"

instance Typeable TyCon where
  typeOf _ = mkAppTy tyConTc []

typeRepTc :: TyCon
typeRepTc = mkTyCon "TypeRep"

instance Typeable TypeRep where
  typeOf _ = mkAppTy typeRepTc []

word8Tc :: TyCon
word8Tc = mkTyCon "Word8"

instance Typeable Word8 where
  typeOf _ = mkAppTy word8Tc []

word16Tc :: TyCon
word16Tc = mkTyCon "Word16"

instance Typeable Word16 where
  typeOf _ = mkAppTy word16Tc []

word32Tc :: TyCon
word32Tc = mkTyCon "Word32"

instance Typeable Word32 where
  typeOf _ = mkAppTy word32Tc []

word64Tc :: TyCon
word64Tc = mkTyCon "Word64"

instance Typeable Word64 where
  typeOf _ = mkAppTy word64Tc []
\end{code}

GHC extension lib types

\begin{code}
byteArrayTc :: TyCon
byteArrayTc = mkTyCon "ByteArray"

instance Typeable a => Typeable (ByteArray a) where
  typeOf sp = mkAppTy byteArrayTc [typeOf ((undefined :: ByteArray a -> a) sp)]

iOArrayTc :: TyCon
iOArrayTc = mkTyCon "IOArray"

instance (Typeable a, Typeable b) => Typeable (IOArray a b) where
  typeOf a = mkAppTy iOArrayTc [typeOf ((undefined :: IOArray a b -> a) a),
				typeOf ((undefined :: IOArray a b -> b) a)]

iOUArrayTc :: TyCon
iOUArrayTc = mkTyCon "IOUArray"

instance (Typeable a, Typeable b) => Typeable (IOUArray a b) where
  typeOf a = mkAppTy iOUArrayTc [typeOf ((undefined :: IOUArray a b -> a) a),
				 typeOf ((undefined :: IOUArray a b -> b) a)]

mutableByteArrayTc :: TyCon
mutableByteArrayTc = mkTyCon "MutableByteArray"

instance (Typeable a, Typeable b) => Typeable (MutableByteArray a b) where
  typeOf a = mkAppTy mutableByteArrayTc [typeOf ((undefined :: MutableByteArray a b -> a) a),
					 typeOf ((undefined :: MutableByteArray a b -> b) a)]

packedStringTc :: TyCon
packedStringTc = mkTyCon "PackedString"

instance Typeable PackedString where
  typeOf _ = mkAppTy packedStringTc []

sTArrayTc :: TyCon
sTArrayTc = mkTyCon "STArray"

instance (Typeable a, Typeable b, Typeable c) => Typeable (STArray a b c) where
  typeOf a = mkAppTy sTArrayTc [typeOf ((undefined :: STArray a b c -> a) a),
				typeOf ((undefined :: STArray a b c -> b) a),
				typeOf ((undefined :: STArray a b c -> c) a)]

sTUArrayTc :: TyCon
sTUArrayTc = mkTyCon "STUArray"

instance (Typeable a, Typeable b, Typeable c) => Typeable (STUArray a b c) where
  typeOf a = mkAppTy sTUArrayTc [typeOf ((undefined :: STUArray a b c -> a) a),
				 typeOf ((undefined :: STUArray a b c -> b) a),
				 typeOf ((undefined :: STUArray a b c -> c) a)]

stableNameTc :: TyCon
stableNameTc = mkTyCon "StableName"

instance Typeable a => Typeable (StableName a) where
  typeOf sp = mkAppTy stableNameTc [typeOf ((undefined :: StableName a -> a) sp)]

uArrayTc :: TyCon
uArrayTc = mkTyCon "UArray"

instance (Typeable a, Typeable b) => Typeable (UArray a b) where
  typeOf a = mkAppTy uArrayTc [typeOf ((undefined :: UArray a b -> a) a),
			       typeOf ((undefined :: UArray a b -> b) a)]

weakTc :: TyCon
weakTc = mkTyCon "Weak"

instance Typeable a => Typeable (Weak a) where
  typeOf sp = mkAppTy weakTc [typeOf ((undefined :: Weak a -> a) sp)]
\end{code}
