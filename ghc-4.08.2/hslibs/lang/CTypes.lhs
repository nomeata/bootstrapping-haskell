% -----------------------------------------------------------------------------
% $Id: CTypes.lhs,v 1.3.2.4 2000/06/09 09:46:56 simonmar Exp $
%
% (c) The FFI task force, 2000
%

A mapping of C types to corresponding Haskell types. A cool hack...

#include "cbits/CTypes.h"

\begin{code}
module CTypes
	( -- Integral types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable, Bounded, Real, Integral, Bits
	  CChar(..),    CSChar(..),  CUChar(..)
	, CShort(..),   CUShort(..), CInt(..),    CUInt(..)
	, CLong(..),    CULong(..),  CLLong(..),  CULLong(..)

	  -- Floating types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable, Real, Fractional, Floating, RealFrac, RealFloat
	, CFloat(..),   CDouble(..), CLDouble(..)
	) where
\end{code}

\begin{code}
import PrelBase	( unsafeCoerce# )
import PrelReal	( Integral(toInt) )
import Bits	( Bits(..) )
import Dynamic	( Typeable(..) )
import Int	( Int8,  Int16,  Int32,  Int64  )
import Word	( Word8, Word16, Word32, Word64 )
import Monad	( liftM )
import Storable	( Storable(..) )
\end{code}

Efficiency hack: We don't really map a newtype over a list,
but do a coercion instead.

\begin{code}
fakeMap :: (a -> b) -> [a] -> [b]
fakeMap _f xs = unsafeCoerce# xs
\end{code}

\begin{code}
INTEGRAL_TYPE(CChar,HTYPE_CHAR)

INTEGRAL_TYPE(CSChar,HTYPE_SIGNED_CHAR)
INTEGRAL_TYPE(CUChar,HTYPE_UNSIGNED_CHAR)

INTEGRAL_TYPE(CShort,HTYPE_SHORT)
INTEGRAL_TYPE(CUShort,HTYPE_UNSIGNED_SHORT)

INTEGRAL_TYPE(CInt,HTYPE_INT)
INTEGRAL_TYPE(CUInt,HTYPE_UNSIGNED_INT)

INTEGRAL_TYPE(CLong,HTYPE_LONG)
INTEGRAL_TYPE(CULong,HTYPE_UNSIGNED_LONG)

INTEGRAL_TYPE(CLLong,HTYPE_LONG_LONG)
INTEGRAL_TYPE(CULLong,HTYPE_UNSIGNED_LONG_LONG)

{-# RULES
"fromIntegral/a->CChar"   fromIntegral = \x -> CChar   (fromIntegral x)
"fromIntegral/a->CSChar"  fromIntegral = \x -> CSChar  (fromIntegral x)
"fromIntegral/a->CUChar"  fromIntegral = \x -> CUChar  (fromIntegral x)
"fromIntegral/a->CShort"  fromIntegral = \x -> CShort  (fromIntegral x)
"fromIntegral/a->CUShort" fromIntegral = \x -> CUShort (fromIntegral x)
"fromIntegral/a->CInt"    fromIntegral = \x -> CInt    (fromIntegral x)
"fromIntegral/a->CUInt"   fromIntegral = \x -> CUInt   (fromIntegral x)
"fromIntegral/a->CLong"   fromIntegral = \x -> CLong   (fromIntegral x)
"fromIntegral/a->CULong"  fromIntegral = \x -> CULong  (fromIntegral x)
"fromIntegral/a->CLLong"  fromIntegral = \x -> CLLong  (fromIntegral x)
"fromIntegral/a->CULLong" fromIntegral = \x -> CULLong (fromIntegral x)

"fromIntegral/CChar->a"   fromIntegral = \(CChar   x) -> fromIntegral x
"fromIntegral/CSChar->a"  fromIntegral = \(CSChar  x) -> fromIntegral x
"fromIntegral/CUChar->a"  fromIntegral = \(CUChar  x) -> fromIntegral x
"fromIntegral/CShort->a"  fromIntegral = \(CShort  x) -> fromIntegral x
"fromIntegral/CUShort->a" fromIntegral = \(CUShort x) -> fromIntegral x
"fromIntegral/CInt->a"    fromIntegral = \(CInt    x) -> fromIntegral x
"fromIntegral/CUInt->a"   fromIntegral = \(CUInt   x) -> fromIntegral x
"fromIntegral/CLong->a"   fromIntegral = \(CLong   x) -> fromIntegral x
"fromIntegral/CULong->a"  fromIntegral = \(CULong  x) -> fromIntegral x
"fromIntegral/CLLong->a"  fromIntegral = \(CLLong  x) -> fromIntegral x
"fromIntegral/CULLong->a" fromIntegral = \(CULLong x) -> fromIntegral x
 #-}

FLOATING_TYPE(CFloat,HTYPE_FLOAT)
FLOATING_TYPE(CDouble,HTYPE_DOUBLE)
-- HACK: Currently no long double in the FFI, so we simply re-use double
FLOATING_TYPE(CLDouble,HTYPE_DOUBLE)
\end{code}
