% -----------------------------------------------------------------------------
% $Id: CTypesISO.lhs,v 1.1.2.6 2000/06/20 10:31:18 simonmar Exp $
%
% (c) The FFI task force, 2000
%

A mapping of C types defined by the ISO C standard to corresponding Haskell
types. Like CTypes, this is a cool hack...

#include "cbits/CTypes.h"

\begin{code}
module CTypesISO
	( -- Integral types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable, Bounded, Real, Integral, Bits
	  CPtrdiff(..), CSize(..), CWchar(..), CSigAtomic(..)

	  -- Numeric types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable
	, CClock(..),   CTime(..),

	, CFile,        CFpos,     CJmpBuf
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
INTEGRAL_TYPE(CPtrdiff,HTYPE_PTRDIFF_T)
INTEGRAL_TYPE(CSize,HTYPE_SIZE_T)
INTEGRAL_TYPE(CWchar,HTYPE_WCHAR_T)
INTEGRAL_TYPE(CSigAtomic,HTYPE_SIG_ATOMIC_T)

{-# RULES
"fromIntegral/a->CPtrdiff"   fromIntegral = \x -> CPtrdiff   (fromIntegral x)
"fromIntegral/a->CSize"      fromIntegral = \x -> CSize      (fromIntegral x)
"fromIntegral/a->CWchar"     fromIntegral = \x -> CWchar     (fromIntegral x)
"fromIntegral/a->CSigAtomic" fromIntegral = \x -> CSigAtomic (fromIntegral x)

"fromIntegral/CPtrdiff->a"   fromIntegral = \(CPtrdiff   x) -> fromIntegral x
"fromIntegral/CSize->a"      fromIntegral = \(CSize      x) -> fromIntegral x
"fromIntegral/CWchar->a"     fromIntegral = \(CWchar     x) -> fromIntegral x
"fromIntegral/CSigAtomic->a" fromIntegral = \(CSigAtomic x) -> fromIntegral x
 #-}

NUMERIC_TYPE(CClock,HTYPE_CLOCK_T)
NUMERIC_TYPE(CTime,HTYPE_TIME_T)

-- TODO: Instances. But which...?  :-}

data CFile = CFile

data CFpos = CFpos

data CJmpBuf = CJmpBuf

-- C99 types which are still missing include:
-- intptr_t, uintptr_t, intmax_t, uintmax_t, wint_t, wctrans_t, wctype_t
\end{code}
