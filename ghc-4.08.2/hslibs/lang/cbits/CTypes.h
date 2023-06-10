/* -----------------------------------------------------------------------------
 * $Id: CTypes.h,v 1.1.2.1 2000/05/24 16:07:25 simonmar Exp $
 *
 * Dirty CPP hackery for CTypes/CTypesISO
 *
 * (c) The FFI task force, 2000
 * -------------------------------------------------------------------------- */

#include "MachDeps.h"

/* As long as there is no automatic derivation of classes for newtypes we resort
   to extremely dirty cpp-hackery.   :-P   Some care has to be taken when the
   macros below are modified, otherwise the layout rule will bite you. */

#define NUMERIC_TYPE(T,B) \
newtype T = T B deriving (Eq, Ord) ; \
INSTANCE_NUM(T) ; \
INSTANCE_READ(T) ; \
INSTANCE_SHOW(T) ; \
INSTANCE_ENUM(T) ; \
INSTANCE_TYPEABLE(T) ; \
INSTANCE_STORABLE(T)

#define INTEGRAL_TYPE(T,B) \
NUMERIC_TYPE(T,B) ; \
INSTANCE_BOUNDED(T) ; \
INSTANCE_REAL(T) ; \
INSTANCE_INTEGRAL(T) ; \
INSTANCE_BITS(T)

#define FLOATING_TYPE(T,B) \
NUMERIC_TYPE(T,B) ; \
INSTANCE_REAL(T) ; \
INSTANCE_FRACTIONAL(T) ; \
INSTANCE_FLOATING(T) ; \
INSTANCE_REALFRAC(T) ; \
INSTANCE_REALFLOAT(T)

#define INSTANCE_READ(T) \
instance Read T where { \
   readsPrec p s = fakeMap (\(x, t) -> (T x, t)) (readsPrec p s) }

#define INSTANCE_SHOW(T) \
instance Show T where { \
   showsPrec p (T x) = showsPrec p x }

#define INSTANCE_NUM(T) \
instance Num T where { \
   (T i) + (T j) = T (i + j) ; \
   (T i) - (T j) = T (i - j) ; \
   (T i) * (T j) = T (i * j) ; \
   negate  (T i) = T (negate i) ; \
   abs     (T i) = T (abs    i) ; \
   signum  (T i) = T (signum i) ; \
   fromInteger x = T (fromInteger x) }

#define INSTANCE_TYPEABLE(T) \
instance Typeable T where { \
  typeOf (T x) = typeOf x }

#define INSTANCE_STORABLE(T) \
instance Storable T where { \
   sizeOf    (T x)       = sizeOf x ; \
   alignment (T x)       = alignment x ; \
   peekElemOff a i       = liftM T (peekElemOff a i) ; \
   pokeElemOff a i (T x) = pokeElemOff a i x }

#define INSTANCE_BOUNDED(T) \
instance Bounded T where { \
   minBound = T minBound ; \
   maxBound = T maxBound }

#define INSTANCE_ENUM(T) \
instance Enum T where { \
   succ           (T i)             = T (succ i) ; \
   pred           (T i)             = T (pred i) ; \
   toEnum               x           = T (toEnum x) ; \
   fromEnum       (T i)             = fromEnum i ; \
   enumFrom       (T i)             = fakeMap T (enumFrom i) ; \
   enumFromThen   (T i) (T j)       = fakeMap T (enumFromThen i j) ; \
   enumFromTo     (T i) (T j)       = fakeMap T (enumFromTo i j) ; \
   enumFromThenTo (T i) (T j) (T k) = fakeMap T (enumFromThenTo i j k) }

#define INSTANCE_REAL(T) \
instance Real T where { \
   toRational (T i) = toRational i }

#define INSTANCE_INTEGRAL(T) \
instance Integral T where { \
   (T i) `quot`    (T j) = T (i `quot` j) ; \
   (T i) `rem`     (T j) = T (i `rem`  j) ; \
   (T i) `div`     (T j) = T (i `div`  j) ; \
   (T i) `mod`     (T j) = T (i `mod`  j) ; \
   (T i) `quotRem` (T j) = let (q,r) = i `quotRem` j in (T q, T r) ; \
   (T i) `divMod`  (T j) = let (d,m) = i `divMod`  j in (T d, T m) ; \
   toInteger (T i)       = toInteger i ; \
   toInt     (T i)       = toInt     i }

#define INSTANCE_BITS(T) \
instance Bits T where { \
  (T x) .&.     (T y)   = T (x .&.   y) ; \
  (T x) .|.     (T y)   = T (x .|.   y) ; \
  (T x) `xor`   (T y)   = T (x `xor` y) ; \
  complement    (T x)   = T (complement x) ; \
  shift         (T x) n = T (shift x n) ; \
  rotate        (T x) n = T (rotate x n) ; \
  bit                 n = T (bit n) ; \
  setBit        (T x) n = T (setBit x n) ; \
  clearBit      (T x) n = T (clearBit x n) ; \
  complementBit (T x) n = T (complementBit x n) ; \
  testBit       (T x) n = testBit x n ; \
  bitSize       (T x)   = bitSize x ; \
  isSigned      (T x)   = isSigned x }

#define INSTANCE_FRACTIONAL(T) \
instance Fractional T where { \
   (T x) / (T y)  = T (x / y) ; \
   recip   (T x)  = T (recip x) ; \
   fromRational	r = T (fromRational r) }

#define INSTANCE_FLOATING(T) \
instance Floating T where { \
   pi                    = pi ; \
   exp   (T x)           = T (exp   x) ; \
   log   (T x)           = T (log   x) ; \
   sqrt  (T x)           = T (sqrt  x) ; \
   (T x) **        (T y) = T (x ** y) ; \
   (T x) `logBase` (T y) = T (x `logBase` y) ; \
   sin   (T x)           = T (sin   x) ; \
   cos   (T x)           = T (cos   x) ; \
   tan   (T x)           = T (tan   x) ; \
   asin  (T x)           = T (asin  x) ; \
   acos  (T x)           = T (acos  x) ; \
   atan  (T x)           = T (atan  x) ; \
   sinh  (T x)           = T (sinh  x) ; \
   cosh  (T x)           = T (cosh  x) ; \
   tanh  (T x)           = T (tanh  x) ; \
   asinh (T x)           = T (asinh x) ; \
   acosh (T x)           = T (acosh x) ; \
   atanh (T x)           = T (atanh x) }

#define INSTANCE_REALFRAC(T) \
instance RealFrac T where { \
   properFraction (T x) = let (m,y) = properFraction x in (m, T y) ; \
   truncate (T x) = truncate x ; \
   round    (T x) = round x ; \
   ceiling  (T x) = ceiling x ; \
   floor    (T x) = floor x }

#define INSTANCE_REALFLOAT(T) \
instance RealFloat T where { \
   floatRadix     (T x) = floatRadix x ; \
   floatDigits    (T x) = floatDigits x ; \
   floatRange     (T x) = floatRange x ; \
   decodeFloat    (T x) = decodeFloat x ; \
   encodeFloat m n      = T (encodeFloat m n) ; \
   exponent       (T x) = exponent x ; \
   significand    (T x) = T (significand  x) ; \
   scaleFloat n   (T x) = T (scaleFloat n x) ; \
   isNaN          (T x) = isNaN x ; \
   isInfinite     (T x) = isInfinite x ; \
   isDenormalized (T x) = isDenormalized x ; \
   isNegativeZero (T x) = isNegativeZero x ; \
   isIEEE         (T x) = isIEEE x ; \
   (T x) `atan2`  (T y) = T (x `atan2` y) }
