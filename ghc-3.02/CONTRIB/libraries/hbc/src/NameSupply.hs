module NameSupply(NameSupply, initialNameSupply, splitNameSupply, getName, listNameSupply, Name
#if defined(__YALE_HASKELL__)
	, Symbol
#endif
	) where

#if defined(__YALE_HASKELL__)
import Symbol
type Name = Symbol

#else
# if defined(__GLASGOW_HASKELL__)
import GlaExts
import ST
import IOExts   ( unsafeInterleaveIO, unsafePerformIO )

type Name = Int

# else
import LMLgensym
type Name = Int
# endif
#endif

data NameSupply = NameSupply Name NameSupply NameSupply

splitNameSupply :: NameSupply -> (NameSupply,NameSupply)
getName		:: NameSupply -> Name
listNameSupply	:: NameSupply -> [NameSupply]

#if defined(__YALE_HASKELL__)
initialNameSupply :: IO NameSupply
#else
initialNameSupply :: NameSupply
#endif

#if defined(__GLASGOW_HASKELL__)
initialNameSupply = unsafePerformIO mk_supply# -- GHC-specific
  where
    mk_supply# = do
        u  <- unsafeInterleaveIO (_ccall_ genSymZh)
	s1 <- unsafeInterleaveIO mk_supply#
	s2 <- unsafeInterleaveIO mk_supply#
	return (NameSupply u s1 s2)
#endif

#if defined(__YALE_HASKELL__)
initialNameSupply :: IO NameSupply
initialNameSupply
 = let
     mk_supply =
 	  unsafeInterleaveIO (genSymbol "NameSupply")	>>= \ sym ->
	  unsafeInterleaveIO mk_supply			>>= \ supply1 ->
	  unsafeInterleaveIO mk_supply			>>= \ supply2 ->
	  return (NameSupply sym supply1 supply2)
   in
   mk_supply
#endif

#if defined(__HBC__)
initialNameSupply = gen ()
	where gen n = NameSupply (__gensym n) (gen n) (gen n)
#endif

splitNameSupply (NameSupply _ s1 s2) = (s1, s2)

getName (NameSupply k _ _) = k

listNameSupply (NameSupply _ s1 s2) = s1 : listNameSupply s2
