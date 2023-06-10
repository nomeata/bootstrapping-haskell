% -----------------------------------------------------------------------------
% $Id: StableName.lhs,v 1.1 2000/04/12 11:40:53 panne Exp $
%
% (c) The GHC Team, 2000
%

\section[StableName]{Module @StableName@}

\begin{code}
module StableName
	( StableName {-a-}   -- abstract, instance of Eq
	, makeStableName     -- :: a -> IO (StableName a)
	, hashStableName     -- :: StableName a -> Int
	) where

import PrelBase		( Int(..) )
import PrelIOBase	( IO(..) )
import PrelGHC		( StableName#, makeStableName#
			, eqStableName#, stableNameToInt# )

-----------------------------------------------------------------------------
-- Stable Names

data StableName a = StableName (StableName# a)

makeStableName  :: a -> IO (StableName a)
#if defined(__PARALLEL_HASKELL__)
makeStableName a = 
  error "makeStableName not implemented in parallel Haskell"
#else
makeStableName a = IO $ \ s ->
    case makeStableName# a s of (# s', sn #) -> (# s', StableName sn #)
#endif

hashStableName :: StableName a -> Int
#if defined(__PARALLEL_HASKELL__)
hashStableName (StableName sn) = 
  error "hashStableName not implemented in parallel Haskell"
#else
hashStableName (StableName sn) = I# (stableNameToInt# sn)
#endif

instance Eq (StableName a) where 
#if defined(__PARALLEL_HASKELL__)
    (StableName sn1) == (StableName sn2) = 
      error "eqStableName not implemented in parallel Haskell"
#else
    (StableName sn1) == (StableName sn2) = 
       case eqStableName# sn1 sn2 of
	 0# -> False
	 _  -> True
#endif
\end{code}
