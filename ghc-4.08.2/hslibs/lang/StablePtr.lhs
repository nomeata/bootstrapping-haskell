% -----------------------------------------------------------------------------
% $Id: StablePtr.lhs,v 1.4 2000/04/13 15:16:53 panne Exp $
%
% (c) The GHC Team, 2000
%

\section[StablePtr]{Module @StablePtr@}

\begin{code}
module StablePtr
        ( StablePtr,       -- abstract
        , makeStablePtr    -- :: a -> IO (StablePtr a)
        , deRefStablePtr   -- :: StablePtr a -> IO a
        , freeStablePtr    -- :: StablePtr a -> IO ()
        , stablePtrToAddr  -- :: StablePtr a -> Addr
        , addrToStablePtr  -- :: Addr -> StablePtr a
        ) where
\end{code}

\begin{code}
import PrelStable	( StablePtr(..), makeStablePtr, deRefStablePtr, freeStablePtr )
import PrelGHC		( unsafeCoerce# )
import PrelAddr		( Addr(..) )
\end{code}

\begin{code}
stablePtrToAddr :: StablePtr a -> Addr
stablePtrToAddr (StablePtr s) = A# (unsafeCoerce# s)

addrToStablePtr :: Addr -> StablePtr a
addrToStablePtr (A# a) = StablePtr (unsafeCoerce# a)
\end{code}
