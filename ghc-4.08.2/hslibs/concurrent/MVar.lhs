%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[MVars]{MVars: Synchronising variables}

\begin{code}
module MVar
	( MVar		-- abstract
	, newEmptyMVar  -- :: IO (MVar a)
	, newMVar 	-- :: a -> IO (MVar a)
	, takeMVar 	-- :: MVar a -> IO a
	, putMVar  	-- :: MVar a -> a -> IO ()
	, readMVar 	-- :: MVar a -> IO a
	, swapMVar 	-- :: MVar a -> a -> IO a
	, tryTakeMVar   -- :: MVar a -> IO (Maybe a)
	, isEmptyMVar	-- :: MVar a -> IO Bool
    ) where

#ifndef __HUGS__
import PrelConc	( MVar, newEmptyMVar, newMVar, takeMVar, putMVar
		, readMVar, swapMVar, tryTakeMVar, isEmptyMVar
		)
#else
import PrelPrim	( MVar, newEmptyMVar, newMVar, takeMVar, putMVar
		, readMVar, swapMVar, tryTakeMVar, isEmptyMVar
		)
#endif
\end{code}
