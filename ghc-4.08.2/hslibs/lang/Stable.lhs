This module is deprecated: Use StablePtr, StableName, or Foreign instead

\begin{code}
module Stable
       	( module StablePtr
	, module StableName
	) where

import StablePtr	( StablePtr, makeStablePtr, deRefStablePtr, freeStablePtr )
import StableName
\end{code}
