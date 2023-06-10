% -----------------------------------------------------------------------------
% $Id: Foreign.lhs,v 1.9.2.2 2000/06/20 10:35:40 simonmar Exp $
%
% (c) The FFI task force, 2000
%

A collection of data types, classes, and functions for interfacing
with another programming language. This is only a convenience module
in the future, but currently it has the additional task of hiding
those entities exported from other modules, which are not part of the
FFI proposal.

\begin{code}
module Foreign
        ( module Int
	, module Word
	, module Addr
	, module ForeignObj
	, module StablePtr
        , module Storable
        , module IOExts
        ) where
\end{code}

\begin{code}
import Int        	( Int8,  Int16,  Int32,  Int64 )
import Word		( Word8, Word16, Word32, Word64 )
import Addr		( Addr, AddrOff, nullAddr, alignAddr, plusAddr, minusAddr )
import ForeignObj
#ifndef __PARALLEL_HASKELL__
			( ForeignObj, newForeignObj, addForeignFinalizer, foreignObjToAddr
			, makeForeignObj -- *grrrrr...*
			)
#endif
import StablePtr
import Storable
import IOExts		( freeHaskellFunctionPtr )
\end{code}
