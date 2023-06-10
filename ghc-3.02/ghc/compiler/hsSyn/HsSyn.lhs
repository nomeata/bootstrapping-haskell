%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section{Haskell abstract syntax definition}

This module glues together the pieces of the Haskell abstract syntax,
which is declared in the various \tr{Hs*} modules.  This module,
therefore, is almost nothing but re-exporting.

\begin{code}
module HsSyn (

	-- NB: don't reexport HsCore or HsPragmas;
	-- this module tells about "real Haskell"

	module HsSyn,
	module HsBinds,
	module HsDecls,
	module HsExpr,
	module HsImpExp,
	module HsBasic,
	module HsMatches,
	module HsPat,
	module HsTypes,
	Fixity, NewOrData, IfaceFlavour,

	collectTopBinders, collectMonoBinders
     ) where

#include "HsVersions.h"

-- friends:
import HsBinds
import HsDecls		( HsDecl(..), TyDecl(..), InstDecl(..), ClassDecl(..), 
			  DefaultDecl(..), 
			  FixityDecl(..), 
			  ConDecl(..), ConDetails(..), BangType(..),
			  IfaceSig(..), HsIdInfo,  SpecDataSig(..), 
			  hsDeclName
			)
import HsExpr
import HsImpExp
import HsBasic
import HsMatches
import HsPat
import HsTypes
import HsPragmas	( ClassPragmas, ClassOpPragmas,
			  DataPragmas, GenPragmas, InstancePragmas )
import HsCore
import BasicTypes	( Fixity, Version, NewOrData, IfaceFlavour, Module )

-- others:
import FiniteMap	( FiniteMap )
import Outputable
import SrcLoc		( SrcLoc )
import Bag
import Name		( NamedThing )
\end{code}

All we actually declare here is the top-level structure for a module.
\begin{code}
data HsModule flexi name pat
  = HsModule
	Module			-- module name
	(Maybe Version)		-- source interface version number
	(Maybe [IE name])	-- export list; Nothing => export everything
				-- Just [] => export *nothing* (???)
				-- Just [...] => as you would expect...
	[ImportDecl name]	-- We snaffle interesting stuff out of the
				-- imported interfaces early on, adding that
				-- info to TyDecls/etc; so this list is
				-- often empty, downstream.
	[FixityDecl name]
	[HsDecl flexi name pat]	-- Type, class, value, and interface signature decls
	SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat)
	=> Outputable (HsModule flexi name pat) where

    ppr (HsModule name iface_version exports imports fixities
		      decls src_loc)
      = vcat [
	    case exports of
	      Nothing -> hsep [ptext SLIT("module"), ptext name, ptext SLIT("where")]
	      Just es -> vcat [
			    hsep [ptext SLIT("module"), ptext name, lparen],
			    nest 8 (interpp'SP es),
			    nest 4 (ptext SLIT(") where"))
			  ],
	    pp_nonnull imports,
	    pp_nonnull fixities,
	    pp_nonnull decls
	]
      where
	pp_nonnull [] = empty
	pp_nonnull xs = vcat (map ppr xs)

	pp_iface_version Nothing  = empty
	pp_iface_version (Just n) = hsep [text "{-# INTERFACE", int n, text "#-}"]
\end{code}


%************************************************************************
%*									*
\subsection{Collecting binders from @HsBinds@}
%*									*
%************************************************************************

Get all the binders in some @MonoBinds@, IN THE ORDER OF APPEARANCE.

These functions are here, rather than in HsBinds, to avoid a loop between HsPat and HsBinds.

\begin{verbatim}
...
where
  (x, y) = ...
  f i j  = ...
  [a, b] = ...
\end{verbatim}
it should return @[x, y, f, a, b]@ (remember, order important).

\begin{code}
collectTopBinders :: HsBinds flexi name (InPat name) -> Bag (name,SrcLoc)
collectTopBinders EmptyBinds     = emptyBag
collectTopBinders (MonoBind b _ _) = collectMonoBinders b
collectTopBinders (ThenBinds b1 b2)
 = collectTopBinders b1 `unionBags` collectTopBinders b2

collectMonoBinders :: MonoBinds flexi name (InPat name) -> Bag (name,SrcLoc)
collectMonoBinders EmptyMonoBinds		       = emptyBag
collectMonoBinders (PatMonoBind pat grhss_w_binds loc) = listToBag (map (\v->(v,loc)) (collectPatBinders pat))
collectMonoBinders (FunMonoBind f _ matches loc)       = unitBag (f,loc)
collectMonoBinders (VarMonoBind v expr) 	       = error "collectMonoBinders"
collectMonoBinders (CoreMonoBind v expr) 	       = error "collectMonoBinders"
collectMonoBinders (AndMonoBinds bs1 bs2)
 = collectMonoBinders bs1 `unionBags` collectMonoBinders bs2
\end{code}

