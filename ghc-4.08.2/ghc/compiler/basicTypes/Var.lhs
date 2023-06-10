s%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{@Vars@: Variables}

\begin{code}
module Var (
	Var, VarDetails,		-- Abstract
	varName, varUnique, varInfo, varType,
	setVarName, setVarUnique, setVarType, setVarOcc,


	-- TyVars
	TyVar,
	tyVarName, tyVarKind,
	setTyVarName, setTyVarUnique,
	mkTyVar, mkSysTyVar, isTyVar, isSigTyVar,
	newMutTyVar, newSigTyVar,
	readMutTyVar, writeMutTyVar, isMutTyVar, makeTyVarImmutable,

        -- UVars
        UVar,
        isUVar,
        mkUVar, mkNamedUVar,

	-- Ids
	Id, DictId,
	idName, idType, idUnique, idInfo, modifyIdInfo, maybeModifyIdInfo,
	setIdName, setIdUnique, setIdInfo, lazySetIdInfo, zapIdInfo,
	mkIdVar, isId, externallyVisibleId
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TypeRep( Type, Kind )
import {-# SOURCE #-}	IdInfo( IdInfo, seqIdInfo, vanillaIdInfo )

import Unique		( Unique, Uniquable(..), mkUniqueGrimily, getKey )
import Name		( Name, OccName, NamedThing(..),
			  setNameUnique, setNameOcc, nameUnique, 
			  mkSysLocalName, isExternallyVisibleName
			)
import BasicTypes	( Unused )
import Outputable

import IOExts		( IORef, newIORef, readIORef, writeIORef )
\end{code}



%************************************************************************
%*									*
\subsection{The main data type declarations}
%*									*
%************************************************************************


Every @Var@ has a @Unique@, to uniquify it and for fast comparison, a
@Type@, and an @IdInfo@ (non-essential info about it, e.g.,
strictness).  The essential info about different kinds of @Vars@ is
in its @VarDetails@.

\begin{code}
data Var
  = Var {
	varName    :: Name,
	realUnique :: Int#,		-- Key for fast comparison
					-- Identical to the Unique in the name,
					-- cached here for speed
	varType    :: Type,
	varDetails :: VarDetails,
	varInfo    :: IdInfo		-- Only used for Ids at the moment
    }

data VarDetails
  = AnId
  | TyVar
  | MutTyVar (IORef (Maybe Type)) 	-- Used during unification;
	     Bool			-- True <=> this is a type signature variable, which
					--	    should not be unified with a non-tyvar type
  | UVar                                -- Usage variable

-- For a long time I tried to keep mutable Vars statically type-distinct
-- from immutable Vars, but I've finally given up.   It's just too painful.
-- After type checking there are no MutTyVars left, but there's no static check
-- of that fact.
\end{code}

\begin{code}
instance Outputable Var where
  ppr var = ppr (varName var)

instance Show Var where
  showsPrec p var = showsPrecSDoc p (ppr var)

instance NamedThing Var where
  getName = varName

instance Uniquable Var where
  getUnique = varUnique

instance Eq Var where
    a == b = realUnique a ==# realUnique b

instance Ord Var where
    a <= b = realUnique a <=# realUnique b
    a <	 b = realUnique a <#  realUnique b
    a >= b = realUnique a >=# realUnique b
    a >	 b = realUnique a >#  realUnique b
    a `compare` b = varUnique a `compare` varUnique b
\end{code}


\begin{code}
varUnique :: Var -> Unique
varUnique (Var {realUnique = uniq}) = mkUniqueGrimily uniq

setVarUnique :: Var -> Unique -> Var
setVarUnique var@(Var {varName = name}) uniq 
  = var {realUnique = getKey uniq, 
	 varName = setNameUnique name uniq}

setVarName :: Var -> Name -> Var
setVarName var new_name
  = var { realUnique = getKey (getUnique new_name), varName = new_name }

setVarOcc :: Var -> OccName -> Var
setVarOcc var new_occ
  = var { varName = setNameOcc (varName var) new_occ }

setVarType :: Var -> Type -> Var
setVarType var ty = var {varType = ty}
\end{code}


%************************************************************************
%*									*
\subsection{Type variables}
%*									*
%************************************************************************

\begin{code}
type TyVar = Var
\end{code}

\begin{code}
tyVarName = varName
tyVarKind = varType

setTyVarUnique = setVarUnique
setTyVarName   = setVarName
\end{code}

\begin{code}
mkTyVar :: Name -> Kind -> TyVar
mkTyVar name kind = Var { varName    = name
			, realUnique = getKey (nameUnique name)
			, varType    = kind
			, varDetails = TyVar
#ifdef DEBUG
			, varInfo = pprPanic "looking at IdInfo of a tyvar" (ppr name)
#endif
			}

mkSysTyVar :: Unique -> Kind -> TyVar
mkSysTyVar uniq kind = Var { varName    = name
			   , realUnique = getKey uniq
			   , varType    = kind
			   , varDetails = TyVar
#ifdef DEBUG
			   , varInfo = pprPanic "mkSysTyVar" (ppr name)
#endif
			   }
		     where
		       name = mkSysLocalName uniq SLIT("t")

newMutTyVar :: Name -> Kind -> IO TyVar
newMutTyVar name kind = 
  do loc <- newIORef Nothing
     return (Var { varName = name, 
		   realUnique = getKey (nameUnique name),
		   varType = kind, 
		   varDetails = MutTyVar loc False})

newSigTyVar :: Name -> Kind -> IO TyVar
newSigTyVar name kind = 
  do loc <- newIORef Nothing
     return (Var { varName = name, 
		   realUnique = getKey (nameUnique name),
		   varType = kind, 
		   varDetails = MutTyVar loc True})

readMutTyVar :: TyVar -> IO (Maybe Type)
readMutTyVar (Var {varDetails = MutTyVar loc _}) = readIORef loc

writeMutTyVar :: TyVar -> Maybe Type -> IO ()
writeMutTyVar (Var {varDetails = MutTyVar loc _}) val = writeIORef loc val

makeTyVarImmutable :: TyVar -> TyVar
makeTyVarImmutable tyvar = tyvar { varDetails = TyVar}

isTyVar :: Var -> Bool
isTyVar (Var {varDetails = details}) = case details of
					TyVar        -> True
					MutTyVar _ _ -> True
					other	     -> False

isMutTyVar :: Var -> Bool
isMutTyVar (Var {varDetails = MutTyVar _ _}) = True
isMutTyVar other			     = False

isSigTyVar :: Var -> Bool
isSigTyVar (Var {varDetails = MutTyVar _ is_sig}) = is_sig
isSigTyVar other			          = False
\end{code}


%************************************************************************
%*									*
\subsection{Usage variables}
%*									*
%************************************************************************

\begin{code}
type UVar = Var
\end{code}

\begin{code}
mkUVar :: Unique -> UVar
mkUVar unique = Var { varName    = mkSysLocalName unique SLIT("u"),
		      realUnique = getKey unique,
		      varDetails = UVar }

mkNamedUVar :: Name -> UVar
mkNamedUVar name = Var { varName    = name
		       , realUnique = getKey (nameUnique name)
		       , varDetails = UVar
#ifdef DEBUG
		       , varType = pprPanic "looking at Type of a uvar" (ppr name)
		       , varInfo = pprPanic "looking at IdInfo of a uvar" (ppr name)
#endif
		       }
\end{code}

\begin{code}
isUVar :: Var -> Bool
isUVar (Var {varDetails = details}) = case details of
					UVar	   -> True
					other	   -> False
\end{code}


%************************************************************************
%*									*
\subsection{Id Construction}
%*									*
%************************************************************************

Most Id-related functions are in Id.lhs and MkId.lhs

\begin{code}
type Id     = Var
type DictId = Id
\end{code}

\begin{code}
idName    = varName
idType    = varType
idUnique  = varUnique
idInfo	  = varInfo

setIdUnique :: Id -> Unique -> Id
setIdUnique = setVarUnique

setIdName :: Id -> Name -> Id
setIdName = setVarName

lazySetIdInfo :: Id -> IdInfo -> Id
lazySetIdInfo var info = var {varInfo = info}

setIdInfo :: Id -> IdInfo -> Id
setIdInfo var info = seqIdInfo info `seq` var {varInfo = info}
	-- Try to avoid spack leaks by seq'ing

zapIdInfo :: Id -> Id
zapIdInfo var = var {varInfo = vanillaIdInfo}

modifyIdInfo :: (IdInfo -> IdInfo) -> Id -> Id
modifyIdInfo fn var@(Var {varInfo = info})
  = seqIdInfo new_info `seq` var {varInfo = new_info}
  where
    new_info = fn info

-- maybeModifyIdInfo tries to avoid unnecesary thrashing
maybeModifyIdInfo :: (IdInfo -> Maybe IdInfo) -> Id -> Id
maybeModifyIdInfo fn var@(Var {varInfo = info}) = case fn info of
						Nothing       -> var
						Just new_info -> var {varInfo = new_info}
\end{code}

\begin{code}
mkIdVar :: Name -> Type -> IdInfo -> Id
mkIdVar name ty info
  = Var {varName = name, realUnique = getKey (nameUnique name), varType = ty, 
	 varDetails = AnId, varInfo = info}
\end{code}

\begin{code}
isId :: Var -> Bool
isId (Var {varDetails = AnId}) = True
isId other		       = False
\end{code}

@externallyVisibleId@: is it true that another module might be
able to ``see'' this Id in a code generation sense. That
is, another .o file might refer to this Id.

In tidyCorePgm (SimplCore.lhs) we carefully set each top level thing's
local-ness precisely so that the test here would be easy

This defn appears here (rather than, say, in Id.lhs) because
CostCentre.lhs uses it (CostCentre feeds PprType feeds Id.lhs)

\end{code}
\begin{code}
externallyVisibleId :: Id -> Bool
externallyVisibleId var = isExternallyVisibleName (varName var)
\end{code}
