%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Id]{@Ids@: Value and constructor identifiers}

\begin{code}
module Id (
	Id, DictId,

	-- Simple construction
	mkId, mkVanillaId, mkSysLocal, mkUserLocal,
	mkTemplateLocals, mkTemplateLocalsNum, mkWildId, mkTemplateLocal,

	-- Taking an Id apart
	idName, idType, idUnique, idInfo,
	idPrimRep, isId,
	recordSelectorFieldLabel,

	-- Modifying an Id
	setIdName, setIdUnique, setIdType, setIdNoDiscard, 
	setIdInfo, lazySetIdInfo, modifyIdInfo, maybeModifyIdInfo,
	zapFragileIdInfo, zapLamIdInfo,

	-- Predicates
	omitIfaceSigForId, isDeadBinder,
	exportWithOrigOccName,
	externallyVisibleId,
	idFreeTyVars,
	isIP,

	-- Inline pragma stuff
	idInlinePragma, setInlinePragma, modifyInlinePragma, 

	isSpecPragmaId,	isRecordSelector,
	isPrimOpId, isPrimOpId_maybe, 
	isDataConId, isDataConId_maybe, isDataConWrapId, isDataConWrapId_maybe,
	isBottomingId,
	isExportedId, isUserExportedId,
	mayHaveNoBinding,

	-- One shot lambda stuff
	isOneShotLambda, setOneShotLambda, clearOneShotLambda,

	-- IdInfo stuff
	setIdUnfolding,
	setIdArityInfo,
	setIdDemandInfo,
	setIdStrictness,
	setIdWorkerInfo,
	setIdSpecialisation,
	setIdUpdateInfo,
	setIdCafInfo,
	setIdCprInfo,
	setIdOccInfo,

	idArity, idArityInfo, 
	idFlavour,
	idDemandInfo,
	idStrictness,
	idWorkerInfo,
	idUnfolding,
	idSpecialisation,
	idUpdateInfo,
	idCafInfo,
	idCprInfo,
	idLBVarInfo,
	idOccInfo

    ) where

#include "HsVersions.h"


import CoreSyn		( Unfolding, CoreRules )
import BasicTypes	( Arity )
import Var		( Id, DictId,
			  isId, mkIdVar,
			  idName, idType, idUnique, idInfo,
			  setIdName, setVarType, setIdUnique, 
			  setIdInfo, lazySetIdInfo, modifyIdInfo, maybeModifyIdInfo,
			  externallyVisibleId
			)
import VarSet
import Type		( Type, tyVarsOfType, typePrimRep, addFreeTyVars, seqType, splitTyConApp_maybe )

import IdInfo 

import Demand		( Demand, isStrict, wwLazy )
import Name	 	( Name, OccName,
			  mkSysLocalName, mkLocalName,
			  isWiredInName, isUserExportedName,
			  getOccName, isIPOcc
			) 
import OccName		( UserFS )
import PrimRep		( PrimRep )
import PrimOp		( PrimOp, primOpIsCheap )
import TysPrim		( statePrimTyCon )
import FieldLabel	( FieldLabel )
import SrcLoc		( SrcLoc )
import Unique		( Unique, mkBuiltinUnique, getBuiltinUniques,
			  getNumBuiltinUniques )
import Outputable

infixl 	1 `setIdUnfolding`,
	  `setIdArityInfo`,
	  `setIdDemandInfo`,
	  `setIdStrictness`,
	  `setIdWorkerInfo`,
	  `setIdSpecialisation`,
	  `setIdUpdateInfo`,
	  `setInlinePragma`,
	  `idCafInfo`,
	  `idCprInfo`

	-- infixl so you can say (id `set` a `set` b)
\end{code}



%************************************************************************
%*									*
\subsection{Simple Id construction}
%*									*
%************************************************************************

Absolutely all Ids are made by mkId.  It 
	a) Pins free-tyvar-info onto the Id's type, 
	   where it can easily be found.
	b) Ensures that exported Ids are 

\begin{code}
mkId :: Name -> Type -> IdInfo -> Id
mkId name ty info = mkIdVar name (addFreeTyVars ty) info'
		  where
		    info' | isUserExportedName name = setNoDiscardInfo info
			  | otherwise		    = info
\end{code}

\begin{code}
mkVanillaId :: Name -> Type -> Id
mkVanillaId name ty = mkId name ty vanillaIdInfo

-- SysLocal: for an Id being created by the compiler out of thin air...
-- UserLocal: an Id with a name the user might recognize...
mkUserLocal :: OccName -> Unique -> Type -> SrcLoc -> Id
mkSysLocal  :: UserFS  -> Unique -> Type -> Id

mkSysLocal  fs uniq ty      = mkVanillaId (mkSysLocalName uniq fs)      ty
mkUserLocal occ uniq ty loc = mkVanillaId (mkLocalName    uniq occ loc) ty
\end{code}

Make some local @Ids@ for a template @CoreExpr@.  These have bogus
@Uniques@, but that's OK because the templates are supposed to be
instantiated before use.

\begin{code}
-- "Wild Id" typically used when you need a binder that you don't expect to use
mkWildId :: Type -> Id
mkWildId ty = mkSysLocal SLIT("wild") (mkBuiltinUnique 1) ty

-- "Template locals" typically used in unfoldings
mkTemplateLocals :: [Type] -> [Id]
mkTemplateLocals tys = zipWith (mkSysLocal SLIT("tpl"))
			       (getBuiltinUniques (length tys))
			       tys

mkTemplateLocalsNum :: Int -> [Type] -> [Id]
-- The Int gives the starting point for unique allocation
mkTemplateLocalsNum n tys = zipWith (mkSysLocal SLIT("tpl"))
			       (getNumBuiltinUniques n (length tys))
			       tys

mkTemplateLocal :: Int -> Type -> Id
mkTemplateLocal i ty = mkSysLocal SLIT("tpl") (mkBuiltinUnique i) ty
\end{code}


%************************************************************************
%*									*
\subsection[Id-general-funs]{General @Id@-related functions}
%*									*
%************************************************************************

\begin{code}
idFreeTyVars :: Id -> TyVarSet
idFreeTyVars id = tyVarsOfType (idType id)

setIdType :: Id -> Type -> Id
	-- Add free tyvar info to the type
setIdType id ty = seqType ty `seq` setVarType id (addFreeTyVars ty)

idPrimRep :: Id -> PrimRep
idPrimRep id = typePrimRep (idType id)
\end{code}


%************************************************************************
%*									*
\subsection{Special Ids}
%*									*
%************************************************************************

\begin{code}
idFlavour :: Id -> IdFlavour
idFlavour id = flavourInfo (idInfo id)

setIdNoDiscard :: Id -> Id
setIdNoDiscard id	-- Make an Id into a NoDiscardId, unless it is already
  = modifyIdInfo setNoDiscardInfo id

recordSelectorFieldLabel :: Id -> FieldLabel
recordSelectorFieldLabel id = case idFlavour id of
				RecordSelId lbl -> lbl

isRecordSelector id = case idFlavour id of
			RecordSelId lbl -> True
			other	  	-> False

isPrimOpId id = case idFlavour id of
		    PrimOpId op -> True
		    other	-> False

isPrimOpId_maybe id = case idFlavour id of
			    PrimOpId op -> Just op
			    other	-> Nothing

isDataConId id = case idFlavour id of
			DataConId _ -> True
			other	    -> False

isDataConId_maybe id = case idFlavour id of
			  DataConId con -> Just con
			  other	        -> Nothing

isDataConWrapId_maybe id = case idFlavour id of
				  DataConWrapId con -> Just con
				  other	            -> Nothing

isDataConWrapId id = case idFlavour id of
			DataConWrapId con -> True
			other	          -> False

isSpecPragmaId id = case idFlavour id of
			SpecPragmaId -> True
			other	     -> False

mayHaveNoBinding id = case idFlavour id of
			DataConId _ -> True
			PrimOpId _  -> True
			other	    -> False
	-- mayHaveNoBinding returns True of an Id which may not have a
	-- binding, even though it is defined in this module.  Notably,
	-- the constructors of a dictionary are in this situation.
	--	
	-- mayHaveNoBinding returns True of some things that *do* have a local binding,
	-- so it's only an approximation.  That's ok... it's only use for assertions.

-- Don't drop a binding for an exported Id,
-- if it otherwise looks dead.  
isExportedId :: Id -> Bool
isExportedId id = case idFlavour id of
			VanillaId -> False
			other	  -> True	-- All the others are no-discard

-- Say if an Id was exported by the user
-- Implies isExportedId (see mkId above)
isUserExportedId :: Id -> Bool
isUserExportedId id = isUserExportedName (idName id)
\end{code}


omitIfaceSigForId tells whether an Id's info is implied by other declarations,
so we don't need to put its signature in an interface file, even if it's mentioned
in some other interface unfolding.

\begin{code}
omitIfaceSigForId :: Id -> Bool
omitIfaceSigForId id
  | isWiredInName (idName id)
  = True

  | otherwise
  = case idFlavour id of
	RecordSelId _   -> True	-- Includes dictionary selectors
        PrimOpId _      -> True
        DataConId _     -> True
	DataConWrapId _ -> True
		-- These are are implied by their type or class decl;
		-- remember that all type and class decls appear in the interface file.
		-- The dfun id must *not* be omitted, because it carries version info for
		-- the instance decl

	other	       -> False	-- Don't omit!

-- Certain names must be exported with their original occ names, because
-- these names are bound by either a class declaration or a data declaration
-- or an explicit user export.
exportWithOrigOccName :: Id -> Bool
exportWithOrigOccName id = omitIfaceSigForId id || isUserExportedId id
\end{code}

\begin{code}
isDeadBinder :: Id -> Bool
isDeadBinder bndr | isId bndr = case idOccInfo bndr of
					IAmDead -> True
					other	-> False
		  | otherwise = False	-- TyVars count as not dead

isIP id = isIPOcc (getOccName id)
\end{code}


%************************************************************************
%*									*
\subsection{IdInfo stuff}
%*									*
%************************************************************************

\begin{code}
	---------------------------------
	-- ARITY
idArityInfo :: Id -> ArityInfo
idArityInfo id = arityInfo (idInfo id)

idArity :: Id -> Arity
idArity id = arityLowerBound (idArityInfo id)

setIdArityInfo :: Id -> ArityInfo -> Id
setIdArityInfo id arity = modifyIdInfo (`setArityInfo` arity) id

	---------------------------------
	-- STRICTNESS
idStrictness :: Id -> StrictnessInfo
idStrictness id = strictnessInfo (idInfo id)

setIdStrictness :: Id -> StrictnessInfo -> Id
setIdStrictness id strict_info = modifyIdInfo (`setStrictnessInfo` strict_info) id

-- isBottomingId returns true if an application to n args would diverge
isBottomingId :: Id -> Bool
isBottomingId id = isBottomingStrictness (idStrictness id)

	---------------------------------
	-- WORKER ID
idWorkerInfo :: Id -> WorkerInfo
idWorkerInfo id = workerInfo (idInfo id)

setIdWorkerInfo :: Id -> WorkerInfo -> Id
setIdWorkerInfo id work_info = modifyIdInfo (`setWorkerInfo` work_info) id

	---------------------------------
	-- UNFOLDING
idUnfolding :: Id -> Unfolding
idUnfolding id = unfoldingInfo (idInfo id)

setIdUnfolding :: Id -> Unfolding -> Id
setIdUnfolding id unfolding = modifyIdInfo (`setUnfoldingInfo` unfolding) id

	---------------------------------
	-- DEMAND
idDemandInfo :: Id -> Demand
idDemandInfo id = demandInfo (idInfo id)

setIdDemandInfo :: Id -> Demand -> Id
setIdDemandInfo id demand_info = modifyIdInfo (`setDemandInfo` demand_info) id

	---------------------------------
	-- UPDATE INFO
idUpdateInfo :: Id -> UpdateInfo
idUpdateInfo id = updateInfo (idInfo id)

setIdUpdateInfo :: Id -> UpdateInfo -> Id
setIdUpdateInfo id upd_info = modifyIdInfo (`setUpdateInfo` upd_info) id

	---------------------------------
	-- SPECIALISATION
idSpecialisation :: Id -> CoreRules
idSpecialisation id = specInfo (idInfo id)

setIdSpecialisation :: Id -> CoreRules -> Id
setIdSpecialisation id spec_info = modifyIdInfo (`setSpecInfo` spec_info) id

	---------------------------------
	-- CAF INFO
idCafInfo :: Id -> CafInfo
idCafInfo id = cafInfo (idInfo id)

setIdCafInfo :: Id -> CafInfo -> Id
setIdCafInfo id caf_info = modifyIdInfo (`setCafInfo` caf_info) id

	---------------------------------
	-- CPR INFO
idCprInfo :: Id -> CprInfo
idCprInfo id = cprInfo (idInfo id)

setIdCprInfo :: Id -> CprInfo -> Id
setIdCprInfo id cpr_info = modifyIdInfo (`setCprInfo` cpr_info) id

	---------------------------------
	-- Occcurrence INFO
idOccInfo :: Id -> OccInfo
idOccInfo id = occInfo (idInfo id)

setIdOccInfo :: Id -> OccInfo -> Id
setIdOccInfo id occ_info = modifyIdInfo (`setOccInfo` occ_info) id
\end{code}


	---------------------------------
	-- INLINING
The inline pragma tells us to be very keen to inline this Id, but it's still
OK not to if optimisation is switched off.

\begin{code}
idInlinePragma :: Id -> InlinePragInfo
idInlinePragma id = inlinePragInfo (idInfo id)

setInlinePragma :: Id -> InlinePragInfo -> Id
setInlinePragma id prag = modifyIdInfo (`setInlinePragInfo` prag) id

modifyInlinePragma :: Id -> (InlinePragInfo -> InlinePragInfo) -> Id
modifyInlinePragma id fn = modifyIdInfo (\info -> info `setInlinePragInfo` (fn (inlinePragInfo info))) id
\end{code}


	---------------------------------
	-- ONE-SHOT LAMBDAS
\begin{code}
idLBVarInfo :: Id -> LBVarInfo
idLBVarInfo id = lbvarInfo (idInfo id)

isOneShotLambda :: Id -> Bool
isOneShotLambda id = case idLBVarInfo id of
			IsOneShotLambda -> True
			NoLBVarInfo	-> case splitTyConApp_maybe (idType id) of
						Just (tycon,_) -> tycon == statePrimTyCon
						other	       -> False
	-- The last clause is a gross hack.  It claims that 
	-- every function over realWorldStatePrimTy is a one-shot
	-- function.  This is pretty true in practice, and makes a big
	-- difference.  For example, consider
	--	a `thenST` \ r -> ...E...
	-- The early full laziness pass, if it doesn't know that r is one-shot
	-- will pull out E (let's say it doesn't mention r) to give
	--	let lvl = E in a `thenST` \ r -> ...lvl...
	-- When `thenST` gets inlined, we end up with
	--	let lvl = E in \s -> case a s of (r, s') -> ...lvl...
	-- and we don't re-inline E.
	--
	-- It would be better to spot that r was one-shot to start with, but
	-- I don't want to rely on that.
	--
	-- Another good example is in fill_in in PrelPack.lhs.  We should be able to
	-- spot that fill_in has arity 2 (and when Keith is done, we will) but we can't yet.

setOneShotLambda :: Id -> Id
setOneShotLambda id = modifyIdInfo (`setLBVarInfo` IsOneShotLambda) id

clearOneShotLambda :: Id -> Id
clearOneShotLambda id 
  | isOneShotLambda id = modifyIdInfo (`setLBVarInfo` NoLBVarInfo) id
  | otherwise	       = id			

-- But watch out: this may change the type of something else
--	f = \x -> e
-- If we change the one-shot-ness of x, f's type changes
\end{code}

\begin{code}
zapFragileIdInfo :: Id -> Id
zapFragileIdInfo id = maybeModifyIdInfo zapFragileInfo id

zapLamIdInfo :: Id -> Id
zapLamIdInfo id = maybeModifyIdInfo zapLamInfo id
\end{code}

