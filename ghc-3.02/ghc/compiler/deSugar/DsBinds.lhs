%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[DsBinds]{Pattern-matching bindings (HsBinds and MonoBinds)}

Handles @HsBinds@; those at the top level require different handling,
in that the @Rec@/@NonRec@/etc structure is thrown away (whereas at
lower levels it is preserved with @let@/@letrec@s).

\begin{code}
module DsBinds ( dsBinds, dsMonoBinds ) where

#include "HsVersions.h"

import {-# SOURCE #-} DsExpr

import HsSyn		-- lots of things
import CoreSyn		-- lots of things
import CoreUtils	( coreExprType )
import TcHsSyn		( TypecheckedHsBinds, TypecheckedHsExpr,
			  TypecheckedMonoBinds,
			  TypecheckedPat
			)
import DsMonad
import DsGRHSs		( dsGuarded )
import DsUtils
import Match		( matchWrapper )

import BasicTypes       ( Module, RecFlag(..) )
import CmdLineOpts	( opt_SccProfilingOn, opt_AutoSccsOnAllToplevs, 
			  opt_AutoSccsOnExportedToplevs
		        )
import CostCentre	( mkAutoCC, IsCafCC(..), mkAllDictsCC )
import Id		( idType, Id )
import Name		( isExported )
import Type		( mkTyVarTy, isDictTy, instantiateTy
			)
import TyVar		( zipTyVarEnv )
import TysPrim		( voidTy )
import Outputable	( assertPanic )
\end{code}

%************************************************************************
%*									*
\subsection[toplevel-and-regular-DsBinds]{Regular and top-level @dsBinds@}
%*									*
%************************************************************************

Like @dsBinds@, @dsBind@ returns a @[CoreBinding]@, but it may be
that some of the binders are of unboxed type.  This is sorted out when
the caller wraps the bindings round an expression.

\begin{code}

dsBinds :: Bool   -- if candidate, auto add scc's on toplevs ?
	-> TypecheckedHsBinds 
	-> DsM [CoreBinding]

dsBinds _ EmptyBinds	  	     = returnDs []
dsBinds auto_scc (ThenBinds binds_1 binds_2) 
  = andDs (++) (dsBinds auto_scc binds_1) (dsBinds auto_scc binds_2)

dsBinds auto_scc (MonoBind binds sigs is_rec)
  = dsMonoBinds auto_scc binds []  `thenDs` \ prs ->
    returnDs (case is_rec of
		Recursive    -> [Rec prs]
	        NonRecursive -> [NonRec binder rhs | (binder,rhs) <- prs]
    )
\end{code}


%************************************************************************
%*									*
\subsection[dsMonoBinds]{Desugaring a @MonoBinds@}
%*									*
%************************************************************************

\begin{code}
dsMonoBinds :: Bool		-- False => don't (auto-)annotate scc on toplevs.
	    -> TypecheckedMonoBinds
	    -> [(Id,CoreExpr)]		-- Put this on the end (avoid quadratic append)
	    -> DsM [(Id,CoreExpr)]	-- Result

dsMonoBinds _ EmptyMonoBinds rest = returnDs rest

dsMonoBinds auto_scc (AndMonoBinds  binds_1 binds_2) rest
  = dsMonoBinds auto_scc binds_2 rest	`thenDs` \ rest' ->
    dsMonoBinds auto_scc binds_1 rest'

dsMonoBinds _ (CoreMonoBind var core_expr) rest
  = returnDs ((var, core_expr) : rest)

dsMonoBinds _ (VarMonoBind var expr) rest
  = dsExpr expr			`thenDs` \ core_expr ->

	-- Dictionary bindings are always VarMonoBinds, so
	-- we only need do this here
    addDictScc var core_expr	`thenDs` \ core_expr' ->

    returnDs ((var, core_expr') : rest)

dsMonoBinds auto_scc (FunMonoBind fun _ matches locn) rest
  = putSrcLocDs locn	$
    matchWrapper (FunMatch fun) matches error_string	`thenDs` \ (args, body) ->
    addAutoScc auto_scc (fun, mkValLam args body)       `thenDs` \ pair ->
    returnDs (pair : rest)
  where
    error_string = "function " ++ showForErr fun

dsMonoBinds _ (PatMonoBind pat grhss_and_binds locn) rest
  = putSrcLocDs locn $
    dsGuarded grhss_and_binds		`thenDs` \ body_expr ->
    mkSelectorBinds pat body_expr	`thenDs` \ sel_binds ->
    returnDs (sel_binds ++ rest)

	-- Common special case: no type or dictionary abstraction
dsMonoBinds auto_scc (AbsBinds [] [] exports binds) rest
  = mapDs (addAutoScc auto_scc) [(global, Var local) | (_, global, local) <- exports] `thenDs` \ exports' ->
    dsMonoBinds False binds (exports' ++ rest)

	-- Another common case: one exported variable
	-- All non-recursive bindings come through this way
dsMonoBinds auto_scc (AbsBinds all_tyvars dicts [(tyvars, global, local)] binds) rest
  = ASSERT( all (`elem` tyvars) all_tyvars )
    dsMonoBinds False binds []			`thenDs` \ core_prs ->
    let 
	-- Always treat the binds as recursive, because the typechecker
	-- makes rather mixed-up dictionary bindings
	core_binds = [Rec core_prs]
    in
    addAutoScc auto_scc (global, mkLam tyvars dicts $ 
			         mkCoLetsAny core_binds (Var local)) `thenDs` \ global' ->
    returnDs (global' : rest)

dsMonoBinds auto_scc (AbsBinds all_tyvars dicts exports binds) rest
  = dsMonoBinds False binds []			`thenDs` \ core_prs ->
    let 
	core_binds = [Rec core_prs]

	tup_expr = mkLam all_tyvars dicts $
		   mkCoLetsAny core_binds $
		   mkTupleExpr locals
	locals    = [local | (_, _, local) <- exports]
	local_tys = map idType locals
    in
    newSysLocalDs (coreExprType tup_expr)		`thenDs` \ tup_id ->
    let
	dict_args    = map VarArg dicts

	mk_bind (tyvars, global, local) n	-- locals !! n == local
	  = 	-- Need to make fresh locals to bind in the selector, because
		-- some of the tyvars will be bound to voidTy
	    newSysLocalsDs (map (instantiateTy env) local_tys) 	`thenDs` \ locals' ->
	    addAutoScc auto_scc
		       (global, mkLam tyvars dicts $
		     	        mkTupleSelector locals' (locals' !! n) $
		     	        mkValApp (mkTyApp (Var tup_id) ty_args) dict_args)
	  where
	    mk_ty_arg all_tyvar | all_tyvar `elem` tyvars = mkTyVarTy all_tyvar
				| otherwise		  = voidTy
	    ty_args = map mk_ty_arg all_tyvars
	    env     = all_tyvars `zipTyVarEnv` ty_args
    in
    zipWithDs mk_bind exports [0..]		`thenDs` \ export_binds ->
     -- don't scc (auto-)annotate the tuple itself.
    returnDs ((tup_id, tup_expr) : (export_binds ++ rest))
\end{code}


%************************************************************************
%*									*
\subsection[addAutoScc]{Adding automatic sccs}
%*									*
%************************************************************************

\begin{code}
addAutoScc :: Bool		-- if needs be, decorate toplevs?
	   -> (Id, CoreExpr)
	   -> DsM (Id, CoreExpr)

addAutoScc auto_scc_candidate pair@(bndr, core_expr) 
 | auto_scc_candidate && worthSCC core_expr && 
   (opt_AutoSccsOnAllToplevs || (isExported bndr && opt_AutoSccsOnExportedToplevs))
     = getModuleAndGroupDs `thenDs` \ (mod,grp) ->
       returnDs (bndr, Note (SCC (mkAutoCC bndr mod grp IsNotCafCC)) core_expr)
 | otherwise 
     = returnDs pair

worthSCC (Note (SCC _) _) = False
worthSCC (Con _ _)        = False
worthSCC core_expr        = True
\end{code}

If profiling and dealing with a dict binding, wrap the dict in "_scc_ DICT <dict>":

\begin{code}
addDictScc var rhs
  | not ( opt_SccProfilingOn || opt_AutoSccsOnAllToplevs)
	    -- the latter is so that -unprof-auto-scc-all adds dict sccs
    || not (isDictTy (idType var))
  = returnDs rhs				-- That's easy: do nothing

  | otherwise
  = getModuleAndGroupDs 	`thenDs` \ (mod, grp) ->

	-- ToDo: do -dicts-all flag (mark dict things with individual CCs)
    returnDs (Note (SCC (mkAllDictsCC mod grp False)) rhs)
\end{code}
