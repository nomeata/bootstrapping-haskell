%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[DsUtils]{Utilities for desugaring}

This module exports some utility functions of no great interest.

\begin{code}
module DsUtils (
	CanItFail(..), EquationInfo(..), MatchResult(..),
        EqnNo, EqnSet,

	combineGRHSMatchResults,
	combineMatchResults,
	dsExprToAtomGivenTy, DsCoreArg,
	mkCoAlgCaseMatchResult,
	mkAppDs, mkConDs, mkPrimDs, mkErrorAppDs,
	mkCoLetsMatchResult,
	mkCoPrimCaseMatchResult,
	mkFailurePair,
	mkGuardedMatchResult,
	mkSelectorBinds,
	mkTupleExpr,
	mkTupleSelector,
	selectMatchVars,
	showForErr
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Match ( matchSimply )

import HsSyn		( OutPat(..), Stmt, DoOrListComp )
import TcHsSyn		( TypecheckedPat )
import DsHsSyn		( outPatType, collectTypedPatBinders )
import CoreSyn

import DsMonad

import CoreUtils	( coreExprType, mkCoreIfThenElse )
import PrelVals		( iRREFUT_PAT_ERROR_ID, voidId )
import Id		( idType, dataConArgTys, 
			  DataCon, Id, GenId )
import Literal		( Literal(..) )
import PrimOp           ( PrimOp )
import TyCon		( isNewTyCon, tyConDataCons )
import Type		( mkRhoTy, mkFunTy,
			  isUnpointedType, mkTyConApp, splitAlgTyConApp,
			  Type
			)
import BasicTypes	( Unused )
import TysPrim		( voidTy )
import TysWiredIn	( unitDataCon, tupleCon, stringTy )
import UniqSet		( mkUniqSet, minusUniqSet, uniqSetToList, UniqSet )
import Unique		( Unique )
import Outputable
\end{code}


%************************************************************************
%*									*
%* Selecting match variables
%*									*
%************************************************************************

We're about to match against some patterns.  We want to make some
@Ids@ to use as match variables.  If a pattern has an @Id@ readily at
hand, which should indeed be bound to the pattern as a whole, then use it;
otherwise, make one up.

\begin{code}
selectMatchVars :: [TypecheckedPat] -> DsM [Id]
selectMatchVars pats
  = mapDs var_from_pat_maybe pats
  where
    var_from_pat_maybe (VarPat var)	= returnDs var
    var_from_pat_maybe (AsPat var pat)	= returnDs var
    var_from_pat_maybe (LazyPat pat)	= var_from_pat_maybe pat
    var_from_pat_maybe other_pat
      = newSysLocalDs (outPatType other_pat) -- OK, better make up one...
\end{code}


%************************************************************************
%*									*
%* type synonym EquationInfo and access functions for its pieces	*
%*									*
%************************************************************************
\subsection[EquationInfo-synonym]{@EquationInfo@: a useful synonym}

The ``equation info'' used by @match@ is relatively complicated and
worthy of a type synonym and a few handy functions.

\begin{code}

type EqnNo   = Int
type EqnSet  = UniqSet EqnNo

data EquationInfo
  = EqnInfo
	EqnNo               -- The number of the equation
	DsMatchContext	-- The context info is used when producing warnings
			-- about shadowed patterns.  It's the context
			-- of the *first* thing matched in this group.
			-- Should perhaps be a list of them all!
	[TypecheckedPat]    -- the patterns for an eqn
      	MatchResult	    -- Encapsulates the guards and bindings
\end{code}

\begin{code}
data MatchResult
  = MatchResult
	CanItFail
	Type		-- Type of argument expression

	(CoreExpr -> CoreExpr)
			-- Takes a expression to plug in at the
			-- failure point(s). The expression should
			-- be duplicatable!

data CanItFail = CanFail | CantFail

orFail CantFail CantFail = CantFail
orFail _        _	 = CanFail


mkCoLetsMatchResult :: [CoreBinding] -> MatchResult -> MatchResult
mkCoLetsMatchResult binds (MatchResult can_it_fail ty body_fn)
  = MatchResult can_it_fail ty (\body -> mkCoLetsAny binds (body_fn body))

mkGuardedMatchResult :: CoreExpr -> MatchResult -> DsM MatchResult
mkGuardedMatchResult pred_expr (MatchResult can_it_fail ty body_fn)
  = returnDs (MatchResult CanFail
			  ty
			  (\fail -> mkCoreIfThenElse pred_expr (body_fn fail) fail)
    )

mkCoPrimCaseMatchResult :: Id				-- Scrutinee
		    -> [(Literal, MatchResult)]	-- Alternatives
		    -> DsM MatchResult
mkCoPrimCaseMatchResult var alts
  = newSysLocalDs (idType var)	`thenDs` \ wild ->
    returnDs (MatchResult CanFail
			  ty1
			  (mk_case alts wild))
  where
    ((_,MatchResult _ ty1 _) : _) = alts

    mk_case alts wild fail_expr
      = Case (Var var) (PrimAlts final_alts (BindDefault wild fail_expr))
      where
	final_alts = [ (lit, body_fn fail_expr)
		     | (lit, MatchResult _ _ body_fn) <- alts
		     ]


mkCoAlgCaseMatchResult :: Id				-- Scrutinee
		    -> [(DataCon, [Id], MatchResult)]	-- Alternatives
		    -> DsM MatchResult

mkCoAlgCaseMatchResult var alts
  | isNewTyCon tycon		-- newtype case; use a let
  = ASSERT( newtype_sanity )
    returnDs (mkCoLetsMatchResult [coercion_bind] match_result)

  | otherwise			-- datatype case  
  =	    -- Find all the constructors in the type which aren't
	    -- explicitly mentioned in the alternatives:
    case un_mentioned_constructors of
	[] ->	-- All constructors mentioned, so no default needed
		returnDs (MatchResult can_any_alt_fail
			  	      ty1
				      (mk_case alts (\ignore -> NoDefault)))

	[con] ->     -- Just one constructor missing, so add a case for it
		     -- We need to build new locals for the args of the constructor,
		     -- and figuring out their types is somewhat tiresome.
		let
			arg_tys = dataConArgTys con tycon_arg_tys
		in
		newSysLocalsDs arg_tys	`thenDs` \ arg_ids ->

		     -- Now we are ready to construct the new alternative
		let
			new_alt = (con, arg_ids, MatchResult CanFail ty1 id)
		in
		returnDs (MatchResult CanFail
			  	      ty1
				      (mk_case (new_alt:alts) (\ignore -> NoDefault)))

	other ->      -- Many constructors missing, so use a default case
		newSysLocalDs scrut_ty		`thenDs` \ wild ->
		returnDs (MatchResult CanFail
			  	      ty1
				      (mk_case alts (\fail_expr -> BindDefault wild fail_expr)))
  where
	-- Common stuff
    scrut_ty = idType var
    (tycon, tycon_arg_tys, _) = splitAlgTyConApp scrut_ty

	-- Stuff for newtype
    (con_id, arg_ids, match_result) = head alts
    arg_id 	   		    = head arg_ids
    coercion_bind		    = NonRec arg_id (Note (Coerce (idType arg_id) scrut_ty) (Var var))
    newtype_sanity		    = null (tail alts) && null (tail arg_ids)

	-- Stuff for data types
    data_cons = tyConDataCons tycon

    un_mentioned_constructors
      = uniqSetToList (mkUniqSet data_cons `minusUniqSet` mkUniqSet [ con | (con, _, _) <- alts] )

    match_results = [match_result | (_,_,match_result) <- alts]
    (MatchResult _ ty1 _ : _) = match_results
    can_any_alt_fail = foldr1 orFail [can_it_fail | MatchResult can_it_fail _ _ <- match_results]

    mk_case alts deflt_fn fail_expr
      = Case (Var var) (AlgAlts final_alts (deflt_fn fail_expr))
      where
	final_alts = [ (con, args, body_fn fail_expr)
		     | (con, args, MatchResult _ _ body_fn) <- alts
		     ]


combineMatchResults :: MatchResult -> MatchResult -> DsM MatchResult
combineMatchResults (MatchResult CanFail      ty1 body_fn1)
		    (MatchResult can_it_fail2 ty2 body_fn2)
  = mkFailurePair ty1		`thenDs` \ (bind_fn, duplicatable_expr) ->
    let
	new_body_fn1 = \body1 -> Let (bind_fn body1) (body_fn1 duplicatable_expr)
	new_body_fn2 = \body2 -> new_body_fn1 (body_fn2 body2)
    in
    returnDs (MatchResult can_it_fail2 ty1 new_body_fn2)

combineMatchResults match_result1@(MatchResult CantFail ty body_fn1)
				  match_result2
  = returnDs match_result1


-- The difference in combineGRHSMatchResults is that there is no
-- need to let-bind to avoid code duplication
combineGRHSMatchResults :: MatchResult -> MatchResult -> DsM MatchResult
combineGRHSMatchResults (MatchResult CanFail     ty1 body_fn1)
			(MatchResult can_it_fail ty2 body_fn2)
  = returnDs (MatchResult can_it_fail ty1 (\ body -> body_fn1 (body_fn2 body)))

combineGRHSMatchResults match_result1 match_result2
  = 	-- Delegate to avoid duplication of code
    combineMatchResults match_result1 match_result2
\end{code}

%************************************************************************
%*									*
\subsection[dsExprToAtom]{Take an expression and produce an atom}
%*									*
%************************************************************************

\begin{code}
dsArgToAtom :: DsCoreArg		    -- The argument expression
	     -> (CoreArg -> DsM CoreExpr)   -- Something taking the argument *atom*,
					    -- and delivering an expression E
	     -> DsM CoreExpr		    -- Either E or let x=arg-expr in E

dsArgToAtom (TyArg    t) continue_with = continue_with (TyArg    t)
dsArgToAtom (LitArg   l) continue_with = continue_with (LitArg   l)
dsArgToAtom (VarArg arg) continue_with = dsExprToAtomGivenTy arg (coreExprType arg) continue_with

dsExprToAtomGivenTy
	 :: CoreExpr		    	-- The argument expression
	 -> Type			-- Type of the argument
	 -> (CoreArg -> DsM CoreExpr)   -- Something taking the argument *atom*,
					-- and delivering an expression E
	 -> DsM CoreExpr		-- Either E or let x=arg-expr in E

dsExprToAtomGivenTy (Var v)  arg_ty continue_with = continue_with (VarArg v)
dsExprToAtomGivenTy (Lit v)  arg_ty continue_with = continue_with (LitArg v)
dsExprToAtomGivenTy arg_expr arg_ty continue_with
  = newSysLocalDs arg_ty		`thenDs` \ arg_id ->
    continue_with (VarArg arg_id)	`thenDs` \ body   ->
    returnDs (
	if isUnpointedType arg_ty
	then Case arg_expr (PrimAlts [] (BindDefault arg_id body))
	else Let (NonRec arg_id arg_expr) body
    )

dsArgsToAtoms :: [DsCoreArg]
	       -> ([CoreArg] -> DsM CoreExpr)
	       -> DsM CoreExpr

dsArgsToAtoms [] continue_with = continue_with []

dsArgsToAtoms (arg:args) continue_with
  = dsArgToAtom   arg 	$ \ arg_atom  ->
    dsArgsToAtoms args $ \ arg_atoms ->
    continue_with (arg_atom:arg_atoms)
\end{code}

%************************************************************************
%*									*
\subsection{Desugarer's versions of some Core functions}
%*									*
%************************************************************************

\begin{code}
type DsCoreArg = GenCoreArg CoreExpr{-NB!-} Unused

mkAppDs  :: CoreExpr -> [DsCoreArg] -> DsM CoreExpr
mkConDs  :: Id       -> [DsCoreArg] -> DsM CoreExpr
mkPrimDs :: PrimOp   -> [DsCoreArg] -> DsM CoreExpr

mkAppDs fun args
  = dsArgsToAtoms args $ \ atoms ->
    returnDs (mkGenApp fun atoms)

mkConDs con args
  = dsArgsToAtoms args $ \ atoms ->
    returnDs (Con con atoms)

mkPrimDs op args
  = dsArgsToAtoms args $ \ atoms ->
    returnDs (Prim op  atoms)
\end{code}

\begin{code}
showForErr :: Outputable a => a -> String		-- Boring but useful
showForErr thing = showSDoc (ppr thing)

mkErrorAppDs :: Id 		-- The error function
	     -> Type		-- Type to which it should be applied
	     -> String		-- The error message string to pass
	     -> DsM CoreExpr

mkErrorAppDs err_id ty msg
  = getSrcLocDs			`thenDs` \ src_loc ->
    let
	full_msg = showSDoc (hcat [ppr src_loc, text "|", text msg])
	msg_lit  = NoRepStr (_PK_ full_msg)
    in
    returnDs (mkApp (Var err_id) [ty] [LitArg msg_lit])
\end{code}

%************************************************************************
%*									*
\subsection[mkSelectorBind]{Make a selector bind}
%*									*
%************************************************************************

This is used in various places to do with lazy patterns.
For each binder $b$ in the pattern, we create a binding:

    b = case v of pat' -> b'

where pat' is pat with each binder b cloned into b'.

ToDo: making these bindings should really depend on whether there's
much work to be done per binding.  If the pattern is complex, it
should be de-mangled once, into a tuple (and then selected from).
Otherwise the demangling can be in-line in the bindings (as here).

Boring!  Boring!  One error message per binder.  The above ToDo is
even more helpful.  Something very similar happens for pattern-bound
expressions.

\begin{code}
mkSelectorBinds :: TypecheckedPat	-- The pattern
		-> CoreExpr    		-- Expression to which the pattern is bound
		-> DsM [(Id,CoreExpr)]

mkSelectorBinds (VarPat v) val_expr
  = returnDs [(v, val_expr)]

mkSelectorBinds pat val_expr
  | length binders == 1 || is_simple_pat pat
  = newSysLocalDs (coreExprType val_expr)	`thenDs` \ val_var ->

	-- For the error message we don't use mkErrorAppDs to avoid
	-- duplicating the string literal each time
    newSysLocalDs stringTy			`thenDs` \ msg_var ->
    getSrcLocDs					`thenDs` \ src_loc ->
    let
	full_msg = showSDoc (hcat [ppr src_loc, text "|", ppr pat])
	msg_lit  = NoRepStr (_PK_ full_msg)
    in
    mapDs (mk_bind val_var msg_var) binders	`thenDs` \ binds ->
    returnDs ( (val_var, val_expr) : 
	       (msg_var, Lit msg_lit) :
	       binds )


  | otherwise
  = mkErrorAppDs iRREFUT_PAT_ERROR_ID tuple_ty (showSDoc (ppr pat))	`thenDs` \ error_expr ->
    matchSimply val_expr LetMatch pat tuple_ty local_tuple error_expr	`thenDs` \ tuple_expr ->
    newSysLocalDs tuple_ty						`thenDs` \ tuple_var ->
    let
	mk_tup_bind binder = (binder, mkTupleSelector binders binder (Var tuple_var))
    in
    returnDs ( (tuple_var, tuple_expr) : map mk_tup_bind binders )
  where
    binders	= collectTypedPatBinders pat
    local_tuple = mkTupleExpr binders
    tuple_ty    = coreExprType local_tuple

    mk_bind scrut_var msg_var bndr_var
    -- (mk_bind sv bv) generates
    --		bv = case sv of { pat -> bv; other -> error-msg }
    -- Remember, pat binds bv
      = matchSimply (Var scrut_var) LetMatch pat binder_ty 
		    (Var bndr_var) error_expr			`thenDs` \ rhs_expr ->
        returnDs (bndr_var, rhs_expr)
      where
        binder_ty = idType bndr_var
        error_expr = mkApp (Var iRREFUT_PAT_ERROR_ID) [binder_ty] [VarArg msg_var]

    is_simple_pat (TuplePat ps)        = all is_triv_pat ps
    is_simple_pat (ConPat _ _ ps)      = all is_triv_pat ps
    is_simple_pat (VarPat _)	       = True
    is_simple_pat (ConOpPat p1 _ p2 _) = is_triv_pat p1 && is_triv_pat p2
    is_simple_pat (RecPat _ _ ps)      = and [is_triv_pat p | (_,p,_) <- ps]
    is_simple_pat other		       = False

    is_triv_pat (VarPat v)  = True
    is_triv_pat (WildPat _) = True
    is_triv_pat other       = False
\end{code}


@mkTupleExpr@ builds a tuple; the inverse to @mkTupleSelector@.  If it
has only one element, it is the identity function.

\begin{code}
mkTupleExpr :: [Id] -> CoreExpr

mkTupleExpr []	 = Con unitDataCon []
mkTupleExpr [id] = Var id
mkTupleExpr ids	 = mkCon (tupleCon (length ids))
			 (map idType ids)
			 [ VarArg i | i <- ids ]
\end{code}


@mkTupleSelector@ builds a selector which scrutises the given
expression and extracts the one name from the list given.
If you want the no-shadowing rule to apply, the caller
is responsible for making sure that none of these names
are in scope.

If there is just one id in the ``tuple'', then the selector is
just the identity.

\begin{code}
mkTupleSelector :: [Id]			-- The tuple args
		-> Id			-- The selected one
		-> CoreExpr		-- Scrutinee
		-> CoreExpr

mkTupleSelector [var] should_be_the_same_var scrut
  = ASSERT(var == should_be_the_same_var)
    scrut

mkTupleSelector vars the_var scrut
  = ASSERT( not (null vars) )
    Case scrut (AlgAlts [(tupleCon (length vars), vars, Var the_var)] NoDefault)
\end{code}


%************************************************************************
%*									*
\subsection[mkFailurePair]{Code for pattern-matching and other failures}
%*									*
%************************************************************************

Generally, we handle pattern matching failure like this: let-bind a
fail-variable, and use that variable if the thing fails:
\begin{verbatim}
	let fail.33 = error "Help"
	in
	case x of
		p1 -> ...
		p2 -> fail.33
		p3 -> fail.33
		p4 -> ...
\end{verbatim}
Then
\begin{itemize}
\item
If the case can't fail, then there'll be no mention of fail.33, and the
simplifier will later discard it.

\item
If it can fail in only one way, then the simplifier will inline it.

\item
Only if it is used more than once will the let-binding remain.
\end{itemize}

There's a problem when the result of the case expression is of
unboxed type.  Then the type of fail.33 is unboxed too, and
there is every chance that someone will change the let into a case:
\begin{verbatim}
	case error "Help" of
	  fail.33 -> case ....
\end{verbatim}

which is of course utterly wrong.  Rather than drop the condition that
only boxed types can be let-bound, we just turn the fail into a function
for the primitive case:
\begin{verbatim}
	let fail.33 :: Void -> Int#
	    fail.33 = \_ -> error "Help"
	in
	case x of
		p1 -> ...
		p2 -> fail.33 void
		p3 -> fail.33 void
		p4 -> ...
\end{verbatim}

Now fail.33 is a function, so it can be let-bound.

\begin{code}
mkFailurePair :: Type		-- Result type of the whole case expression
	      -> DsM (CoreExpr -> CoreBinding,
				-- Binds the newly-created fail variable
				-- to either the expression or \ _ -> expression
		      CoreExpr)	-- Either the fail variable, or fail variable
				-- applied to unit tuple
mkFailurePair ty
  | isUnpointedType ty
  = newFailLocalDs (voidTy `mkFunTy` ty)	`thenDs` \ fail_fun_var ->
    newSysLocalDs voidTy			`thenDs` \ fail_fun_arg ->
    returnDs (\ body ->
		NonRec fail_fun_var (Lam (ValBinder fail_fun_arg) body),
	      App (Var fail_fun_var) (VarArg voidId))

  | otherwise
  = newFailLocalDs ty 		`thenDs` \ fail_var ->
    returnDs (\ body -> NonRec fail_var body, Var fail_var)
\end{code}



