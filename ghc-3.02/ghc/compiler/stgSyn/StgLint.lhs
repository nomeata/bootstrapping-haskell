%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[StgLint]{A ``lint'' pass to check for Stg correctness}

\begin{code}
module StgLint ( lintStgBindings ) where

#include "HsVersions.h"

import StgSyn

import Bag		( Bag, emptyBag, isEmptyBag, snocBag, foldBag )
import Id		( idType, isAlgCon, dataConArgTys,
			  emptyIdSet, isEmptyIdSet, elementOfIdSet,
			  mkIdSet, intersectIdSets, 
			  unionIdSets, idSetToList, IdSet,
			  GenId{-instanced NamedThing-}, Id
			)
import Literal		( literalType, Literal{-instance Outputable-} )
import Maybes		( catMaybes )
import Name		( isLocallyDefined, getSrcLoc )
import ErrUtils		( ErrMsg )
import PrimOp		( primOpType )
import SrcLoc		( SrcLoc{-instance Outputable-} )
import Type		( mkFunTys, splitFunTys, splitAlgTyConApp_maybe,
			  isTyVarTy, Type
			)
import TyCon		( TyCon, isDataTyCon )
import Util		( zipEqual )
import GlaExts		( trace )
import Outputable

infixr 9 `thenL`, `thenL_`, `thenMaybeL`, `thenMaybeL_`

unDictifyTy = panic "StgLint.unDictifyTy (ToDo)"
\end{code}

Checks for
	(a) *some* type errors
	(b) locally-defined variables used but not defined

%************************************************************************
%*									*
\subsection{``lint'' for various constructs}
%*									*
%************************************************************************

@lintStgBindings@ is the top-level interface function.

\begin{code}
lintStgBindings :: String -> [StgBinding] -> [StgBinding]

lintStgBindings whodunnit binds
  = _scc_ "StgLint"
    case (initL (lint_binds binds)) of
      Nothing  -> binds
      Just msg -> pprPanic "" (vcat [
			ptext SLIT("*** Stg Lint ErrMsgs: in "),text whodunnit, ptext SLIT(" ***"),
			msg,
			ptext SLIT("*** Offending Program ***"),
			pprStgBindings binds,
			ptext SLIT("*** End of Offense ***")])
  where
    lint_binds :: [StgBinding] -> LintM ()

    lint_binds [] = returnL ()
    lint_binds (bind:binds)
      = lintStgBinds bind 		`thenL` \ binders ->
	addInScopeVars binders (
	    lint_binds binds
	)
\end{code}


\begin{code}
lintStgArg :: StgArg -> LintM (Maybe Type)

lintStgArg (StgLitArg lit)       = returnL (Just (literalType lit))
lintStgArg (StgConArg con)       = returnL (Just (idType con))
lintStgArg a@(StgVarArg v)
  = checkInScope v	`thenL_`
    returnL (Just (idType v))
\end{code}

\begin{code}
lintStgBinds :: StgBinding -> LintM [Id]		-- Returns the binders
lintStgBinds (StgNonRec binder rhs)
  = lint_binds_help (binder,rhs) 	`thenL_`
    returnL [binder]

lintStgBinds (StgRec pairs)
  = addInScopeVars binders (
	mapL lint_binds_help pairs `thenL_`
	returnL binders
    )
  where
    binders = [b | (b,_) <- pairs]

lint_binds_help (binder, rhs)
  = addLoc (RhsOf binder) (
	-- Check the rhs
	lintStgRhs rhs    `thenL` \ maybe_rhs_ty ->

	-- Check match to RHS type
	(case maybe_rhs_ty of
	  Nothing     -> returnL ()
	  Just rhs_ty -> checkTys (idType binder)
				   rhs_ty
				   (mkRhsMsg binder rhs_ty)
	)			`thenL_`

	returnL ()
    )
\end{code}

\begin{code}
lintStgRhs :: StgRhs -> LintM (Maybe Type)

lintStgRhs (StgRhsClosure _ _ _ _ binders expr)
  = addLoc (LambdaBodyOf binders) (
    addInScopeVars binders (
	lintStgExpr expr   `thenMaybeL` \ body_ty ->
	returnL (Just (mkFunTys (map idType binders) body_ty))
    ))

lintStgRhs (StgRhsCon _ con args)
  = mapMaybeL lintStgArg args	`thenL` \ maybe_arg_tys ->
    case maybe_arg_tys of
      Nothing 	    -> returnL Nothing
      Just arg_tys  -> checkFunApp con_ty arg_tys (mkRhsConMsg con_ty arg_tys)
  where
    con_ty = idType con
\end{code}

\begin{code}
lintStgExpr :: StgExpr -> LintM (Maybe Type)	-- Nothing if error found

lintStgExpr e@(StgApp fun args _)
  = lintStgArg fun		`thenMaybeL` \ fun_ty  ->
    mapMaybeL lintStgArg args	`thenL`      \ maybe_arg_tys ->
    case maybe_arg_tys of
      Nothing 	    -> returnL Nothing
      Just arg_tys  -> checkFunApp fun_ty arg_tys (mkFunAppMsg fun_ty arg_tys e)

lintStgExpr e@(StgCon con args _)
  = mapMaybeL lintStgArg args	`thenL` \ maybe_arg_tys ->
    case maybe_arg_tys of
      Nothing 	    -> returnL Nothing
      Just arg_tys  -> checkFunApp con_ty arg_tys (mkFunAppMsg con_ty arg_tys e)
  where
    con_ty = idType con

lintStgExpr e@(StgPrim op args _)
  = mapMaybeL lintStgArg args	`thenL` \ maybe_arg_tys ->
    case maybe_arg_tys of
      Nothing      -> returnL Nothing
      Just arg_tys -> checkFunApp op_ty arg_tys (mkFunAppMsg op_ty arg_tys e)
  where
    op_ty = primOpType op

lintStgExpr (StgLet binds body)
  = lintStgBinds binds		`thenL` \ binders ->
    addLoc (BodyOfLetRec binders) (
    addInScopeVars binders (
	lintStgExpr body
    ))

lintStgExpr (StgLetNoEscape _ _ binds body)
  = lintStgBinds binds		`thenL` \ binders ->
    addLoc (BodyOfLetRec binders) (
    addInScopeVars binders (
	lintStgExpr body
    ))

lintStgExpr (StgSCC _ _ expr)	= lintStgExpr expr

lintStgExpr e@(StgCase scrut _ _ _ alts)
  = lintStgExpr scrut		`thenMaybeL` \ _ ->

	-- Check that it is a data type
    case (splitAlgTyConApp_maybe scrut_ty) of
      Just (tycon, _, _) | isDataTyCon tycon
	      -> lintStgAlts alts scrut_ty tycon
      other   -> addErrL (mkCaseDataConMsg e)	`thenL_`
		 returnL Nothing
  where
    scrut_ty = get_ty alts

    get_ty (StgAlgAlts  ty _ _) = ty
    get_ty (StgPrimAlts ty _ _) = ty
\end{code}

\begin{code}
lintStgAlts :: StgCaseAlts
	     -> Type 		-- Type of scrutinee
	     -> TyCon			-- TyCon pinned on the case
	     -> LintM (Maybe Type)	-- Type of alternatives

lintStgAlts alts scrut_ty case_tycon
  = (case alts of
	 StgAlgAlts _ alg_alts deflt ->
	   mapL (lintAlgAlt scrut_ty) alg_alts 	`thenL` \ maybe_alt_tys ->
	   lintDeflt deflt scrut_ty		`thenL` \ maybe_deflt_ty ->
	   returnL (maybe_deflt_ty : maybe_alt_tys)

	 StgPrimAlts _ prim_alts deflt ->
	   mapL (lintPrimAlt scrut_ty) prim_alts `thenL` \ maybe_alt_tys ->
	   lintDeflt deflt scrut_ty		 `thenL` \ maybe_deflt_ty ->
	   returnL (maybe_deflt_ty : maybe_alt_tys)
    )						 `thenL` \ maybe_result_tys ->
	 -- Check the result types
    case catMaybes (maybe_result_tys) of
      []	     -> returnL Nothing

      (first_ty:tys) -> mapL check tys	`thenL_`
			returnL (Just first_ty)
	where
	  check ty = checkTys first_ty ty (mkCaseAltMsg alts)

lintAlgAlt scrut_ty (con, args, _, rhs)
  = (case splitAlgTyConApp_maybe scrut_ty of
      Nothing ->
	 addErrL (mkAlgAltMsg1 scrut_ty)
      Just (tycon, tys_applied, cons) ->
	 let
	   arg_tys = dataConArgTys con tys_applied
	 in
	 checkL (con `elem` cons) (mkAlgAltMsg2 scrut_ty con) `thenL_`
	 checkL (length arg_tys == length args) (mkAlgAltMsg3 con args)
								 `thenL_`
	 mapL check (zipEqual "lintAlgAlt:stg" arg_tys args)	 `thenL_`
	 returnL ()
    )								 `thenL_`
    addInScopeVars args 	(
	 lintStgExpr rhs
    )
  where
    check (ty, arg) = checkTys ty (idType arg) (mkAlgAltMsg4 ty arg)

    -- elem: yes, the elem-list here can sometimes be long-ish,
    -- but as it's use-once, probably not worth doing anything different
    -- We give it its own copy, so it isn't overloaded.
    elem _ []	    = False
    elem x (y:ys)   = x==y || elem x ys

lintPrimAlt scrut_ty alt@(lit,rhs)
 = checkTys (literalType lit) scrut_ty (mkPrimAltMsg alt)	`thenL_`
   lintStgExpr rhs

lintDeflt StgNoDefault scrut_ty = returnL Nothing
lintDeflt deflt@(StgBindDefault binder _ rhs) scrut_ty
  = checkTys (idType binder) scrut_ty (mkDefltMsg deflt)	`thenL_`
    addInScopeVars [binder] (
	lintStgExpr rhs
    )
\end{code}


%************************************************************************
%*									*
\subsection[lint-monad]{The Lint monad}
%*									*
%************************************************************************

\begin{code}
type LintM a = [LintLocInfo] 	-- Locations
	    -> IdSet		-- Local vars in scope
	    -> Bag ErrMsg	-- Error messages so far
	    -> (a, Bag ErrMsg)	-- Result and error messages (if any)

data LintLocInfo
  = RhsOf Id		-- The variable bound
  | LambdaBodyOf [Id]	-- The lambda-binder
  | BodyOfLetRec [Id]	-- One of the binders

instance Outputable LintLocInfo where
    ppr (RhsOf v)
      = hcat [ppr (getSrcLoc v), ptext SLIT(": [RHS of "), pp_binders [v], char ']']

    ppr (LambdaBodyOf bs)
      = hcat [ppr (getSrcLoc (head bs)),
		ptext SLIT(": [in body of lambda with binders "), pp_binders bs, char ']']

    ppr (BodyOfLetRec bs)
      = hcat [ppr (getSrcLoc (head bs)),
		ptext SLIT(": [in body of letrec with binders "), pp_binders bs, char ']']

pp_binders :: [Id] -> SDoc
pp_binders bs
  = sep (punctuate comma (map pp_binder bs))
  where
    pp_binder b
      = hsep [ppr b, ptext SLIT("::"), ppr (idType b)]
\end{code}

\begin{code}
initL :: LintM a -> Maybe ErrMsg
initL m
  = case (m [] emptyIdSet emptyBag) of { (_, errs) ->
    if isEmptyBag errs then
	Nothing
    else
	Just (foldBag ($$) (\ msg -> msg) empty errs)
    }

returnL :: a -> LintM a
returnL r loc scope errs = (r, errs)

thenL :: LintM a -> (a -> LintM b) -> LintM b
thenL m k loc scope errs
  = case m loc scope errs of
      (r, errs') -> k r loc scope errs'

thenL_ :: LintM a -> LintM b -> LintM b
thenL_ m k loc scope errs
  = case m loc scope errs of
      (_, errs') -> k loc scope errs'

thenMaybeL :: LintM (Maybe a) -> (a -> LintM (Maybe b)) -> LintM (Maybe b)
thenMaybeL m k loc scope errs
  = case m loc scope errs of
      (Nothing, errs2) -> (Nothing, errs2)
      (Just r,  errs2) -> k r loc scope errs2

thenMaybeL_ :: LintM (Maybe a) -> LintM (Maybe b) -> LintM (Maybe b)
thenMaybeL_ m k loc scope errs
  = case m loc scope errs of
      (Nothing, errs2) -> (Nothing, errs2)
      (Just _,  errs2) -> k loc scope errs2

mapL :: (a -> LintM b) -> [a] -> LintM [b]
mapL f [] = returnL []
mapL f (x:xs)
  = f x 	`thenL` \ r ->
    mapL f xs	`thenL` \ rs ->
    returnL (r:rs)

mapMaybeL :: (a -> LintM (Maybe b)) -> [a] -> LintM (Maybe [b])
	-- Returns Nothing if anything fails
mapMaybeL f [] = returnL (Just [])
mapMaybeL f (x:xs)
  = f x	    	    `thenMaybeL` \ r ->
    mapMaybeL f xs  `thenMaybeL` \ rs ->
    returnL (Just (r:rs))
\end{code}

\begin{code}
checkL :: Bool -> ErrMsg -> LintM ()
checkL True  msg loc scope errs = ((), errs)
checkL False msg loc scope errs = ((), addErr errs msg loc)

addErrL :: ErrMsg -> LintM ()
addErrL msg loc scope errs = ((), addErr errs msg loc)

addErr :: Bag ErrMsg -> ErrMsg -> [LintLocInfo] -> Bag ErrMsg

addErr errs_so_far msg locs
  = errs_so_far `snocBag` (hang (ppr (head locs)) 4 msg)

addLoc :: LintLocInfo -> LintM a -> LintM a
addLoc extra_loc m loc scope errs
  = m (extra_loc:loc) scope errs

addInScopeVars :: [Id] -> LintM a -> LintM a
addInScopeVars ids m loc scope errs
  = -- We check if these "new" ids are already
    -- in scope, i.e., we have *shadowing* going on.
    -- For now, it's just a "trace"; we may make
    -- a real error out of it...
    let
	new_set = mkIdSet ids

	shadowed = scope `intersectIdSets` new_set
    in
--  After adding -fliberate-case, Simon decided he likes shadowed
--  names after all.  WDP 94/07
--  (if isEmptyIdSet shadowed
--  then id
--  else pprTrace "Shadowed vars:" (ppr (idSetToList shadowed))) $
    m loc (scope `unionIdSets` new_set) errs
\end{code}

\begin{code}
checkFunApp :: Type 		-- The function type
	    -> [Type] 	-- The arg type(s)
	    -> ErrMsg 		-- Error messgae
	    -> LintM (Maybe Type)	-- The result type

checkFunApp fun_ty arg_tys msg loc scope errs
  = cfa res_ty expected_arg_tys arg_tys
  where
    (expected_arg_tys, res_ty) = splitFunTys fun_ty

    cfa res_ty expected []	-- Args have run out; that's fine
      = (Just (mkFunTys expected res_ty), errs)

    cfa res_ty [] arg_tys	-- Expected arg tys ran out first;
				-- first see if res_ty is a tyvar template;
				-- otherwise, maybe res_ty is a
				-- dictionary type which is actually a function?
      | isTyVarTy res_ty
      = (Just res_ty, errs)
      | otherwise
      = case splitFunTys (unDictifyTy res_ty) of
	  ([], _) 		  -> (Nothing, addErr errs msg loc)	-- Too many args
	  (new_expected, new_res) -> cfa new_res new_expected arg_tys

    cfa res_ty (expected_arg_ty:expected_arg_tys) (arg_ty:arg_tys)
      = if (sleazy_eq_ty expected_arg_ty arg_ty)
	then cfa res_ty expected_arg_tys arg_tys
	else (Nothing, addErr errs msg loc) -- Arg mis-match
\end{code}

\begin{code}
checkInScope :: Id -> LintM ()
checkInScope id loc scope errs
  = if isLocallyDefined id && not (isAlgCon id) && not (id `elementOfIdSet` scope) then
	((), addErr errs (hsep [ppr id, ptext SLIT("is out of scope")]) loc)
    else
	((), errs)

checkTys :: Type -> Type -> ErrMsg -> LintM ()
checkTys ty1 ty2 msg loc scope errs
  = if (sleazy_eq_ty ty1 ty2)
    then ((), errs)
    else ((), addErr errs msg loc)
\end{code}

\begin{code}
mkCaseAltMsg :: StgCaseAlts -> ErrMsg
mkCaseAltMsg alts
  = ($$) (text "In some case alternatives, type of alternatives not all same:")
	    -- LATER: (ppr alts)
	    (panic "mkCaseAltMsg")

mkCaseDataConMsg :: StgExpr -> ErrMsg
mkCaseDataConMsg expr
  = ($$) (ptext SLIT("A case scrutinee not a type-constructor type:"))
	    (pp_expr expr)

mkCaseAbstractMsg :: TyCon -> ErrMsg
mkCaseAbstractMsg tycon
  = ($$) (ptext SLIT("An algebraic case on an abstract type:"))
	    (ppr tycon)

mkDefltMsg :: StgCaseDefault -> ErrMsg
mkDefltMsg deflt
  = ($$) (ptext SLIT("Binder in default case of a case expression doesn't match type of scrutinee:"))
	    --LATER: (ppr deflt)
	    (panic "mkDefltMsg")

mkFunAppMsg :: Type -> [Type] -> StgExpr -> ErrMsg
mkFunAppMsg fun_ty arg_tys expr
  = vcat [text "In a function application, function type doesn't match arg types:",
	      hang (ptext SLIT("Function type:")) 4 (ppr fun_ty),
	      hang (ptext SLIT("Arg types:")) 4 (vcat (map (ppr) arg_tys)),
	      hang (ptext SLIT("Expression:")) 4 (pp_expr expr)]

mkRhsConMsg :: Type -> [Type] -> ErrMsg
mkRhsConMsg fun_ty arg_tys
  = vcat [text "In a RHS constructor application, con type doesn't match arg types:",
	      hang (ptext SLIT("Constructor type:")) 4 (ppr fun_ty),
	      hang (ptext SLIT("Arg types:")) 4 (vcat (map (ppr) arg_tys))]

mkUnappTyMsg :: Id -> Type -> ErrMsg
mkUnappTyMsg var ty
  = vcat [text "Variable has a for-all type, but isn't applied to any types.",
	      (<>) (ptext SLIT("Var:      ")) (ppr var),
	      (<>) (ptext SLIT("Its type: ")) (ppr ty)]

mkAlgAltMsg1 :: Type -> ErrMsg
mkAlgAltMsg1 ty
  = ($$) (text "In some case statement, type of scrutinee is not a data type:")
	    (ppr ty)

mkAlgAltMsg2 :: Type -> Id -> ErrMsg
mkAlgAltMsg2 ty con
  = vcat [
	text "In some algebraic case alternative, constructor is not a constructor of scrutinee type:",
	ppr ty,
	ppr con
    ]

mkAlgAltMsg3 :: Id -> [Id] -> ErrMsg
mkAlgAltMsg3 con alts
  = vcat [
	text "In some algebraic case alternative, number of arguments doesn't match constructor:",
	ppr con,
	ppr alts
    ]

mkAlgAltMsg4 :: Type -> Id -> ErrMsg
mkAlgAltMsg4 ty arg
  = vcat [
	text "In some algebraic case alternative, type of argument doesn't match data constructor:",
	ppr ty,
	ppr arg
    ]

mkPrimAltMsg :: (Literal, StgExpr) -> ErrMsg
mkPrimAltMsg alt
  = ($$) (text "In a primitive case alternative, type of literal doesn't match type of scrutinee:")
	    (ppr alt)

mkRhsMsg :: Id -> Type -> ErrMsg
mkRhsMsg binder ty
  = vcat [hsep [ptext SLIT("The type of this binder doesn't match the type of its RHS:"),
		     ppr binder],
	      hsep [ptext SLIT("Binder's type:"), ppr (idType binder)],
	      hsep [ptext SLIT("Rhs type:"), ppr ty]
	     ]

pp_expr :: StgExpr -> SDoc
pp_expr expr = ppr expr

sleazy_eq_ty ty1 ty2
	-- NB: probably severe overkill (WDP 95/04)
  = trace "StgLint.sleazy_eq_ty:use eqSimplTy?" $
    case (splitFunTys ty1) of { (tyargs1,tyres1) ->
    case (splitFunTys ty2) of { (tyargs2,tyres2) ->
    let
	ty11 = mkFunTys tyargs1 tyres1
	ty22 = mkFunTys tyargs2 tyres2
    in
    ty11 == ty22 }}
\end{code}
