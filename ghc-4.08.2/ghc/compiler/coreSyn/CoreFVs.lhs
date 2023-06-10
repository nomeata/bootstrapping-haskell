%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
Taken quite directly from the Peyton Jones/Lester paper.

\begin{code}
module CoreFVs (
	exprFreeVars, exprsFreeVars,
	exprSomeFreeVars, exprsSomeFreeVars,
	idRuleVars, idFreeVars, ruleSomeFreeVars, ruleSomeLhsFreeVars,

	CoreExprWithFVs, CoreBindWithFVs, freeVars, freeVarsOf,
    ) where

#include "HsVersions.h"

import CoreSyn
import Id		( Id, idFreeTyVars, idSpecialisation )
import VarSet
import Var		( Var, isId )
import Name		( isLocallyDefined )
import Type		( tyVarsOfType, Type )
import Util		( mapAndUnzip )
\end{code}

%************************************************************************
%*									*
\section{Finding the free variables of an expression}
%*									*
%************************************************************************

This function simply finds the free variables of an expression.
So far as type variables are concerned, it only finds tyvars that are

	* free in type arguments, 
	* free in the type of a binder,

but not those that are free in the type of variable occurrence.

\begin{code}
exprFreeVars :: CoreExpr -> VarSet	-- Find all locally-defined free Ids or tyvars
exprFreeVars = exprSomeFreeVars isLocallyDefined

exprsFreeVars :: [CoreExpr] -> VarSet
exprsFreeVars = foldr (unionVarSet . exprFreeVars) emptyVarSet

exprSomeFreeVars :: InterestingVarFun 	-- Says which Vars are interesting
		 -> CoreExpr
		 -> VarSet
exprSomeFreeVars fv_cand e = expr_fvs e fv_cand emptyVarSet

exprsSomeFreeVars :: InterestingVarFun 	-- Says which Vars are interesting
		  -> [CoreExpr]
		  -> VarSet
exprsSomeFreeVars fv_cand = foldr (unionVarSet . exprSomeFreeVars fv_cand) emptyVarSet

type InterestingVarFun = Var -> Bool	-- True <=> interesting
\end{code}


\begin{code}
type FV = InterestingVarFun 
	  -> VarSet		-- In scope
	  -> VarSet		-- Free vars

union :: FV -> FV -> FV
union fv1 fv2 fv_cand in_scope = fv1 fv_cand in_scope `unionVarSet` fv2 fv_cand in_scope

noVars :: FV
noVars fv_cand in_scope = emptyVarSet

-- At a variable occurrence, add in any free variables of its rule rhss
-- Curiously, we gather the Id's free *type* variables from its binding
-- site, but its free *rule-rhs* variables from its usage sites.  This
-- is a little weird.  The reason is that the former is more efficient,
-- but the latter is more fine grained, and a makes a difference when
-- a variable mentions itself one of its own rule RHSs
oneVar :: Var -> FV
oneVar var fv_cand in_scope
  = foldVarSet add_rule_var var_itself_set (idRuleVars var)
  where
    var_itself_set | keep_it fv_cand in_scope var = unitVarSet var
	           | otherwise		      = emptyVarSet
    add_rule_var var set | keep_it fv_cand in_scope var = extendVarSet set var
			 | otherwise			= set

someVars :: VarSet -> FV
someVars vars fv_cand in_scope
  = filterVarSet (keep_it fv_cand in_scope) vars

keep_it fv_cand in_scope var
  | var `elemVarSet` in_scope = False
  | fv_cand var		      = True
  | otherwise		      = False


addBndr :: CoreBndr -> FV -> FV
addBndr bndr fv fv_cand in_scope
  | isId bndr = inside_fvs `unionVarSet` someVars (idFreeTyVars bndr) fv_cand in_scope
  | otherwise = inside_fvs
  where
    inside_fvs = fv fv_cand (in_scope `extendVarSet` bndr) 

addBndrs :: [CoreBndr] -> FV -> FV
addBndrs bndrs fv = foldr addBndr fv bndrs
\end{code}


\begin{code}
expr_fvs :: CoreExpr -> FV

expr_fvs (Type ty) 	 = someVars (tyVarsOfType ty)
expr_fvs (Var var) 	 = oneVar var
expr_fvs (Lit lit)	 = noVars
expr_fvs (Note _ expr)   = expr_fvs expr
expr_fvs (App fun arg)   = expr_fvs fun `union` expr_fvs arg
expr_fvs (Lam bndr body) = addBndr bndr (expr_fvs body)

expr_fvs (Case scrut bndr alts)
  = expr_fvs scrut `union` addBndr bndr (foldr (union . alt_fvs) noVars alts)
  where
    alt_fvs (con, bndrs, rhs) = addBndrs bndrs (expr_fvs rhs)

expr_fvs (Let (NonRec bndr rhs) body)
  = expr_fvs rhs `union` addBndr bndr (expr_fvs body)

expr_fvs (Let (Rec pairs) body)
  = addBndrs bndrs (foldr (union . expr_fvs) (expr_fvs body) rhss)
  where
    (bndrs,rhss) = unzip pairs
\end{code}



\begin{code}
idRuleVars ::Id -> VarSet
idRuleVars id = rulesRhsFreeVars (idSpecialisation id)

idFreeVars :: Id -> VarSet
idFreeVars id = idRuleVars id `unionVarSet` idFreeTyVars id

rulesSomeFreeVars :: InterestingVarFun -> CoreRules -> VarSet
rulesSomeFreeVars interesting (Rules rules _)
  = foldr (unionVarSet . ruleSomeFreeVars interesting) emptyVarSet rules

ruleSomeFreeVars :: InterestingVarFun -> CoreRule -> VarSet
ruleSomeFreeVars interesting (BuiltinRule _) = noFVs
ruleSomeFreeVars interesting (Rule _ tpl_vars tpl_args rhs)
  = rule_fvs interesting emptyVarSet
  where
    rule_fvs = addBndrs tpl_vars $
	       foldr (union . expr_fvs) (expr_fvs rhs) tpl_args

ruleSomeLhsFreeVars :: InterestingVarFun -> CoreRule -> VarSet
ruleSomeLhsFreeVars fn (BuiltinRule _) = noFVs
ruleSomeLhsFreeVars fn (Rule _ tpl_vars tpl_args rhs)
  = foldl delVarSet (exprsSomeFreeVars fn tpl_args) tpl_vars
\end{code}


%************************************************************************
%*									*
\section[freevars-everywhere]{Attaching free variables to every sub-expression}
%*									*
%************************************************************************

The free variable pass annotates every node in the expression with its
NON-GLOBAL free variables and type variables.

\begin{code}
type CoreBindWithFVs = AnnBind Id VarSet
type CoreExprWithFVs = AnnExpr Id VarSet
	-- Every node annotated with its free variables,
	-- both Ids and TyVars

freeVarsOf :: CoreExprWithFVs -> IdSet
freeVarsOf (free_vars, _) = free_vars

noFVs    = emptyVarSet
aFreeVar = unitVarSet
unionFVs = unionVarSet

filters :: Var -> VarSet -> VarSet

-- (b `filters` s) removes the binder b from the free variable set s,
-- but *adds* to s
--	(a) the free variables of b's type
--	(b) the idSpecVars of b
--
-- This is really important for some lambdas:
-- 	In (\x::a -> x) the only mention of "a" is in the binder.
--
-- Also in
--	let x::a = b in ...
-- we should really note that "a" is free in this expression.
-- It'll be pinned inside the /\a by the binding for b, but
-- it seems cleaner to make sure that a is in the free-var set 
-- when it is mentioned.
--
-- This also shows up in recursive bindings.  Consider:
--	/\a -> letrec x::a = x in E
-- Now, there are no explicit free type variables in the RHS of x,
-- but nevertheless "a" is free in its definition.  So we add in
-- the free tyvars of the types of the binders, and include these in the
-- free vars of the group, attached to the top level of each RHS.
--
-- This actually happened in the defn of errorIO in IOBase.lhs:
--	errorIO (ST io) = case (errorIO# io) of
--	    		    _ -> bottom
--			  where
--			    bottom = bottom -- Never evaluated

filters b s | isId b    = (s `delVarSet` b) `unionFVs` idFreeVars b
	    | otherwise = s `delVarSet` b
\end{code}


%************************************************************************
%*									*
\subsection{Free variables (and types)}
%*									*
%************************************************************************

\begin{code}
freeVars :: CoreExpr -> CoreExprWithFVs

freeVars (Var v)
  = (fvs, AnnVar v)
  where
	-- ToDo: insert motivating example for why we *need*
	-- to include the idSpecVars in the FV list.
	--	Actually [June 98] I don't think it's necessary
	-- fvs = fvs_v `unionVarSet` idSpecVars v

    fvs | isLocallyDefined v = aFreeVar v
	| otherwise	     = noFVs

freeVars (Lit lit) = (noFVs, AnnLit lit)
freeVars (Lam b body)
  = (b `filters` freeVarsOf body', AnnLam b body')
  where
    body' = freeVars body

freeVars (App fun arg)
  = (freeVarsOf fun2 `unionFVs` freeVarsOf arg2, AnnApp fun2 arg2)
  where
    fun2 = freeVars fun
    arg2 = freeVars arg

freeVars (Case scrut bndr alts)
  = ((bndr `filters` alts_fvs) `unionFVs` freeVarsOf scrut2,
     AnnCase scrut2 bndr alts2)
  where
    scrut2 = freeVars scrut

    (alts_fvs_s, alts2) = mapAndUnzip fv_alt alts
    alts_fvs 		= foldr1 unionFVs alts_fvs_s

    fv_alt (con,args,rhs) = (foldr filters (freeVarsOf rhs2) args,
			     (con, args, rhs2))
		    	  where
			     rhs2 = freeVars rhs

freeVars (Let (NonRec binder rhs) body)
  = (freeVarsOf rhs2 `unionFVs` body_fvs,
     AnnLet (AnnNonRec binder rhs2) body2)
  where
    rhs2     = freeVars rhs
    body2    = freeVars body
    body_fvs = binder `filters` freeVarsOf body2

freeVars (Let (Rec binds) body)
  = (foldl delVarSet group_fvs binders,
	-- The "filters" part may have added one of the binders
	-- via the idSpecVars part, so we must delete it again
     AnnLet (AnnRec (binders `zip` rhss2)) body2)
  where
    (binders, rhss) = unzip binds

    rhss2     = map freeVars rhss
    all_fvs   = foldr (unionFVs . fst) body_fvs rhss2
    group_fvs = foldr filters all_fvs binders

    body2     = freeVars body
    body_fvs  = freeVarsOf body2

freeVars (Note (Coerce to_ty from_ty) expr)
  = (freeVarsOf expr2 `unionFVs` tfvs1 `unionFVs` tfvs2,
     AnnNote (Coerce to_ty from_ty) expr2)
  where
    expr2  = freeVars expr
    tfvs1  = tyVarsOfType from_ty
    tfvs2  = tyVarsOfType to_ty

freeVars (Note other_note expr)
  = (freeVarsOf expr2, AnnNote other_note expr2)
  where
    expr2 = freeVars expr

freeVars (Type ty) = (tyVarsOfType ty, AnnType ty)
\end{code}

