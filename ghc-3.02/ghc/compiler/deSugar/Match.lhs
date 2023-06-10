
%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Main_match]{The @match@ function}

\begin{code}
module Match ( match, matchExport, matchWrapper, matchSimply ) where

#include "HsVersions.h"

import {-# SOURCE #-} DsExpr  ( dsExpr  )
import {-# SOURCE #-} DsBinds ( dsBinds )

import CmdLineOpts	( opt_WarnIncompletePatterns, opt_WarnOverlappingPatterns,
			  opt_WarnSimplePatterns
     			)
import HsSyn		
import TcHsSyn		( TypecheckedPat, TypecheckedMatch,
			  TypecheckedHsBinds, TypecheckedHsExpr	)
import DsHsSyn		( outPatType )
import Check            ( check, ExhaustivePat, WarningPat, BoxedString )
import CoreSyn
import CoreUtils	( coreExprType )
import DsMonad
import DsGRHSs		( dsGRHSs )
import DsUtils
import Id		( idType, dataConFieldLabels,
			  dataConArgTys, recordSelectorFieldLabel,
			  Id
			)
import MatchCon		( matchConFamily )
import MatchLit		( matchLiterals )
import Name		( Name {--O only-} )
import PrelVals		( pAT_ERROR_ID )
import Type		( isUnpointedType, splitAlgTyConApp,
			  Type
			)
import TyVar		( TyVar )
import TysPrim		( intPrimTy, charPrimTy, floatPrimTy, doublePrimTy,
			  addrPrimTy, wordPrimTy
			)
import TysWiredIn	( nilDataCon, consDataCon, mkTupleTy, mkListTy,
			  charTy, charDataCon, intTy, intDataCon,
			  floatTy, floatDataCon, doubleTy, tupleCon,
			  doubleDataCon, addrTy,
			  addrDataCon, wordTy, wordDataCon
			)
import UniqSet
import Outputable
\end{code}

This function is a wrapper of @match@, it must be called from all the parts where 
it was called match, but only substitutes the firs call, ....
if the associated flags are declared, warnings will be issued.
It can not be called matchWrapper because this name already exists :-(

JJCQ 30-Nov-1997

\begin{code}
matchExport :: [Id]	        -- Vars rep'ing the exprs we're matching with
            -> [EquationInfo]   -- Info about patterns, etc. (type synonym below)
            -> DsM MatchResult  -- Desugared result!

matchExport vars qs@((EqnInfo _ ctx _ (MatchResult _ _ _)) : _)
  | incomplete && shadow = 
      dsShadowWarn ctx eqns_shadow		`thenDs`   \ () ->
      dsIncompleteWarn ctx pats			`thenDs`   \ () ->
      match vars qs
  | incomplete            = 
      dsIncompleteWarn ctx pats			`thenDs`   \ () ->
      match vars qs
  | shadow                = 
      dsShadowWarn ctx eqns_shadow		`thenDs`   \ () ->
      match vars qs
  | otherwise             =
      match vars qs
  where (pats,indexs) = check qs
        incomplete    = opt_WarnIncompletePatterns && (length pats /= 0)
        shadow        = opt_WarnOverlappingPatterns && sizeUniqSet indexs < no_eqns
        no_eqns       = length qs
	unused_eqns   = uniqSetToList (mkUniqSet [1..no_eqns] `minusUniqSet` indexs)
	eqns_shadow   = map (\n -> qs!!(n - 1)) unused_eqns
\end{code}

This variable shows the maximun number of lines of output generated for warnings.
It will limit the number of patterns/equations displayed to maximum_output.

(ToDo: add command-line option?)

\begin{code}
maximum_output = 4
\end{code}

The next two functions creates the warning message.

\begin{code}
dsShadowWarn :: DsMatchContext -> [EquationInfo] -> DsM ()
dsShadowWarn ctx@(DsMatchContext kind _ _) qs = dsWarn warn 
	where
	  warn | length qs > maximum_output
               = hang (pp_context ctx (ptext SLIT("are overlapped")))
                    12 ((vcat $ map (ppr_eqn kind) (take maximum_output qs))
                        $$ ptext SLIT("..."))
	       | otherwise
               = hang (pp_context ctx (ptext SLIT("are overlapped")))
                    12 (vcat $ map (ppr_eqn kind) qs)

dsIncompleteWarn :: DsMatchContext -> [ExhaustivePat] -> DsM ()
dsIncompleteWarn ctx@(DsMatchContext kind _ _) pats = dsWarn warn 
	where
	  warn | length pats > maximum_output
               = hang (pp_context ctx (ptext SLIT("are non-exhaustive")))
                    12 (hang (ptext SLIT("Patterns not recognized:"))
                       4 ((vcat $ map (ppr_incomplete_pats kind) (take maximum_output pats))
                          $$ ptext SLIT("...")))
	       | otherwise
               = hang (pp_context ctx (ptext SLIT("are non-exhaustive")))
                    12 (hang (ptext SLIT("Patterns not recognized:"))
                       4 (vcat $ map (ppr_incomplete_pats kind) pats))

pp_context NoMatchContext msg = ptext SLIT("Some match(es)") <+> msg

pp_context (DsMatchContext kind pats loc) msg
  = hang (hcat [ppr loc, ptext SLIT(": ")])
	     4 (hang message
		     4 (pp_match kind pats))
 where
    message = ptext SLIT("Pattern match(es)") <+> msg     

    pp_match (FunMatch fun) pats
      = hsep [ptext SLIT("in the definition of function"), quotes (ppr fun)]

    pp_match CaseMatch pats
      = hang (ptext SLIT("in a group of case alternatives beginning:"))
	4 (ppr_pats pats)

    pp_match PatBindMatch pats
      = hang (ptext SLIT("in a pattern binding:"))
	4 (ppr_pats pats)

    pp_match LambdaMatch pats
      = hang (ptext SLIT("in a lambda abstraction:"))
	4 (ppr_pats pats)

    pp_match DoBindMatch pats
      = hang (ptext SLIT("in a `do' pattern binding:"))
	4 (ppr_pats pats)

    pp_match ListCompMatch pats
      = hang (ptext SLIT("in a `list comprension' pattern binding:"))
	4 (ppr_pats pats)

    pp_match LetMatch pats
      = hang (ptext SLIT("in a `let' pattern binding:"))
	4 (ppr_pats pats)

ppr_pats pats = sep (map ppr pats)

separator (FunMatch _)    = SLIT("=")
separator (CaseMatch)     = SLIT("->") 
separator (LambdaMatch)   = SLIT("->") 
separator (PatBindMatch)  = panic "When is this used?"
separator (DoBindMatch)   = SLIT("<-")  
separator (ListCompMatch) = SLIT("<-")  
separator (LetMatch)      = SLIT("=")
                 
ppr_shadow_pats kind pats = sep [ppr_pats pats, ptext (separator kind), ptext SLIT("...")]
    
ppr_incomplete_pats kind (pats,[]) = ppr_pats pats
ppr_incomplete_pats kind (pats,constraints) = 
	                 sep [ppr_pats pats, ptext SLIT("with"), 
	                      sep (map ppr_constraint constraints)]
    

ppr_constraint (var,pats) = sep [ppr var, ptext SLIT("`not_elem`"), ppr pats]

ppr_eqn kind (EqnInfo _ _ pats _) = ppr_shadow_pats kind pats

\end{code}


The function @match@ is basically the same as in the Wadler chapter,
except it is monadised, to carry around the name supply, info about
annotations, etc.

Notes on @match@'s arguments, assuming $m$ equations and $n$ patterns:
\begin{enumerate}
\item
A list of $n$ variable names, those variables presumably bound to the
$n$ expressions being matched against the $n$ patterns.  Using the
list of $n$ expressions as the first argument showed no benefit and
some inelegance.

\item
The second argument, a list giving the ``equation info'' for each of
the $m$ equations:
\begin{itemize}
\item
the $n$ patterns for that equation, and
\item
a list of Core bindings [@(Id, CoreExpr)@ pairs] to be ``stuck on
the front'' of the matching code, as in:
\begin{verbatim}
let <binds>
in  <matching-code>
\end{verbatim}
\item
and finally: (ToDo: fill in)

The right way to think about the ``after-match function'' is that it
is an embryonic @CoreExpr@ with a ``hole'' at the end for the
final ``else expression''.
\end{itemize}

There is a type synonym, @EquationInfo@, defined in module @DsUtils@.

An experiment with re-ordering this information about equations (in
particular, having the patterns available in column-major order)
showed no benefit.

\item
A default expression---what to evaluate if the overall pattern-match
fails.  This expression will (almost?) always be
a measly expression @Var@, unless we know it will only be used once
(as we do in @glue_success_exprs@).

Leaving out this third argument to @match@ (and slamming in lots of
@Var "fail"@s) is a positively {\em bad} idea, because it makes it
impossible to share the default expressions.  (Also, it stands no
chance of working in our post-upheaval world of @Locals@.)
\end{enumerate}
So, the full type signature:
\begin{code}
match :: [Id]		  -- Variables rep'ing the exprs we're matching with
      -> [EquationInfo]	  -- Info about patterns, etc. (type synonym below)
      -> DsM MatchResult  -- Desugared result!
\end{code}

Note: @match@ is often called via @matchWrapper@ (end of this module),
a function that does much of the house-keeping that goes with a call
to @match@.

It is also worth mentioning the {\em typical} way a block of equations
is desugared with @match@.  At each stage, it is the first column of
patterns that is examined.  The steps carried out are roughly:
\begin{enumerate}
\item
Tidy the patterns in column~1 with @tidyEqnInfo@ (this may add
bindings to the second component of the equation-info):
\begin{itemize}
\item
Remove the `as' patterns from column~1.
\item
Make all constructor patterns in column~1 into @ConPats@, notably
@ListPats@ and @TuplePats@.
\item
Handle any irrefutable (or ``twiddle'') @LazyPats@.
\end{itemize}
\item
Now {\em unmix} the equations into {\em blocks} [w/ local function
@unmix_eqns@], in which the equations in a block all have variable
patterns in column~1, or they all have constructor patterns in ...
(see ``the mixture rule'' in SLPJ).
\item
Call @matchUnmixedEqns@ on each block of equations; it will do the
appropriate thing for each kind of column-1 pattern, usually ending up
in a recursive call to @match@.
\end{enumerate}

%************************************************************************
%*									*
%*  match: empty rule							*
%*									*
%************************************************************************
\subsection[Match-empty-rule]{The ``empty rule''}

We are a little more paranoid about the ``empty rule'' (SLPJ, p.~87)
than the Wadler-chapter code for @match@ (p.~93, first @match@ clause).
And gluing the ``success expressions'' together isn't quite so pretty.

\begin{code}
match [] eqns_info
  = complete_matches eqns_info
  where
    complete_matches [eqn] 
	= complete_match eqn
 
    complete_matches (eqn:eqns)
	= complete_match eqn		`thenDs` \ match_result1 ->
	  complete_matches eqns 	`thenDs` \ match_result2 ->
	  combineMatchResults match_result1 match_result2

    complete_match (EqnInfo _ _ [] match_result@(MatchResult _ _ _))
	= returnDs match_result
\end{code}

%************************************************************************
%*									*
%*  match: non-empty rule						*
%*									*
%************************************************************************
\subsection[Match-nonempty]{@match@ when non-empty: unmixing}

This (more interesting) clause of @match@ uses @tidy_and_unmix_eqns@
(a)~to get `as'- and `twiddle'-patterns out of the way (tidying), and
(b)~to do ``the mixture rule'' (SLPJ, p.~88) [which really {\em
un}mixes the equations], producing a list of equation-info
blocks, each block having as its first column of patterns either all
constructors, or all variables (or similar beasts), etc.

@match_unmixed_eqn_blks@ simply takes the place of the @foldr@ in the
Wadler-chapter @match@ (p.~93, last clause), and @match_unmixed_blk@
corresponds roughly to @matchVarCon@.

\begin{code}
match vars@(v:vs) eqns_info
  = mapDs (tidyEqnInfo v) eqns_info	`thenDs` \ tidy_eqns_info ->
    let
	tidy_eqns_blks = unmix_eqns tidy_eqns_info
    in
    match_unmixed_eqn_blks vars tidy_eqns_blks
  where
    unmix_eqns []    = []
    unmix_eqns [eqn] = [ [eqn] ]
    unmix_eqns (eq1@(EqnInfo _ _ (p1:p1s) _) : eq2@(EqnInfo _ _ (p2:p2s) _) : eqs)
      = if (  (irrefutablePat p1 && irrefutablePat p2)
	   || (isConPat       p1 && isConPat 	   p2)
	   || (isLitPat       p1 && isLitPat 	   p2) ) then
	    eq1 `tack_onto` unmixed_rest
	else
	    [ eq1 ] : unmixed_rest
      where
	unmixed_rest = unmix_eqns (eq2:eqs)

	x `tack_onto` xss   = ( x : head xss) : tail xss

    -----------------------------------------------------------------------
    -- loop through the blocks:
    -- subsequent blocks create a "fail expr" for the first one...
    match_unmixed_eqn_blks :: [Id]
			   -> [ [EquationInfo] ]	-- List of eqn BLOCKS
			   -> DsM MatchResult

    match_unmixed_eqn_blks vars [] = panic "match_unmixed_eqn_blks"

    match_unmixed_eqn_blks vars [eqn_blk] = matchUnmixedEqns vars eqn_blk 

    match_unmixed_eqn_blks vars (eqn_blk:eqn_blks) 
      = matchUnmixedEqns vars eqn_blk    	`thenDs` \ match_result1 ->  -- try to match with first blk
	match_unmixed_eqn_blks vars eqn_blks 	`thenDs` \ match_result2 ->
	combineMatchResults match_result1 match_result2
\end{code}

Tidy up the leftmost pattern in an @EquationInfo@, given the variable @v@
which will be scrutinised.  This means:
\begin{itemize}
\item
Replace variable patterns @x@ (@x /= v@) with the pattern @_@,
together with the binding @x = v@.
\item
Replace the `as' pattern @x@@p@ with the pattern p and a binding @x = v@.
\item
Removing lazy (irrefutable) patterns (you don't want to know...).
\item
Converting explicit tuple- and list-pats into ordinary @ConPats@.
\item
Convert the literal pat "" to [].
\end{itemize}

The result of this tidying is that the column of patterns will include
{\em only}:
\begin{description}
\item[@WildPats@:]
The @VarPat@ information isn't needed any more after this.

\item[@ConPats@:]
@ListPats@, @TuplePats@, etc., are all converted into @ConPats@.

\item[@LitPats@ and @NPats@:]
@LitPats@/@NPats@ of ``known friendly types'' (Int, Char,
Float, 	Double, at least) are converted to unboxed form; e.g.,
\tr{(NPat (HsInt i) _ _)} is converted to:
\begin{verbatim}
(ConPat I# _ _ [LitPat (HsIntPrim i) _])
\end{verbatim}
\end{description}

\begin{code}
tidyEqnInfo :: Id -> EquationInfo -> DsM EquationInfo
	-- DsM'd because of internal call to "match".
	-- "tidy1" does the interesting stuff, looking at
	-- one pattern and fiddling the list of bindings.
tidyEqnInfo v (EqnInfo n ctx (pat : pats) match_result)
  = tidy1 v pat match_result	`thenDs` \ (pat', match_result') ->
    returnDs (EqnInfo n ctx (pat' : pats) match_result')

tidy1 :: Id 					-- The Id being scrutinised
      -> TypecheckedPat 			-- The pattern against which it is to be matched
      -> MatchResult				-- Current thing do do after matching
      -> DsM (TypecheckedPat, 			-- Equivalent pattern
	      MatchResult)			-- Augmented thing to do afterwards
						-- The augmentation usually takes the form
						-- of new bindings to be added to the front

tidy1 v (VarPat var) match_result
  = returnDs (WildPat (idType var),
	      mkCoLetsMatchResult extra_binds match_result)
  where
    extra_binds | v == var  = []
		| otherwise = [NonRec var (Var v)]

tidy1 v (AsPat var pat) match_result
  = tidy1 v pat (mkCoLetsMatchResult extra_binds match_result)
  where
    extra_binds | v == var  = []
		| otherwise = [NonRec var (Var v)]

tidy1 v (WildPat ty) match_result
  = returnDs (WildPat ty, match_result)

{- now, here we handle lazy patterns:
    tidy1 v ~p bs = (v, v1 = case v of p -> v1 :
			v2 = case v of p -> v2 : ... : bs )

    where the v_i's are the binders in the pattern.

    ToDo: in "v_i = ... -> v_i", are the v_i's really the same thing?

    The case expr for v_i is just: match [v] [(p, [], \ x -> Var v_i)] any_expr
-}

tidy1 v (LazyPat pat) match_result
  = mkSelectorBinds pat (Var v)		`thenDs` \ sel_binds ->
    returnDs (WildPat (idType v),
	      mkCoLetsMatchResult [NonRec b rhs | (b,rhs) <- sel_binds] match_result)

-- re-express <con-something> as (ConPat ...) [directly]

tidy1 v (ConOpPat pat1 id pat2 ty) match_result
  = returnDs (ConPat id ty [pat1, pat2], match_result)

tidy1 v (RecPat con_id pat_ty rpats) match_result
  = returnDs (ConPat con_id pat_ty pats, match_result)
  where
    pats 	     = map mk_pat tagged_arg_tys

	-- Boring stuff to find the arg-tys of the constructor
    (_, inst_tys, _) = splitAlgTyConApp pat_ty
    con_arg_tys'     = dataConArgTys con_id inst_tys 
    tagged_arg_tys   = con_arg_tys' `zip` (dataConFieldLabels con_id)

	-- mk_pat picks a WildPat of the appropriate type for absent fields,
	-- and the specified pattern for present fields
    mk_pat (arg_ty, lbl) = case [pat | (sel_id,pat,_) <- rpats,
					recordSelectorFieldLabel sel_id == lbl
				] of
				(pat:pats) -> ASSERT( null pats )
					      pat
				[]	   -> WildPat arg_ty

tidy1 v (ListPat ty pats) match_result
  = returnDs (list_ConPat, match_result)
  where
    list_ty = mkListTy ty
    list_ConPat
      = foldr (\ x -> \y -> ConPat consDataCon list_ty [x, y])
	      (ConPat nilDataCon  list_ty [])
	      pats

tidy1 v (TuplePat pats) match_result
  = returnDs (tuple_ConPat, match_result)
  where
    arity = length pats
    tuple_ConPat
      = ConPat (tupleCon arity)
	       (mkTupleTy arity (map outPatType pats))
	       pats

tidy1 v (DictPat dicts methods) match_result
  = case num_of_d_and_ms of
	0 -> tidy1 v (TuplePat []) match_result
	1 -> tidy1 v (head dict_and_method_pats) match_result
	_ -> tidy1 v (TuplePat dict_and_method_pats) match_result
  where
    num_of_d_and_ms	 = length dicts + length methods
    dict_and_method_pats = map VarPat (dicts ++ methods)


-- deeply ugly mangling for some (common) NPats/LitPats

-- LitPats: the desugarer only sees these at well-known types

tidy1 v pat@(LitPat lit lit_ty) match_result
  | isUnpointedType lit_ty
  = returnDs (pat, match_result)

  | lit_ty == charTy
  = returnDs (ConPat charDataCon charTy [LitPat (mk_char lit) charPrimTy],
	      match_result)

  | otherwise = pprPanic "tidy1:LitPat:" (ppr pat)
  where
    mk_char (HsChar c)    = HsCharPrim c

-- NPats: we *might* be able to replace these w/ a simpler form


tidy1 v pat@(NPat lit lit_ty _) match_result
  = returnDs (better_pat, match_result)
  where
    better_pat
      | lit_ty == charTy   = ConPat charDataCon   lit_ty [LitPat (mk_char lit)   charPrimTy]
      | lit_ty == intTy    = ConPat intDataCon    lit_ty [LitPat (mk_int lit)    intPrimTy]
      | lit_ty == wordTy   = ConPat wordDataCon   lit_ty [LitPat (mk_word lit)   wordPrimTy]
      | lit_ty == addrTy   = ConPat addrDataCon   lit_ty [LitPat (mk_addr lit)   addrPrimTy]
      | lit_ty == floatTy  = ConPat floatDataCon  lit_ty [LitPat (mk_float lit)  floatPrimTy]
      | lit_ty == doubleTy = ConPat doubleDataCon lit_ty [LitPat (mk_double lit) doublePrimTy]

		-- Convert the literal pattern "" to the constructor pattern [].
      | null_str_lit lit       = ConPat nilDataCon    lit_ty [] 

      | otherwise	   = pat

    mk_int    (HsInt i)      = HsIntPrim i
    mk_int    l@(HsLitLit s) = l

    mk_char   (HsChar c)     = HsCharPrim c
    mk_char   l@(HsLitLit s) = l

    mk_word   l@(HsLitLit s) = l

    mk_addr   l@(HsLitLit s) = l

    mk_float  (HsInt i)      = HsFloatPrim (fromInteger i)
    mk_float  (HsFrac f)     = HsFloatPrim f
    mk_float  l@(HsLitLit s) = l

    mk_double (HsInt i)      = HsDoublePrim (fromInteger i)
    mk_double (HsFrac f)     = HsDoublePrim f
    mk_double l@(HsLitLit s) = l

    null_str_lit (HsString s) = _NULL_ s
    null_str_lit other_lit    = False

-- and everything else goes through unchanged...

tidy1 v non_interesting_pat match_result
  = returnDs (non_interesting_pat, match_result)
\end{code}

PREVIOUS matchTwiddled STUFF:

Now we get to the only interesting part; note: there are choices for
translation [from Simon's notes]; translation~1:
\begin{verbatim}
deTwiddle [s,t] e
\end{verbatim}
returns
\begin{verbatim}
[ w = e,
  s = case w of [s,t] -> s
  t = case w of [s,t] -> t
]
\end{verbatim}

Here \tr{w} is a fresh variable, and the \tr{w}-binding prevents multiple
evaluation of \tr{e}.  An alternative translation (No.~2):
\begin{verbatim}
[ w = case e of [s,t] -> (s,t)
  s = case w of (s,t) -> s
  t = case w of (s,t) -> t
]
\end{verbatim}

%************************************************************************
%*									*
\subsubsection[improved-unmixing]{UNIMPLEMENTED idea for improved unmixing}
%*									*
%************************************************************************

We might be able to optimise unmixing when confronted by
only-one-constructor-possible, of which tuples are the most notable
examples.  Consider:
\begin{verbatim}
f (a,b,c) ... = ...
f d ... (e:f) = ...
f (g,h,i) ... = ...
f j ...       = ...
\end{verbatim}
This definition would normally be unmixed into four equation blocks,
one per equation.  But it could be unmixed into just one equation
block, because if the one equation matches (on the first column),
the others certainly will.

You have to be careful, though; the example
\begin{verbatim}
f j ...       = ...
-------------------
f (a,b,c) ... = ...
f d ... (e:f) = ...
f (g,h,i) ... = ...
\end{verbatim}
{\em must} be broken into two blocks at the line shown; otherwise, you
are forcing unnecessary evaluation.  In any case, the top-left pattern
always gives the cue.  You could then unmix blocks into groups of...
\begin{description}
\item[all variables:]
As it is now.
\item[constructors or variables (mixed):]
Need to make sure the right names get bound for the variable patterns.
\item[literals or variables (mixed):]
Presumably just a variant on the constructor case (as it is now).
\end{description}

%************************************************************************
%*									*
%* match on an unmixed block: the real business				*
%*									*
%************************************************************************
\subsection[matchUnmixedEqns]{@matchUnmixedEqns@: getting down to business}

The function @matchUnmixedEqns@ is where the matching stuff sets to
work a block of equations, to which the mixture rule has been applied.
Its arguments and results are the same as for the ``top-level'' @match@.

\begin{code}
matchUnmixedEqns :: [Id]
		  -> [EquationInfo]
		  -> DsM MatchResult

matchUnmixedEqns [] _ = panic "matchUnmixedEqns: no names"

matchUnmixedEqns all_vars@(var:vars) eqns_info 
  | irrefutablePat first_pat
  = ASSERT( irrefutablePats column_1_pats )	-- Sanity check
  	-- Real true variables, just like in matchVar, SLPJ p 94
    match vars remaining_eqns_info

  | isConPat first_pat
  = ASSERT( patsAreAllCons column_1_pats )
    matchConFamily all_vars eqns_info 

  | isLitPat first_pat
  = ASSERT( patsAreAllLits column_1_pats )
  	-- see notes in MatchLiteral
	-- not worried about the same literal more than once in a column
	-- (ToDo: sort this out later)
    matchLiterals all_vars eqns_info

  where
    first_pat		= head column_1_pats
    column_1_pats 	= [pat                       | EqnInfo _ _ (pat:_)  _            <- eqns_info]
    remaining_eqns_info = [EqnInfo n ctx pats match_result | EqnInfo n ctx (_:pats) match_result <- eqns_info]
\end{code}

%************************************************************************
%*									*
%*  matchWrapper: a convenient way to call @match@			*
%*									*
%************************************************************************
\subsection[matchWrapper]{@matchWrapper@: a convenient interface to @match@}

Calls to @match@ often involve similar (non-trivial) work; that work
is collected here, in @matchWrapper@.  This function takes as
arguments:
\begin{itemize}
\item
Typchecked @Matches@ (of a function definition, or a case or lambda
expression)---the main input;
\item
An error message to be inserted into any (runtime) pattern-matching
failure messages.
\end{itemize}

As results, @matchWrapper@ produces:
\begin{itemize}
\item
A list of variables (@Locals@) that the caller must ``promise'' to
bind to appropriate values; and
\item
a @CoreExpr@, the desugared output (main result).
\end{itemize}

The main actions of @matchWrapper@ include:
\begin{enumerate}
\item
Flatten the @[TypecheckedMatch]@ into a suitable list of
@EquationInfo@s.
\item
Create as many new variables as there are patterns in a pattern-list
(in any one of the @EquationInfo@s).
\item
Create a suitable ``if it fails'' expression---a call to @error@ using
the error-string input; the {\em type} of this fail value can be found
by examining one of the RHS expressions in one of the @EquationInfo@s.
\item
Call @match@ with all of this information!
\end{enumerate}

\begin{code}
matchWrapper :: DsMatchKind			-- For shadowing warning messages
	     -> [TypecheckedMatch]		-- Matches being desugared
	     -> String 				-- Error message if the match fails
	     -> DsM ([Id], CoreExpr) 	-- Results
\end{code}

 a special case for the common ...:
	just one Match
	lots of (all?) unfailable pats
  e.g.,
	f x y z = ....
 
 This special case have been ``undone'' due to problems with the new warnings 
 messages (Check.lhs.check). We need there the name of the variables to be able to 
 print later the equation. JJQC 30-11-97

\begin{old_code}
matchWrapper kind [(PatMatch (VarPat var) match)] error_string
  = matchWrapper kind [match] error_string `thenDs` \ (vars, core_expr) ->
    returnDs (var:vars, core_expr)

matchWrapper kind [(PatMatch (WildPat ty) match)] error_string
  = newSysLocalDs ty		           `thenDs` \ var ->
    matchWrapper kind [match] error_string `thenDs` \ (vars, core_expr) ->
    returnDs (var:vars, core_expr)

matchWrapper kind [(GRHSMatch
		     (GRHSsAndBindsOut [GRHS [] expr _] binds _))] error_string
  = dsBinds False{-don't auto-scc-} binds            `thenDs` \ core_binds ->
    dsExpr  expr	                             `thenDs` \ core_expr ->
    returnDs ([], mkCoLetsAny core_binds core_expr)
\end{old_code}

 And all the rest... (general case)


 There is one small problem with the Lambda Patterns, when somebody
 writes something similar to:
    (\ (x:xs) -> ...)
 he/she don't want a warning about incomplete patterns, that is done with 
 the flag opt_WarnSimplePatterns.
 This problem also appears in the :
   do patterns, but if the do can fail it creates another equation if the match can 
                fail (see DsExpr.doDo function)
   let patterns, are treated by matchSimply
   List Comprension Patterns, are treated by matchSimply also

We can't call matchSimply with Lambda patterns, due to lambda patterns can have more than
one pattern, and match simply only accepts one pattern.

JJQC 30-Nov-1997
 
\begin{code}

matchWrapper kind matches error_string
  = flattenMatches kind 1 matches	`thenDs` \ eqns_info@(EqnInfo _ _ arg_pats (MatchResult _ result_ty _) : _) ->

    selectMatchVars arg_pats				`thenDs` \ new_vars ->
    match_fun new_vars eqns_info 		 	`thenDs` \ match_result ->

    mkErrorAppDs pAT_ERROR_ID result_ty error_string	`thenDs` \ fail_expr ->

    extractMatchResult match_result fail_expr		`thenDs` \ result_expr ->
    returnDs (new_vars, result_expr)
  where match_fun = case kind of 
                      LambdaMatch | opt_WarnSimplePatterns -> matchExport 
                                  | otherwise              -> match
                      _                                    -> matchExport
\end{code}

%************************************************************************
%*									*
\subsection[matchSimply]{@matchSimply@: match a single expression against a single pattern}
%*									*
%************************************************************************

@mkSimpleMatch@ is a wrapper for @match@ which deals with the
situation where we want to match a single expression against a single
pattern. It returns an expression.

\begin{code}
matchSimply :: CoreExpr			-- Scrutinee
	    -> DsMatchKind              -- Match kind
	    -> TypecheckedPat		-- Pattern it should match
	    -> Type			-- Type of result
	    -> CoreExpr			-- Return this if it matches
	    -> CoreExpr			-- Return this if it does
	    -> DsM CoreExpr

matchSimply (Var var) kind pat result_ty result_expr fail_expr
  = getSrcLocDs				        `thenDs` \ locn ->
    let
      ctx = DsMatchContext kind [pat] locn
      eqn_info = EqnInfo 1 ctx [pat] initial_match_result
    in 
      match_fun [var] [eqn_info]	        `thenDs` \ match_result ->
      extractMatchResult match_result fail_expr
  where
    initial_match_result = MatchResult CantFail result_ty (\ ignore -> result_expr)
    match_fun = if opt_WarnSimplePatterns 
                  then matchExport
                  else match

matchSimply scrut_expr kind pat result_ty result_expr msg
  = newSysLocalDs (outPatType pat) 	        			`thenDs` \ scrut_var ->
    matchSimply (Var scrut_var) kind pat result_ty result_expr msg	`thenDs` \ expr ->
    returnDs (Let (NonRec scrut_var scrut_expr) expr)


extractMatchResult (MatchResult CantFail _ match_fn) fail_expr
  = returnDs (match_fn (error "It can't fail!"))

extractMatchResult (MatchResult CanFail result_ty match_fn) fail_expr
  = mkFailurePair result_ty	 	`thenDs` \ (fail_bind_fn, if_it_fails) ->
    returnDs (Let (fail_bind_fn fail_expr) (match_fn if_it_fails))
\end{code}

%************************************************************************
%*									*
%*  flattenMatches : create a list of EquationInfo			*
%*									*
%************************************************************************
\subsection[flattenMatches]{@flattenMatches@: create @[EquationInfo]@}

This is actually local to @matchWrapper@.

\begin{code}
flattenMatches
	:: DsMatchKind
        -> EqnNo
	-> [TypecheckedMatch]
	-> DsM [EquationInfo]

flattenMatches kind n [] = returnDs []

flattenMatches kind n (match : matches)
  = flatten_match [] n match	`thenDs` \ eqn_info ->
    flattenMatches kind (n+1) matches	`thenDs` \ eqn_infos ->
    returnDs (eqn_info : eqn_infos)
  where
    flatten_match :: [TypecheckedPat] 		-- Reversed list of patterns encountered so far
                  -> EqnNo
		  -> TypecheckedMatch
		  -> DsM EquationInfo

    flatten_match pats_so_far n (PatMatch pat match)
      = flatten_match (pat:pats_so_far) n match

    flatten_match pats_so_far n (GRHSMatch (GRHSsAndBindsOut grhss binds ty))
      = dsBinds False{-don't auto-scc-} binds	`thenDs` \ core_binds ->
	dsGRHSs ty kind pats grhss 		`thenDs` \ match_result ->
        getSrcLocDs				`thenDs` \ locn ->
	returnDs (EqnInfo n (DsMatchContext kind pats locn) pats 
                  (mkCoLetsMatchResult core_binds match_result))
      where
	pats = reverse pats_so_far	-- They've accumulated in reverse order

    flatten_match pats_so_far n (SimpleMatch expr) 
      = dsExpr expr		`thenDs` \ core_expr ->
	getSrcLocDs		`thenDs` \ locn ->
	returnDs (EqnInfo n (DsMatchContext kind pats locn) pats
		    (MatchResult CantFail (coreExprType core_expr) 
			      (\ ignore -> core_expr)))

	 -- the matching can't fail, so we won't generate an error message.
        where
	 pats = reverse pats_so_far	-- They've accumulated in reverse order

\end{code}

