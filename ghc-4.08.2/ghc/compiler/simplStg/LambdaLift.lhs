%
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section[LambdaLift]{A STG-code lambda lifter}

\begin{code}
module LambdaLift ( liftProgram ) where

#include "HsVersions.h"

import StgSyn

import Bag		( Bag, emptyBag, unionBags, unitBag, snocBag, bagToList )
import Id		( mkVanillaId, idType, setIdArityInfo, Id )
import VarSet
import VarEnv
import IdInfo		( exactArity )
import Module		( Module )
import Name             ( mkTopName )
import Type		( splitForAllTys, mkForAllTys, mkFunTys, Type )
import UniqSupply	( uniqFromSupply, splitUniqSupply, UniqSupply )
import Util		( zipEqual )
import Panic		( panic, assertPanic )
\end{code}

This is the lambda lifter.  It turns lambda abstractions into
supercombinators on a selective basis:

* Let-no-escaped bindings are never lifted. That's one major reason
  why the lambda lifter is done in STG.

* Non-recursive bindings whose RHS is a lambda abstractions are lifted,
  provided all the occurrences of the bound variable is in a function
  postition.  In this example, f will be lifted:

	let
	  f = \x -> e
	in
	..(f a1)...(f a2)...
  thus

    $f p q r x = e	-- Supercombinator

	..($f p q r a1)...($f p q r a2)...

  NOTE that the original binding is eliminated.

  But in this case, f won't be lifted:

	let
	  f = \x -> e
	in
	..(g f)...(f a2)...

  Why? Because we have to heap-allocate a closure for f thus:

    $f p q r x = e	-- Supercombinator

	let
	  f = $f p q r
	in
	..(g f)...($f p q r a2)..

  so it might as well be the original lambda abstraction.

  We also do not lift if the function has an occurrence with no arguments, e.g.

	let
	  f = \x -> e
	in f

  as this form is more efficient than if we create a partial application

  $f p q r x = e      -- Supercombinator

	f p q r

* Recursive bindings *all* of whose RHSs are lambda abstractions are
  lifted iff
	- all the occurrences of all the binders are in a function position
	- there aren't ``too many'' free variables.

  Same reasoning as before for the function-position stuff.  The ``too many
  free variable'' part comes from considering the (potentially many)
  recursive calls, which may now have lots of free vars.

Recent Observations:

* 2 might be already ``too many'' variables to abstract.
  The problem is that the increase in the number of free variables
  of closures refering to the lifted function (which is always # of
  abstracted args - 1) may increase heap allocation a lot.
  Expeiments are being done to check this...

* We do not lambda lift if the function has at least one occurrence
  without any arguments. This caused lots of problems. Ex:
  h = \ x -> ... let y = ...
		 in let let f = \x -> ...y...
		    in f
  ==>
  f = \y x -> ...y...
  h = \ x -> ... let y = ...
		 in f y

  now f y is a partial application, so it will be updated, and this
  is Bad.


--- NOT RELEVANT FOR STG ----
* All ``lone'' lambda abstractions are lifted.  Notably this means lambda
  abstractions:
	- in a case alternative: case e of True -> (\x->b)
	- in the body of a let:  let x=e in (\y->b)
-----------------------------

%************************************************************************
%*									*
\subsection[Lift-expressions]{The main function: liftExpr}
%*									*
%************************************************************************

\begin{code}
liftProgram :: Module -> UniqSupply -> [StgBinding] -> [StgBinding]
liftProgram mod us prog = concat (runLM mod Nothing us (mapLM liftTopBind prog))


liftTopBind :: StgBinding -> LiftM [StgBinding]
liftTopBind (StgNonRec id rhs)
  = dontLiftRhs rhs		`thenLM` \ (rhs', rhs_info) ->
    returnLM (getScBinds rhs_info ++ [StgNonRec id rhs'])

liftTopBind (StgRec pairs)
  = mapAndUnzipLM dontLiftRhs rhss	`thenLM` \ (rhss', rhs_infos) ->
    returnLM ([co_rec_ify (StgRec (ids `zip` rhss') :
			   getScBinds (unionLiftInfos rhs_infos))
	     ])
  where
   (ids, rhss) = unzip pairs
\end{code}


\begin{code}
liftExpr :: StgExpr
	 -> LiftM (StgExpr, LiftInfo)


liftExpr expr@(StgLit _) 	 = returnLM (expr, emptyLiftInfo)
liftExpr expr@(StgConApp _ _)	 = returnLM (expr, emptyLiftInfo)
liftExpr expr@(StgPrimApp _ _ _) = returnLM (expr, emptyLiftInfo)

liftExpr expr@(StgApp v args)
  = lookUp v		`thenLM` \ ~(sc, sc_args) ->	-- NB the ~.  We don't want to
							-- poke these bindings too early!
    returnLM (StgApp sc (map StgVarArg sc_args ++ args),
	      emptyLiftInfo)
	-- The lvs field is probably wrong, but we reconstruct it
	-- anyway following lambda lifting

liftExpr (StgCase scrut lv1 lv2 bndr srt alts)
  = liftExpr scrut	`thenLM` \ (scrut', scrut_info) ->
    lift_alts alts	`thenLM` \ (alts', alts_info) ->
    returnLM (StgCase scrut' lv1 lv2 bndr srt alts', scrut_info `unionLiftInfo` alts_info)
  where
    lift_alts (StgAlgAlts ty alg_alts deflt)
	= mapAndUnzipLM lift_alg_alt alg_alts	`thenLM` \ (alg_alts', alt_infos) ->
	  lift_deflt deflt			`thenLM` \ (deflt', deflt_info) ->
	  returnLM (StgAlgAlts ty alg_alts' deflt', foldr unionLiftInfo deflt_info alt_infos)

    lift_alts (StgPrimAlts ty prim_alts deflt)
	= mapAndUnzipLM lift_prim_alt prim_alts	`thenLM` \ (prim_alts', alt_infos) ->
	  lift_deflt deflt			`thenLM` \ (deflt', deflt_info) ->
	  returnLM (StgPrimAlts ty prim_alts' deflt', foldr unionLiftInfo deflt_info alt_infos)

    lift_alg_alt (con, args, use_mask, rhs)
	= liftExpr rhs		`thenLM` \ (rhs', rhs_info) ->
	  returnLM ((con, args, use_mask, rhs'), rhs_info)

    lift_prim_alt (lit, rhs)
	= liftExpr rhs	`thenLM` \ (rhs', rhs_info) ->
	  returnLM ((lit, rhs'), rhs_info)

    lift_deflt StgNoDefault = returnLM (StgNoDefault, emptyLiftInfo)
    lift_deflt (StgBindDefault rhs)
      	= liftExpr rhs	`thenLM` \ (rhs', rhs_info) ->
	  returnLM (StgBindDefault rhs', rhs_info)
\end{code}

Now the interesting cases.  Let no escape isn't lifted.  We turn it
back into a let, to play safe, because we have to redo that pass after
lambda anyway.

\begin{code}
liftExpr (StgLetNoEscape _ _ (StgNonRec binder rhs) body)
  = dontLiftRhs rhs	`thenLM` \ (rhs', rhs_info) ->
    liftExpr body	`thenLM` \ (body', body_info) ->
    returnLM (StgLet (StgNonRec binder rhs') body',
	      rhs_info `unionLiftInfo` body_info)

liftExpr (StgLetNoEscape _ _ (StgRec pairs) body)
  = liftExpr body			`thenLM` \ (body', body_info) ->
    mapAndUnzipLM dontLiftRhs rhss	`thenLM` \ (rhss', rhs_infos) ->
    returnLM (StgLet (StgRec (zipEqual "liftExpr" binders rhss')) body',
	      foldr unionLiftInfo body_info rhs_infos)
  where
   (binders,rhss) = unzip pairs
\end{code}

\begin{code}
liftExpr (StgLet (StgNonRec binder rhs) body)
  | not (isLiftable rhs)
  = dontLiftRhs rhs	`thenLM` \ (rhs', rhs_info) ->
    liftExpr body	`thenLM` \ (body', body_info) ->
    returnLM (StgLet (StgNonRec binder rhs') body',
	      rhs_info `unionLiftInfo` body_info)

  | otherwise	-- It's a lambda
  = 	-- Do the body of the let
    fixLM (\ ~(sc_inline, _, _) ->
      addScInlines [binder] [sc_inline]	(
	liftExpr body
      )			`thenLM` \ (body', body_info) ->

	-- Deal with the RHS
      dontLiftRhs rhs		`thenLM` \ (rhs', rhs_info) ->

	-- All occurrences in function position, so lambda lift
      getFinalFreeVars (rhsFreeVars rhs)    `thenLM` \ final_free_vars ->

      mkScPieces final_free_vars (binder,rhs')	`thenLM` \ (sc_inline, sc_bind) ->

      returnLM (sc_inline,
		body',
		nonRecScBind rhs_info sc_bind `unionLiftInfo` body_info)

    )			`thenLM` \ (_, expr', final_info) ->

    returnLM (expr', final_info)

liftExpr (StgLet (StgRec pairs) body)
--[Andre-testing]
  | not (all isLiftableRec rhss)
  = liftExpr body			`thenLM` \ (body', body_info) ->
    mapAndUnzipLM dontLiftRhs rhss	`thenLM` \ (rhss', rhs_infos) ->
    returnLM (StgLet (StgRec (zipEqual "liftExpr2" binders rhss')) body',
	      foldr unionLiftInfo body_info rhs_infos)

  | otherwise	-- All rhss are liftable
  = -- Do the body of the let
    fixLM (\ ~(sc_inlines, _, _) ->
      addScInlines binders sc_inlines	(

      liftExpr body			`thenLM` \ (body', body_info) ->
      mapAndUnzipLM dontLiftRhs rhss	`thenLM` \ (rhss', rhs_infos) ->
      let
	-- Find the free vars of all the rhss,
	-- excluding the binders themselves.
	rhs_free_vars = unionVarSets (map rhsFreeVars rhss)
			`minusVarSet`
			mkVarSet binders

	rhs_info      = unionLiftInfos rhs_infos
      in
      getFinalFreeVars rhs_free_vars	`thenLM` \ final_free_vars ->

      mapAndUnzipLM (mkScPieces final_free_vars) (binders `zip` rhss')
	     				`thenLM` \ (sc_inlines, sc_pairs) ->
      returnLM (sc_inlines,
		body',
		recScBind rhs_info sc_pairs `unionLiftInfo` body_info)

    ))			`thenLM` \ (_, expr', final_info) ->

    returnLM (expr', final_info)
  where
    (binders,rhss)    = unzip pairs
\end{code}

\begin{code}
liftExpr (StgSCC cc expr)
  = liftExpr expr `thenLM` \ (expr2, expr_info) ->
    returnLM (StgSCC cc expr2, expr_info)
\end{code}

A binding is liftable if it's a *function* (args not null) and never
occurs in an argument position.

\begin{code}
isLiftable :: StgRhs -> Bool

isLiftable (StgRhsClosure _ (StgBinderInfo arg_occ _ _ _ unapplied_occ) _ fvs _ args _)

  -- Experimental evidence suggests we should lift only if we will be
  -- abstracting up to 4 fvs.

  = if not (null args	|| 	-- Not a function
	 unapplied_occ	|| 	-- Has an occ with no args at all
	 arg_occ	|| 	-- Occurs in arg position
	 length fvs > 4 	-- Too many free variables
	)
    then {-trace ("LL: " ++ show (length fvs))-} True
    else False
isLiftable other_rhs = False

isLiftableRec :: StgRhs -> Bool

-- this is just the same as for non-rec, except we only lift to
-- abstract up to 1 argument this avoids undoing Static Argument
-- Transformation work

{- Andre's longer comment about isLiftableRec: 1996/01:

A rec binding is "liftable" (according to our heuristics) if:
* It is a function,
* all occurrences have arguments,
* does not occur in an argument position and
* has up to *2* free variables (including the rec binding variable
  itself!)

The point is: my experiments show that SAT is more important than LL.
Therefore if we still want to do LL, for *recursive* functions, we do
not want LL to undo what SAT did.  We do this by avoiding LL recursive
functions that have more than 2 fvs, since if this recursive function
was created by SAT (we don't know!), it would have at least 3 fvs: one
for the rec binding itself and 2 more for the static arguments (note:
this matches with the choice of performing SAT to have at least 2
static arguments, if we change things there we should change things
here).
-}

isLiftableRec (StgRhsClosure _ (StgBinderInfo arg_occ _ _ _ unapplied_occ) _ fvs _ args _)
  = if not (null args	|| 	-- Not a function
	 unapplied_occ	|| 	-- Has an occ with no args at all
	 arg_occ	|| 	-- Occurs in arg position
	 length fvs > 2 	-- Too many free variables
	)
    then {-trace ("LLRec: " ++ show (length fvs))-} True
    else False
isLiftableRec other_rhs = False

rhsFreeVars :: StgRhs -> IdSet
rhsFreeVars (StgRhsClosure _ _ _ fvs _ _ _) = mkVarSet fvs
rhsFreeVars other 			  = panic "rhsFreeVars"
\end{code}

dontLiftRhs is like liftExpr, except that it does not lift a top-level
lambda abstraction.  It is used for the right-hand sides of
definitions where we've decided *not* to lift: for example, top-level
ones or mutually-recursive ones where not all are lambdas.

\begin{code}
dontLiftRhs :: StgRhs -> LiftM (StgRhs, LiftInfo)

dontLiftRhs rhs@(StgRhsCon cc v args) = returnLM (rhs, emptyLiftInfo)

dontLiftRhs (StgRhsClosure cc bi srt fvs upd args body)
  = liftExpr body	`thenLM` \ (body', body_info) ->
    returnLM (StgRhsClosure cc bi srt fvs upd args body', body_info)
\end{code}

\begin{code}
mkScPieces :: IdSet 		-- Extra args for the supercombinator
	   -> (Id, StgRhs)	-- The processed RHS and original Id
	   -> LiftM ((Id,[Id]), 	-- Replace abstraction with this;
						-- the set is its free vars
		     (Id,StgRhs))	-- Binding for supercombinator

mkScPieces extra_arg_set (id, StgRhsClosure cc bi srt _ upd args body)
  = ASSERT( n_args > 0 )
	-- Construct the rhs of the supercombinator, and its Id
    newSupercombinator sc_ty arity  `thenLM` \ sc_id ->
    returnLM ((sc_id, extra_args), (sc_id, sc_rhs))
  where
    n_args     = length args
    extra_args = varSetElems extra_arg_set
    arity      = n_args + length extra_args

	-- Construct the supercombinator type
    type_of_original_id = idType id
    extra_arg_tys       = map idType extra_args
    (tyvars, rest)      = splitForAllTys type_of_original_id
    sc_ty 	        = mkForAllTys tyvars (mkFunTys extra_arg_tys rest)

    sc_rhs = StgRhsClosure cc bi srt [] upd (extra_args ++ args) body
\end{code}


%************************************************************************
%*									*
\subsection[Lift-monad]{The LiftM monad}
%*									*
%************************************************************************

The monad is used only to distribute global stuff, and the unique supply.

\begin{code}
type LiftM a =  Module 
	     -> LiftFlags
	     -> UniqSupply
	     -> (IdEnv 				-- Domain = candidates for lifting
		       (Id,			-- The supercombinator
			[Id])			-- Args to apply it to
		 )
	     -> a


type LiftFlags = Maybe Int	-- No of fvs reqd to float recursive
				-- binding; Nothing == infinity


runLM :: Module -> LiftFlags -> UniqSupply -> LiftM a -> a
runLM mod flags us m = m mod flags us emptyVarEnv

thenLM :: LiftM a -> (a -> LiftM b) -> LiftM b
thenLM m k mod ci us idenv
  = k (m mod ci us1 idenv) mod ci us2 idenv
  where
    (us1, us2) = splitUniqSupply us

returnLM :: a -> LiftM a
returnLM a mod ci us idenv = a

fixLM :: (a -> LiftM a) -> LiftM a
fixLM k mod ci us idenv = r
		       where
			 r = k r mod ci us idenv

mapLM :: (a -> LiftM b) -> [a] -> LiftM [b]
mapLM f [] = returnLM []
mapLM f (a:as) = f a		`thenLM` \ r ->
		 mapLM f as	`thenLM` \ rs ->
		 returnLM (r:rs)

mapAndUnzipLM :: (a -> LiftM (b,c)) -> [a] -> LiftM ([b],[c])
mapAndUnzipLM f []     = returnLM ([],[])
mapAndUnzipLM f (a:as) = f a			`thenLM` \ (b,c) ->
			 mapAndUnzipLM f as 	`thenLM` \ (bs,cs) ->
			 returnLM (b:bs, c:cs)
\end{code}

\begin{code}
newSupercombinator :: Type
		   -> Int		-- Arity
		   -> LiftM Id

newSupercombinator ty arity mod ci us idenv
  = mkVanillaId (mkTopName uniq mod SLIT("_ll")) ty
    `setIdArityInfo` exactArity arity
	-- ToDo: rm the setIdArity?  Just let subsequent stg-saturation pass do it?
  where
    uniq = uniqFromSupply us

lookUp :: Id -> LiftM (Id,[Id])
lookUp v mod ci us idenv
  = case (lookupVarEnv idenv v) of
      Just result -> result
      Nothing     -> (v, [])

addScInlines :: [Id] -> [(Id,[Id])] -> LiftM a -> LiftM a
addScInlines ids values m mod ci us idenv
  = m mod ci us idenv'
  where
    idenv' = extendVarEnvList idenv (ids `zip_lazy` values)

    -- zip_lazy zips two things together but matches lazily on the
    -- second argument.  This is important, because the ids are know here,
    -- but the things they are bound to are decided only later
    zip_lazy [] _           = []
    zip_lazy (x:xs) ~(y:ys) = (x,y) : zip_lazy xs ys


-- The free vars reported by the free-var analyser will include
-- some ids, f, which are to be replaced by ($f a b c), where $f
-- is the supercombinator.  Hence instead of f being a free var,
-- {a,b,c} are.
--
-- Example
--	let
--	   f a = ...y1..y2.....
--	in
--	let
--	   g b = ...f...z...
--	in
--	...
--
--  Here the free vars of g are {f,z}; but f will be lambda-lifted
--  with free vars {y1,y2}, so the "real~ free vars of g are {y1,y2,z}.

getFinalFreeVars :: IdSet -> LiftM IdSet

getFinalFreeVars free_vars mod ci us idenv
  = unionVarSets (map munge_it (varSetElems free_vars))
  where
    munge_it :: Id -> IdSet	-- Takes a free var and maps it to the "real"
				-- free var
    munge_it id = case (lookupVarEnv idenv id) of
		    Just (_, args) -> mkVarSet args
		    Nothing	   -> unitVarSet id
\end{code}


%************************************************************************
%*									*
\subsection[Lift-info]{The LiftInfo type}
%*									*
%************************************************************************

\begin{code}
type LiftInfo = Bag StgBinding	-- Float to top

emptyLiftInfo = emptyBag

unionLiftInfo :: LiftInfo -> LiftInfo -> LiftInfo
unionLiftInfo binds1 binds2 = binds1 `unionBags` binds2

unionLiftInfos :: [LiftInfo] -> LiftInfo
unionLiftInfos infos = foldr unionLiftInfo emptyLiftInfo infos

mkScInfo :: StgBinding -> LiftInfo
mkScInfo bind = unitBag bind

nonRecScBind :: LiftInfo 		-- From body of supercombinator
	     -> (Id, StgRhs)	-- Supercombinator and its rhs
	     -> LiftInfo
nonRecScBind binds (sc_id,sc_rhs) = binds `snocBag` (StgNonRec sc_id sc_rhs)


-- In the recursive case, all the SCs from the RHSs of the recursive group
-- are dealing with might potentially mention the new, recursive SCs.
-- So we flatten the whole lot into a single recursive group.

recScBind :: LiftInfo			-- From body of supercombinator
	   -> [(Id,StgRhs)]	-- Supercombinator rhs
	   -> LiftInfo

recScBind binds pairs = unitBag (co_rec_ify (StgRec pairs : bagToList binds))

co_rec_ify :: [StgBinding] -> StgBinding
co_rec_ify binds = StgRec (concat (map f binds))
  where
    f (StgNonRec id rhs) = [(id,rhs)]
    f (StgRec pairs)     = pairs


getScBinds :: LiftInfo -> [StgBinding]
getScBinds binds = bagToList binds

looksLikeSATRhs [(f,StgRhsClosure _ _ _ _ _ ls _)] (StgApp f' args)
  = (f == f') && (length args == length ls)
looksLikeSATRhs _ _ = False
\end{code}
