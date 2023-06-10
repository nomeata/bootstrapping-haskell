%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcDeriv]{Deriving}

Handles @deriving@ clauses on @data@ declarations.

\begin{code}
module TcDeriv ( tcDeriving ) where

#include "HsVersions.h"

import HsSyn		( HsBinds(..), MonoBinds(..), collectMonoBinders )
import RdrHsSyn		( RdrNameMonoBinds )
import RnHsSyn		( RenamedHsBinds, RenamedMonoBinds )
import CmdLineOpts	( opt_D_dump_deriv )

import TcMonad
import Inst		( InstanceMapper )
import TcEnv		( getEnvTyCons )
import TcGenDeriv	-- Deriv stuff
import TcInstUtil	( InstInfo(..), buildInstanceEnvs )
import TcSimplify	( tcSimplifyThetas )

import RnBinds		( rnMethodBinds, rnTopMonoBinds )
import RnEnv		( newDFunName, bindLocatedLocalsRn )
import RnMonad		( RnNameSupply, 
			  renameSourceCode, thenRn, mapRn, returnRn )

import Bag		( Bag, emptyBag, unionBags, listToBag )
import Class		( classKey, Class )
import ErrUtils		( dumpIfSet, Message, pprBagOfErrors )
import MkId		( mkDictFunId )
import Id		( mkVanillaId )
import DataCon		( dataConArgTys, isNullaryDataCon, isExistentialDataCon )
import PrelInfo		( needsDataDeclCtxtClassKeys )
import Maybes		( maybeToBool, catMaybes )
import Module		( ModuleName )
import Name		( isLocallyDefined, getSrcLoc,
			  Name, NamedThing(..),
			  OccName, nameOccName
			)
import RdrName		( RdrName )
import RnMonad		( Fixities )
import SrcLoc		( mkGeneratedSrcLoc, SrcLoc )
import TyCon		( tyConTyVars, tyConDataCons, tyConDerivings,
			  tyConTheta, maybeTyConSingleCon, isDataTyCon,
			  isEnumerationTyCon, isAlgTyCon, TyCon
			)
import Type		( TauType, mkTyVarTys, mkTyConApp,
			  mkSigmaTy, mkDictTy, isUnboxedType,
			  splitAlgTyConApp, classesToPreds
			)
import PprType		( {- instance Outputable Type -} )
import TysWiredIn	( voidTy )
import Var		( TyVar )
import Unique		-- Keys stuff
import Bag		( bagToList )
import Util		( zipWithEqual, sortLt, removeDups,  assoc, thenCmp )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[TcDeriv-intro]{Introduction to how we do deriving}
%*									*
%************************************************************************

Consider

	data T a b = C1 (Foo a) (Bar b)
		   | C2 Int (T b a)
		   | C3 (T a a)
		   deriving (Eq)

[NOTE: See end of these comments for what to do with 
	data (C a, D b) => T a b = ...
]

We want to come up with an instance declaration of the form

	instance (Ping a, Pong b, ...) => Eq (T a b) where
		x == y = ...

It is pretty easy, albeit tedious, to fill in the code "...".  The
trick is to figure out what the context for the instance decl is,
namely @Ping@, @Pong@ and friends.

Let's call the context reqd for the T instance of class C at types
(a,b, ...)  C (T a b).  Thus:

	Eq (T a b) = (Ping a, Pong b, ...)

Now we can get a (recursive) equation from the @data@ decl:

	Eq (T a b) = Eq (Foo a) u Eq (Bar b)	-- From C1
		   u Eq (T b a) u Eq Int	-- From C2
		   u Eq (T a a)			-- From C3

Foo and Bar may have explicit instances for @Eq@, in which case we can
just substitute for them.  Alternatively, either or both may have
their @Eq@ instances given by @deriving@ clauses, in which case they
form part of the system of equations.

Now all we need do is simplify and solve the equations, iterating to
find the least fixpoint.  Notice that the order of the arguments can
switch around, as here in the recursive calls to T.

Let's suppose Eq (Foo a) = Eq a, and Eq (Bar b) = Ping b.

We start with:

	Eq (T a b) = {}		-- The empty set

Next iteration:
	Eq (T a b) = Eq (Foo a) u Eq (Bar b)	-- From C1
		   u Eq (T b a) u Eq Int	-- From C2
		   u Eq (T a a)			-- From C3

	After simplification:
		   = Eq a u Ping b u {} u {} u {}
		   = Eq a u Ping b

Next iteration:

	Eq (T a b) = Eq (Foo a) u Eq (Bar b)	-- From C1
		   u Eq (T b a) u Eq Int	-- From C2
		   u Eq (T a a)			-- From C3

	After simplification:
		   = Eq a u Ping b
		   u (Eq b u Ping a)
		   u (Eq a u Ping a)

		   = Eq a u Ping b u Eq b u Ping a

The next iteration gives the same result, so this is the fixpoint.  We
need to make a canonical form of the RHS to ensure convergence.  We do
this by simplifying the RHS to a form in which

	- the classes constrain only tyvars
	- the list is sorted by tyvar (major key) and then class (minor key)
	- no duplicates, of course

So, here are the synonyms for the ``equation'' structures:

\begin{code}
type DerivEqn = (Class, TyCon, [TyVar], DerivRhs)
			 -- The tyvars bind all the variables in the RHS
			 -- NEW: it's convenient to re-use InstInfo
			 -- We'll "panic" out some fields...

type DerivRhs = [(Class, [TauType])]	-- Same as a ThetaType!

type DerivSoln = DerivRhs
\end{code}


A note about contexts on data decls
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

	data (RealFloat a) => Complex a = !a :+ !a deriving( Read )

We will need an instance decl like:

	instance (Read a, RealFloat a) => Read (Complex a) where
	  ...

The RealFloat in the context is because the read method for Complex is bound
to construct a Complex, and doing that requires that the argument type is
in RealFloat. 

But this ain't true for Show, Eq, Ord, etc, since they don't construct
a Complex; they only take them apart.

Our approach: identify the offending classes, and add the data type
context to the instance decl.  The "offending classes" are

	Read, Enum?


%************************************************************************
%*									*
\subsection[TcDeriv-driver]{Top-level function for \tr{derivings}}
%*									*
%************************************************************************

\begin{code}
tcDeriving  :: ModuleName		-- name of module under scrutiny
	    -> Fixities			-- for the deriving code (Show/Read.)
	    -> RnNameSupply		-- for "renaming" bits of generated code
	    -> Bag InstInfo		-- What we already know about instances
	    -> TcM s (Bag InstInfo,	-- The generated "instance decls".
		      RenamedHsBinds)	-- Extra generated bindings

tcDeriving modname fixs rn_name_supply inst_decl_infos_in
  = recoverTc (returnTc (emptyBag, EmptyBinds)) $

  	-- Fish the "deriving"-related information out of the TcEnv
	-- and make the necessary "equations".
    makeDerivEqns			    	`thenTc` \ eqns ->
    if null eqns then
	returnTc (emptyBag, EmptyBinds)
    else

	-- Take the equation list and solve it, to deliver a list of
	-- solutions, a.k.a. the contexts for the instance decls
	-- required for the corresponding equations.
    solveDerivEqns inst_decl_infos_in eqns    	`thenTc` \ new_inst_infos ->

	-- Now augment the InstInfos, adding in the rather boring
	-- actual-code-to-do-the-methods binds.  We may also need to
	-- generate extra not-one-inst-decl-specific binds, notably
	-- "con2tag" and/or "tag2con" functions.  We do these
	-- separately.

    gen_taggery_Names new_inst_infos		`thenTc` \ nm_alist_etc ->


    let
	extra_mbind_list = map gen_tag_n_con_monobind nm_alist_etc
	extra_mbinds     = foldr AndMonoBinds EmptyMonoBinds extra_mbind_list
	method_binds_s   = map (gen_bind fixs) new_inst_infos
	mbinders	 = bagToList (collectMonoBinders extra_mbinds)
	
	-- Rename to get RenamedBinds.
	-- The only tricky bit is that the extra_binds must scope over the
	-- method bindings for the instances.
	(dfun_names_w_method_binds, rn_extra_binds)
		= renameSourceCode modname rn_name_supply (
			bindLocatedLocalsRn (ptext (SLIT("deriving"))) mbinders	$ \ _ ->
			rnTopMonoBinds extra_mbinds []		`thenRn` \ (rn_extra_binds, _) ->
			mapRn rn_one method_binds_s		`thenRn` \ dfun_names_w_method_binds ->
			returnRn (dfun_names_w_method_binds, rn_extra_binds)
		  )
	rn_one (cl_nm, tycon_nm, meth_binds) 
	        = newDFunName (cl_nm, tycon_nm)
		              mkGeneratedSrcLoc	        `thenRn` \ dfun_name ->
		  rnMethodBinds meth_binds		`thenRn` \ (rn_meth_binds, _) ->
		  returnRn (dfun_name, rn_meth_binds)

	really_new_inst_infos = zipWith gen_inst_info
			  	        new_inst_infos
					dfun_names_w_method_binds

	ddump_deriv = ddump_deriving really_new_inst_infos rn_extra_binds
    in
    ioToTc (dumpIfSet opt_D_dump_deriv "Derived instances" ddump_deriv)	`thenTc_`

    returnTc (listToBag really_new_inst_infos, rn_extra_binds)
  where
    ddump_deriving :: [InstInfo] -> RenamedHsBinds -> SDoc
    ddump_deriving inst_infos extra_binds
      = vcat (map pp_info inst_infos) $$ ppr extra_binds
      where
	pp_info (InstInfo clas tvs [ty] inst_decl_theta _ mbinds _ _)
	  = ppr (mkSigmaTy tvs inst_decl_theta' (mkDictTy clas [ty]))
	    $$
	    ppr mbinds
	    where inst_decl_theta' = classesToPreds inst_decl_theta
\end{code}


%************************************************************************
%*									*
\subsection[TcDeriv-eqns]{Forming the equations}
%*									*
%************************************************************************

@makeDerivEqns@ fishes around to find the info about needed derived
instances.  Complicating factors:
\begin{itemize}
\item
We can only derive @Enum@ if the data type is an enumeration
type (all nullary data constructors).

\item
We can only derive @Ix@ if the data type is an enumeration {\em
or} has just one data constructor (e.g., tuples).
\end{itemize}

[See Appendix~E in the Haskell~1.2 report.] This code here deals w/
all those.

\begin{code}
makeDerivEqns :: TcM s [DerivEqn]

makeDerivEqns
  = tcGetEnv			    `thenNF_Tc` \ env ->
    let
	local_data_tycons = filter (\tc -> isLocallyDefined tc && isAlgTyCon tc)
				   (getEnvTyCons env)

	think_about_deriving = need_deriving local_data_tycons
	(derive_these, _)    = removeDups cmp_deriv think_about_deriving
	eqns		     = map mk_eqn derive_these
    in
    if null local_data_tycons then
	returnTc []	-- Bale out now
    else
    mapTc mk_eqn derive_these `thenTc`	\ maybe_eqns ->
    returnTc (catMaybes maybe_eqns)
  where
    ------------------------------------------------------------------
    need_deriving :: [TyCon] -> [(Class, TyCon)]
	-- find the tycons that have `deriving' clauses;

    need_deriving tycons_to_consider
      = foldr (\ tycon acc -> [(clas,tycon) | clas <- tyConDerivings tycon] ++ acc)
	      []
	      tycons_to_consider

    ------------------------------------------------------------------
    cmp_deriv :: (Class, TyCon) -> (Class, TyCon) -> Ordering
    cmp_deriv (c1, t1) (c2, t2)
      = (c1 `compare` c2) `thenCmp` (t1 `compare` t2)

    ------------------------------------------------------------------
    mk_eqn :: (Class, TyCon) -> NF_TcM s (Maybe DerivEqn)
	-- we swizzle the tyvars and datacons out of the tycon
	-- to make the rest of the equation

    mk_eqn (clas, tycon)
      = case chk_out clas tycon of
	   Just err ->  addErrTc err	`thenNF_Tc_` 
			returnNF_Tc Nothing
	   Nothing  ->  returnNF_Tc (Just (clas, tycon, tyvars, constraints))
      where
	clas_key  = classKey clas
	tyvars    = tyConTyVars tycon	-- ToDo: Do we need new tyvars ???
	tyvar_tys = mkTyVarTys tyvars
	data_cons = tyConDataCons tycon

	constraints = extra_constraints ++ concat (map mk_constraints data_cons)

	-- "extra_constraints": see notes above about contexts on data decls
	extra_constraints
	  | offensive_class = tyConTheta tycon
	  | otherwise	    = []
	   where
	    offensive_class = clas_key `elem` needsDataDeclCtxtClassKeys

	mk_constraints data_con
	   = [ (clas, [arg_ty])
	     | arg_ty <- instd_arg_tys,
	       not (isUnboxedType arg_ty)	-- No constraints for unboxed types?
	     ]
	   where
	     instd_arg_tys  = dataConArgTys data_con tyvar_tys

    ------------------------------------------------------------------
    chk_out :: Class -> TyCon -> Maybe Message
    chk_out clas tycon
	| clas_key == enumClassKey    && not is_enumeration 	      = bog_out nullary_why
	| clas_key == boundedClassKey && not is_enumeration_or_single = bog_out single_nullary_why
	| clas_key == ixClassKey      && not is_enumeration_or_single = bog_out single_nullary_why
	| any isExistentialDataCon (tyConDataCons tycon)	      = Just (existentialErr clas tycon)
	| otherwise						      = Nothing
	where
	    clas_key = classKey clas

	    is_enumeration = isEnumerationTyCon tycon
	    is_single_con  = maybeToBool (maybeTyConSingleCon tycon)
	    is_enumeration_or_single = is_enumeration || is_single_con

	    single_nullary_why = SLIT("one constructor data type or type with all nullary constructors expected")
	    nullary_why        = SLIT("data type with all nullary constructors expected")

	    bog_out why = Just (derivingThingErr clas tycon why)
\end{code}

%************************************************************************
%*									*
\subsection[TcDeriv-fixpoint]{Finding the fixed point of \tr{deriving} equations}
%*									*
%************************************************************************

A ``solution'' (to one of the equations) is a list of (k,TyVarTy tv)
terms, which is the final correct RHS for the corresponding original
equation.
\begin{itemize}
\item
Each (k,TyVarTy tv) in a solution constrains only a type
variable, tv.

\item
The (k,TyVarTy tv) pairs in a solution are canonically
ordered by sorting on type varible, tv, (major key) and then class, k,
(minor key)
\end{itemize}

\begin{code}
solveDerivEqns :: Bag InstInfo
	       -> [DerivEqn]
	       -> TcM s [InstInfo]	-- Solns in same order as eqns.
				  	-- This bunch is Absolutely minimal...

solveDerivEqns inst_decl_infos_in orig_eqns
  = iterateDeriv initial_solutions
  where
	-- The initial solutions for the equations claim that each
	-- instance has an empty context; this solution is certainly
	-- in canonical form.
    initial_solutions :: [DerivSoln]
    initial_solutions = [ [] | _ <- orig_eqns ]

    ------------------------------------------------------------------
	-- iterateDeriv calculates the next batch of solutions,
	-- compares it with the current one; finishes if they are the
	-- same, otherwise recurses with the new solutions.
	-- It fails if any iteration fails
    iterateDeriv :: [DerivSoln] ->TcM s [InstInfo]
    iterateDeriv current_solns
      = checkNoErrsTc (iterateOnce current_solns)	`thenTc` \ (new_inst_infos, new_solns) ->
	if (current_solns == new_solns) then
	    returnTc new_inst_infos
	else
	    iterateDeriv new_solns

    ------------------------------------------------------------------
    iterateOnce current_solns
      =	    -- Extend the inst info from the explicit instance decls
	    -- with the current set of solutions, giving a

	add_solns inst_decl_infos_in orig_eqns current_solns
				`thenNF_Tc` \ (new_inst_infos, inst_mapper) ->
	let
	   class_to_inst_env cls = inst_mapper cls
	in
	    -- Simplify each RHS

	listTc [ tcAddErrCtxt (derivCtxt tc) $
		 tcSimplifyThetas class_to_inst_env deriv_rhs
	       | (_,tc,_,deriv_rhs) <- orig_eqns ]  `thenTc` \ next_solns ->

	    -- Canonicalise the solutions, so they compare nicely
	let canonicalised_next_solns
	      = [ sortLt (<) next_soln | next_soln <- next_solns ]
	in
	returnTc (new_inst_infos, canonicalised_next_solns)
\end{code}

\begin{code}
add_solns :: Bag InstInfo			-- The global, non-derived ones
	  -> [DerivEqn] -> [DerivSoln]
	  -> NF_TcM s ([InstInfo], 		-- The new, derived ones
		       InstanceMapper)
    -- the eqns and solns move "in lockstep"; we have the eqns
    -- because we need the LHS info for addClassInstance.

add_solns inst_infos_in eqns solns

  = discardErrsTc (buildInstanceEnvs all_inst_infos)	`thenNF_Tc` \ inst_mapper ->
	-- We do the discard-errs so that we don't get repeated error messages
	-- about duplicate instances.
	-- They'll appear later, when we do the top-level buildInstanceEnvs.

    returnNF_Tc (new_inst_infos, inst_mapper)
  where
    new_inst_infos = zipWithEqual "add_solns" mk_deriv_inst_info eqns solns

    all_inst_infos = inst_infos_in `unionBags` listToBag new_inst_infos

    mk_deriv_inst_info (clas, tycon, tyvars, _) theta
      = InstInfo clas tyvars [mkTyConApp tycon (mkTyVarTys tyvars)]
		 theta
		 dummy_dfun_id
		 (my_panic "binds") (getSrcLoc tycon)
		 (my_panic "upragmas")
      where
	dummy_dfun_id
	  = mkVanillaId (getName tycon) dummy_dfun_ty
		-- The name is getSrcLoc'd in an error message 

	theta' = classesToPreds theta
	dummy_dfun_ty = mkSigmaTy tyvars theta' voidTy
		-- All we need from the dfun is its "theta" part, used during
		-- equation simplification (tcSimplifyThetas).  The final
		-- dfun_id will have the superclass dictionaries as arguments too,
		-- but that'll be added after the equations are solved.  For now,
		-- it's enough just to make a dummy dfun with the simple theta part.
		-- 
		-- The part after the theta is dummied here as voidTy; actually it's
		-- 	(C (T a b)), but it doesn't seem worth constructing it.
		-- We can't leave it as a panic because to get the theta part we
		-- have to run down the type!

	my_panic str = panic "add_soln" -- pprPanic ("add_soln:"++str) (hsep [char ':', ppr clas, ppr tycon])
\end{code}

%************************************************************************
%*									*
\subsection[TcDeriv-normal-binds]{Bindings for the various classes}
%*									*
%************************************************************************

After all the trouble to figure out the required context for the
derived instance declarations, all that's left is to chug along to
produce them.  They will then be shoved into @tcInstDecls2@, which
will do all its usual business.

There are lots of possibilities for code to generate.  Here are
various general remarks.

PRINCIPLES:
\begin{itemize}
\item
We want derived instances of @Eq@ and @Ord@ (both v common) to be
``you-couldn't-do-better-by-hand'' efficient.

\item
Deriving @Show@---also pretty common--- should also be reasonable good code.

\item
Deriving for the other classes isn't that common or that big a deal.
\end{itemize}

PRAGMATICS:

\begin{itemize}
\item
Deriving @Ord@ is done mostly with the 1.3 @compare@ method.

\item
Deriving @Eq@ also uses @compare@, if we're deriving @Ord@, too.

\item
We {\em normally} generate code only for the non-defaulted methods;
there are some exceptions for @Eq@ and (especially) @Ord@...

\item
Sometimes we use a @_con2tag_<tycon>@ function, which returns a data
constructor's numeric (@Int#@) tag.  These are generated by
@gen_tag_n_con_binds@, and the heuristic for deciding if one of
these is around is given by @hasCon2TagFun@.

The examples under the different sections below will make this
clearer.

\item
Much less often (really just for deriving @Ix@), we use a
@_tag2con_<tycon>@ function.  See the examples.

\item
We use the renamer!!!  Reason: we're supposed to be
producing @RenamedMonoBinds@ for the methods, but that means
producing correctly-uniquified code on the fly.  This is entirely
possible (the @TcM@ monad has a @UniqueSupply@), but it is painful.
So, instead, we produce @RdrNameMonoBinds@ then heave 'em through
the renamer.  What a great hack!
\end{itemize}

\begin{code}
-- Generate the method bindings for the required instance
-- (paired with class name, as we need that when generating dict
--  names.)
gen_bind :: Fixities -> InstInfo -> ({-class-}OccName, {-tyCon-}OccName, RdrNameMonoBinds)
gen_bind fixities (InstInfo clas _ [ty] _ _ _ _ _)
  | not from_here 
  = (clas_nm, tycon_nm, EmptyMonoBinds)
  |  ckey == showClassKey 
  = (clas_nm, tycon_nm, gen_Show_binds fixities tycon)
  |  ckey == readClassKey 
  = (clas_nm, tycon_nm, gen_Read_binds fixities tycon)
  | otherwise
  = (clas_nm, tycon_nm,
     assoc "gen_bind:bad derived class"
	   [(eqClassKey,      gen_Eq_binds)
	   ,(ordClassKey,     gen_Ord_binds)
	   ,(enumClassKey,    gen_Enum_binds)
	   ,(boundedClassKey, gen_Bounded_binds)
	   ,(ixClassKey,      gen_Ix_binds)
	   ]
	   ckey
	   tycon)
  where
      clas_nm     = nameOccName (getName clas)
      tycon_nm    = nameOccName (getName tycon)
      from_here   = isLocallyDefined tycon
      (tycon,_,_) = splitAlgTyConApp ty	
      ckey	  = classKey clas
	    

gen_inst_info :: InstInfo
	      -> (Name, RenamedMonoBinds)
	      -> InstInfo				-- the gen'd (filled-in) "instance decl"

gen_inst_info (InstInfo clas tyvars tys@(ty:_) inst_decl_theta _ _ locn _) 
	      (dfun_name, meth_binds)
  =
	-- Generate the various instance-related Ids
    InstInfo clas tyvars tys inst_decl_theta
	       dfun_id
	       meth_binds
	       locn []
  where
   dfun_id = mkDictFunId dfun_name clas tyvars tys inst_decl_theta

   from_here = isLocallyDefined tycon
   (tycon,_,_) = splitAlgTyConApp ty
\end{code}


%************************************************************************
%*									*
\subsection[TcDeriv-taggery-Names]{What con2tag/tag2con functions are available?}
%*									*
%************************************************************************


data Foo ... = ...

con2tag_Foo :: Foo ... -> Int#
tag2con_Foo :: Int -> Foo ...	-- easier if Int, not Int#
maxtag_Foo  :: Int		-- ditto (NB: not unboxed)


We have a @con2tag@ function for a tycon if:
\begin{itemize}
\item
We're deriving @Eq@ and the tycon has nullary data constructors.

\item
Or: we're deriving @Ord@ (unless single-constructor), @Enum@, @Ix@
(enum type only????)
\end{itemize}

We have a @tag2con@ function for a tycon if:
\begin{itemize}
\item
We're deriving @Enum@, or @Ix@ (enum type only???)
\end{itemize}

If we have a @tag2con@ function, we also generate a @maxtag@ constant.

\begin{code}
gen_taggery_Names :: [InstInfo]
		  -> TcM s [(RdrName,	-- for an assoc list
		  	     TyCon,	-- related tycon
			     TagThingWanted)]

gen_taggery_Names inst_infos
  = --pprTrace "gen_taggery:\n" (vcat [hsep [ppr c, ppr t] | (c,t) <- all_CTs]) $
    foldlTc do_con2tag []           tycons_of_interest `thenTc` \ names_so_far ->
    foldlTc do_tag2con names_so_far tycons_of_interest
  where
    all_CTs = [ (c, get_tycon ty) | (InstInfo c _ [ty] _ _ _ _ _) <- inst_infos ]
		    
    get_tycon ty = case splitAlgTyConApp ty of { (tc, _, _) -> tc }

    all_tycons = map snd all_CTs
    (tycons_of_interest, _) = removeDups compare all_tycons
    
    do_con2tag acc_Names tycon
      | isDataTyCon tycon &&
        ((we_are_deriving eqClassKey tycon
	    && any isNullaryDataCon (tyConDataCons tycon))
	 || (we_are_deriving ordClassKey  tycon
	    && not (maybeToBool (maybeTyConSingleCon tycon)))
	 || (we_are_deriving enumClassKey tycon)
	 || (we_are_deriving ixClassKey   tycon))
	
      = returnTc ((con2tag_RDR tycon, tycon, GenCon2Tag)
		   : acc_Names)
      | otherwise
      = returnTc acc_Names

    do_tag2con acc_Names tycon
      | isDataTyCon tycon &&
         (we_are_deriving enumClassKey tycon ||
	  we_are_deriving ixClassKey   tycon
	  && isEnumerationTyCon tycon)
      = returnTc ( (tag2con_RDR tycon, tycon, GenTag2Con)
		 : (maxtag_RDR  tycon, tycon, GenMaxTag)
		 : acc_Names)
      | otherwise
      = returnTc acc_Names

    we_are_deriving clas_key tycon
      = is_in_eqns clas_key tycon all_CTs
      where
	is_in_eqns clas_key tycon [] = False
	is_in_eqns clas_key tycon ((c,t):cts)
	  =  (clas_key == classKey c && tycon == t)
	  || is_in_eqns clas_key tycon cts

\end{code}

\begin{code}
derivingThingErr :: Class -> TyCon -> FAST_STRING -> Message

derivingThingErr clas tycon why
  = sep [hsep [ptext SLIT("Can't make a derived instance of"), quotes (ppr clas)],
	 hsep [ptext SLIT("for the type"), quotes (ppr tycon)],
	 parens (ptext why)]

existentialErr clas tycon
  = sep [ptext SLIT("Can't derive any instances for type") <+> quotes (ppr tycon),
	 ptext SLIT("because it has existentially-quantified constructor(s)")]

derivCtxt tycon
  = ptext SLIT("When deriving classes for") <+> quotes (ppr tycon)
\end{code}
