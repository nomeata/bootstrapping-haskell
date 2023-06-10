%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcInstDecls]{Typechecking instance declarations}

\begin{code}
module TcInstDcls ( tcInstDecls1, tcInstDecls2 ) where

#include "HsVersions.h"

import HsSyn		( HsDecl(..), InstDecl(..),
			  HsBinds(..), MonoBinds(..),
			  HsExpr(..), InPat(..), HsLit(..), Sig(..),
			  andMonoBindList
			)
import RnHsSyn		( RenamedHsBinds, RenamedInstDecl, RenamedHsDecl )
import TcHsSyn		( TcMonoBinds, mkHsConApp )

import TcBinds		( tcSpecSigs )
import TcClassDcl	( tcMethodBind, checkFromThisClass )
import TcMonad
import RnMonad		( RnNameSupply, Fixities )
import Inst		( Inst, InstOrigin(..),
			  newDicts, newClassDicts,
			  LIE, emptyLIE, plusLIE, plusLIEs )
import TcDeriv		( tcDeriving )
import TcEnv		( ValueEnv, tcExtendGlobalValEnv, tcExtendTyVarEnvForMeths,
			  tcAddImportedIdInfo, tcInstId
			)
import TcInstUtil	( InstInfo(..), classDataCon )
import TcMonoType	( tcHsTopType )
import TcSimplify	( tcSimplifyAndCheck )
import TcType		( TcTyVar, zonkTcTyVarBndr )

import Bag		( emptyBag, unitBag, unionBags, unionManyBags,
			  foldBag, Bag
			)
import CmdLineOpts	( opt_GlasgowExts, opt_AllowUndecidableInstances )
import Class		( classBigSig, Class )
import Var		( idName, idType, Id, TyVar )
import DataCon		( isNullaryDataCon, splitProductType_maybe )
import Maybes 		( maybeToBool, catMaybes, expectJust )
import MkId		( mkDictFunId )
import Module		( ModuleName )
import Name		( isLocallyDefined, NamedThing(..)	)
import NameSet		( emptyNameSet )
import PrelInfo		( eRROR_ID )
import PprType		( pprConstraint )
import SrcLoc		( SrcLoc )
import TyCon		( isSynTyCon, tyConDerivings )
import Type		( Type, isUnLiftedType, mkTyVarTys,
			  splitSigmaTy, isTyVarTy,
			  splitTyConApp_maybe, splitDictTy_maybe,
			  getClassTys_maybe, splitAlgTyConApp_maybe,
			  classesToPreds, classesOfPreds,
			  unUsgTy, tyVarsOfTypes
			)
import Subst		( mkTopTyVarSubst, substClasses )
import VarSet		( mkVarSet, varSetElems )
import TysPrim		( byteArrayPrimTyCon, mutableByteArrayPrimTyCon )
import TysWiredIn	( stringTy, isFFIArgumentTy, isFFIResultTy )
import Unique		( Unique, cCallableClassKey, cReturnableClassKey, Uniquable(..) )
import Outputable
\end{code}

Typechecking instance declarations is done in two passes. The first
pass, made by @tcInstDecls1@, collects information to be used in the
second pass.

This pre-processed info includes the as-yet-unprocessed bindings
inside the instance declaration.  These are type-checked in the second
pass, when the class-instance envs and GVE contain all the info from
all the instance and value decls.  Indeed that's the reason we need
two passes over the instance decls.


Here is the overall algorithm.
Assume that we have an instance declaration

    instance c => k (t tvs) where b

\begin{enumerate}
\item
$LIE_c$ is the LIE for the context of class $c$
\item
$betas_bar$ is the free variables in the class method type, excluding the
   class variable
\item
$LIE_cop$ is the LIE constraining a particular class method
\item
$tau_cop$ is the tau type of a class method
\item
$LIE_i$ is the LIE for the context of instance $i$
\item
$X$ is the instance constructor tycon
\item
$gammas_bar$ is the set of type variables of the instance
\item
$LIE_iop$ is the LIE for a particular class method instance
\item
$tau_iop$ is the tau type for this instance of a class method
\item
$alpha$ is the class variable
\item
$LIE_cop' = LIE_cop [X gammas_bar / alpha, fresh betas_bar]$
\item
$tau_cop' = tau_cop [X gammas_bar / alpha, fresh betas_bar]$
\end{enumerate}

ToDo: Update the list above with names actually in the code.

\begin{enumerate}
\item
First, make the LIEs for the class and instance contexts, which means
instantiate $thetaC [X inst_tyvars / alpha ]$, yielding LIElistC' and LIEC',
and make LIElistI and LIEI.
\item
Then process each method in turn.
\item
order the instance methods according to the ordering of the class methods
\item
express LIEC' in terms of LIEI, yielding $dbinds_super$ or an error
\item
Create final dictionary function from bindings generated already
\begin{pseudocode}
df = lambda inst_tyvars
       lambda LIEI
	 let Bop1
	     Bop2
	     ...
	     Bopn
	 and dbinds_super
	      in <op1,op2,...,opn,sd1,...,sdm>
\end{pseudocode}
Here, Bop1 \ldots Bopn bind the methods op1 \ldots opn,
and $dbinds_super$ bind the superclass dictionaries sd1 \ldots sdm.
\end{enumerate}

\begin{code}
tcInstDecls1 :: ValueEnv		-- Contains IdInfo for dfun ids
	     -> [RenamedHsDecl]
	     -> ModuleName			-- module name for deriving
	     -> Fixities
	     -> RnNameSupply			-- for renaming derivings
	     -> TcM s (Bag InstInfo,
		       RenamedHsBinds)

tcInstDecls1 unf_env decls mod_name fixs rn_name_supply
  = 	-- Do the ordinary instance declarations
    mapNF_Tc (tcInstDecl1 unf_env) 
	     [inst_decl | InstD inst_decl <- decls]	`thenNF_Tc` \ inst_info_bags ->
    let
	decl_inst_info = unionManyBags inst_info_bags
    in
	-- Handle "derived" instances; note that we only do derivings
	-- for things in this module; we ignore deriving decls from
	-- interfaces!
    tcDeriving mod_name fixs rn_name_supply decl_inst_info
		    	`thenTc` \ (deriv_inst_info, deriv_binds) ->

    let
	full_inst_info = deriv_inst_info `unionBags` decl_inst_info
    in
    returnTc (full_inst_info, deriv_binds)


tcInstDecl1 :: ValueEnv -> RenamedInstDecl -> NF_TcM s (Bag InstInfo)

tcInstDecl1 unf_env (InstDecl poly_ty binds uprags dfun_name src_loc)
  = 	-- Prime error recovery, set source location
    recoverNF_Tc (returnNF_Tc emptyBag)	$
    tcAddSrcLoc src_loc			$

	-- Type-check all the stuff before the "where"
    tcHsTopType poly_ty			`thenTc` \ poly_ty' ->
    let
	(tyvars, theta, dict_ty) = splitSigmaTy poly_ty'
	constr			 = classesOfPreds theta
	(clas, inst_tys)	 = case splitDictTy_maybe dict_ty of
				     Just ct -> ct
				     Nothing -> pprPanic "tcInstDecl1" (ppr poly_ty)
    in

	-- Check for respectable instance type, and context
	-- but only do this for non-imported instance decls.
	-- Imported ones should have been checked already, and may indeed
	-- contain something illegal in normal Haskell, notably
	--	instance CCallable [Char] 
    (if isLocallyDefined dfun_name then
	scrutiniseInstanceHead clas inst_tys	`thenNF_Tc_`
	mapNF_Tc scrutiniseInstanceConstraint constr
     else
	returnNF_Tc []
     )						`thenNF_Tc_`

	-- Make the dfun id
    let
	dfun_id = mkDictFunId dfun_name clas tyvars inst_tys constr

	-- Add info from interface file
	final_dfun_id = tcAddImportedIdInfo unf_env dfun_id
    in
    returnTc (unitBag (InstInfo clas tyvars inst_tys constr
				final_dfun_id
			     	binds src_loc uprags))
\end{code}


%************************************************************************
%*									*
\subsection{Type-checking instance declarations, pass 2}
%*									*
%************************************************************************

\begin{code}
tcInstDecls2 :: Bag InstInfo
	     -> NF_TcM s (LIE, TcMonoBinds)

tcInstDecls2 inst_decls
  = foldBag combine tcInstDecl2 (returnNF_Tc (emptyLIE, EmptyMonoBinds)) inst_decls
  where
    combine tc1 tc2 = tc1 	`thenNF_Tc` \ (lie1, binds1) ->
		      tc2	`thenNF_Tc` \ (lie2, binds2) ->
		      returnNF_Tc (lie1 `plusLIE` lie2,
				   binds1 `AndMonoBinds` binds2)
\end{code}


======= New documentation starts here (Sept 92)	 ==============

The main purpose of @tcInstDecl2@ is to return a @HsBinds@ which defines
the dictionary function for this instance declaration.	For example
\begin{verbatim}
	instance Foo a => Foo [a] where
		op1 x = ...
		op2 y = ...
\end{verbatim}
might generate something like
\begin{verbatim}
	dfun.Foo.List dFoo_a = let op1 x = ...
				   op2 y = ...
			       in
				   Dict [op1, op2]
\end{verbatim}

HOWEVER, if the instance decl has no context, then it returns a
bigger @HsBinds@ with declarations for each method.  For example
\begin{verbatim}
	instance Foo [a] where
		op1 x = ...
		op2 y = ...
\end{verbatim}
might produce
\begin{verbatim}
	dfun.Foo.List a = Dict [Foo.op1.List a, Foo.op2.List a]
	const.Foo.op1.List a x = ...
	const.Foo.op2.List a y = ...
\end{verbatim}
This group may be mutually recursive, because (for example) there may
be no method supplied for op2 in which case we'll get
\begin{verbatim}
	const.Foo.op2.List a = default.Foo.op2 (dfun.Foo.List a)
\end{verbatim}
that is, the default method applied to the dictionary at this type.

What we actually produce in either case is:

	AbsBinds [a] [dfun_theta_dicts]
		 [(dfun.Foo.List, d)] ++ (maybe) [(const.Foo.op1.List, op1), ...]
		 { d = (sd1,sd2, ..., op1, op2, ...)
		   op1 = ...
		   op2 = ...
	 	 }

The "maybe" says that we only ask AbsBinds to make global constant methods
if the dfun_theta is empty.

		
For an instance declaration, say,

	instance (C1 a, C2 b) => C (T a b) where
		...

where the {\em immediate} superclasses of C are D1, D2, we build a dictionary
function whose type is

	(C1 a, C2 b, D1 (T a b), D2 (T a b)) => C (T a b)

Notice that we pass it the superclass dictionaries at the instance type; this
is the ``Mark Jones optimisation''.  The stuff before the "=>" here
is the @dfun_theta@ below.

First comes the easy case of a non-local instance decl.

\begin{code}
tcInstDecl2 :: InstInfo -> NF_TcM s (LIE, TcMonoBinds)

tcInstDecl2 (InstInfo clas inst_tyvars inst_tys
		      inst_decl_theta
		      dfun_id monobinds
		      locn uprags)
  | not (isLocallyDefined dfun_id)
  = returnNF_Tc (emptyLIE, EmptyMonoBinds)

{-
  -- I deleted this "optimisation" because when importing these
  -- instance decls the renamer would look for the dfun bindings and they weren't there.
  -- This would be fixable, but it seems simpler just to produce a tiny void binding instead,
  -- even though it's never used.

	-- This case deals with CCallable etc, which don't need any bindings
  | isNoDictClass clas			
  = returnNF_Tc (emptyLIE, EmptyBinds)
-}

  | otherwise
  =	 -- Prime error recovery
    recoverNF_Tc (returnNF_Tc (emptyLIE, EmptyMonoBinds))  $
    tcAddSrcLoc locn					   $

	-- Instantiate the instance decl with tc-style type variables
    tcInstId dfun_id		`thenNF_Tc` \ (inst_tyvars', dfun_theta', dict_ty') ->
    let
	(clas, inst_tys')	= expectJust "tcInstDecl2" (splitDictTy_maybe dict_ty')

	origin			= InstanceDeclOrigin

        (class_tyvars, sc_theta, _, op_items) = classBigSig clas

	dm_ids = [dm_id | (_, dm_id, _) <- op_items]

	-- Instantiate the theta found in the original instance decl
	inst_decl_theta' = substClasses (mkTopTyVarSubst inst_tyvars (mkTyVarTys inst_tyvars'))
				        inst_decl_theta

         -- Instantiate the super-class context with inst_tys
	sc_theta' = substClasses (mkTopTyVarSubst class_tyvars inst_tys') sc_theta
    in
	 -- Create dictionary Ids from the specified instance contexts.
    newClassDicts origin sc_theta'	`thenNF_Tc` \ (sc_dicts,        sc_dict_ids) ->
    newDicts origin dfun_theta'		`thenNF_Tc` \ (dfun_arg_dicts,  dfun_arg_dicts_ids)  ->
    newClassDicts origin inst_decl_theta' `thenNF_Tc` \ (inst_decl_dicts, _) ->
    newClassDicts origin [(clas,inst_tys')] `thenNF_Tc` \ (this_dict,       [this_dict_id]) ->

	 -- Check that all the method bindings come from this class
    checkFromThisClass clas op_items monobinds		`thenNF_Tc_`

    tcExtendTyVarEnvForMeths inst_tyvars inst_tyvars' (
 	tcExtendGlobalValEnv dm_ids (
		-- Default-method Ids may be mentioned in synthesised RHSs 

	mapAndUnzip3Tc (tcMethodBind clas origin inst_tyvars' inst_tys'
				     (classesToPreds inst_decl_theta')
				     monobinds uprags True)
		       op_items
    ))		 	`thenTc` \ (method_binds_s, insts_needed_s, meth_lies_w_ids) ->

	-- Deal with SPECIALISE instance pragmas by making them
	-- look like SPECIALISE pragmas for the dfun
    let
	dfun_prags = [SpecSig (idName dfun_id) ty loc | SpecInstSig ty loc <- uprags]
    in
    tcExtendGlobalValEnv [dfun_id] (
	tcSpecSigs dfun_prags
    )					`thenTc` \ (prag_binds, prag_lie) ->

	-- Check the overloading constraints of the methods and superclasses

	-- tcMethodBind has checked that the class_tyvars havn't
	-- been unified with each other or another type, but we must
	-- still zonk them
    mapNF_Tc zonkTcTyVarBndr inst_tyvars' 	`thenNF_Tc` \ zonked_inst_tyvars ->
    let
        inst_tyvars_set = mkVarSet zonked_inst_tyvars

	(meth_lies, meth_ids) = unzip meth_lies_w_ids

		 -- These insts are in scope; quite a few, eh?
	avail_insts = this_dict			`plusLIE` 
		      dfun_arg_dicts		`plusLIE`
		      sc_dicts			`plusLIE`
		      unionManyBags meth_lies

        methods_lie = plusLIEs insts_needed_s
    in

	-- Ditto method bindings
    tcAddErrCtxt methodCtxt (
      tcSimplifyAndCheck
		 (ptext SLIT("instance declaration context"))
		 inst_tyvars_set			-- Local tyvars
		 avail_insts
		 methods_lie
    )						 `thenTc` \ (const_lie1, lie_binds1) ->
    
	-- Check that we *could* construct the superclass dictionaries,
	-- even though we are *actually* going to pass the superclass dicts in;
	-- the check ensures that the caller will never have 
	--a problem building them.
    tcAddErrCtxt superClassCtxt (
      tcSimplifyAndCheck
		 (ptext SLIT("instance declaration context"))
		 inst_tyvars_set		-- Local tyvars
		 inst_decl_dicts		-- The instance dictionaries available
		 sc_dicts			-- The superclass dicationaries reqd
    )					`thenTc` \ _ -> 
    						-- Ignore the result; we're only doing
						-- this to make sure it can be done.

	-- Now do the simplification again, this time to get the
	-- bindings; this time we use an enhanced "avails"
	-- Ignore errors because they come from the *previous* tcSimplify
    discardErrsTc (
	tcSimplifyAndCheck
		 (ptext SLIT("instance declaration context"))
		 inst_tyvars_set
		 dfun_arg_dicts		-- NB! Don't include this_dict here, else the sc_dicts
					-- get bound by just selecting from this_dict!!
		 sc_dicts
    )						 `thenTc` \ (const_lie2, lie_binds2) ->
	

	-- Create the result bindings
    let
        dict_constr   = classDataCon clas
	scs_and_meths = sc_dict_ids ++ meth_ids

	dict_rhs
	  | null scs_and_meths
	  = 	-- Blatant special case for CCallable, CReturnable
		-- If the dictionary is empty then we should never
		-- select anything from it, so we make its RHS just
		-- emit an error message.  This in turn means that we don't
		-- mention the constructor, which doesn't exist for CCallable, CReturnable
		-- Hardly beautiful, but only three extra lines.
	    HsApp (TyApp (HsVar eRROR_ID) [(unUsgTy . idType) this_dict_id])
		  (HsLitOut (HsString msg) stringTy)

	  | otherwise	-- The common case
	  = mkHsConApp dict_constr inst_tys' (map HsVar (sc_dict_ids ++ meth_ids))
		-- We don't produce a binding for the dict_constr; instead we
		-- rely on the simplifier to unfold this saturated application
		-- We do this rather than generate an HsCon directly, because
		-- it means that the special cases (e.g. dictionary with only one
		-- member) are dealt with by the common MkId.mkDataConWrapId code rather
		-- than needing to be repeated here.

	  where
	    msg = _PK_ ("Compiler error: bad dictionary " ++ showSDoc (ppr clas))

	dict_bind    = VarMonoBind this_dict_id dict_rhs
	method_binds = andMonoBindList method_binds_s

	main_bind
	  = AbsBinds
		 zonked_inst_tyvars
		 dfun_arg_dicts_ids
		 [(inst_tyvars', dfun_id, this_dict_id)] 
		 emptyNameSet		-- No inlines (yet)
		 (lie_binds1	`AndMonoBinds` 
		  lie_binds2	`AndMonoBinds`
		  method_binds	`AndMonoBinds`
		  dict_bind)
    in
    returnTc (const_lie1 `plusLIE` const_lie2 `plusLIE` prag_lie,
	      main_bind `AndMonoBinds` prag_binds)
\end{code}


%************************************************************************
%*									*
\subsection{Checking for a decent instance type}
%*									*
%************************************************************************

@scrutiniseInstanceHead@ checks the type {\em and} its syntactic constraints:
it must normally look like: @instance Foo (Tycon a b c ...) ...@

The exceptions to this syntactic checking: (1)~if the @GlasgowExts@
flag is on, or (2)~the instance is imported (they must have been
compiled elsewhere). In these cases, we let them go through anyway.

We can also have instances for functions: @instance Foo (a -> b) ...@.

\begin{code}
scrutiniseInstanceConstraint (clas, tys)
  |  all isTyVarTy tys 
  || opt_AllowUndecidableInstances = returnNF_Tc ()
  | otherwise	      	           = addErrTc (instConstraintErr clas tys)

scrutiniseInstanceHead clas inst_taus
  |	-- CCALL CHECK
	-- A user declaration of a CCallable/CReturnable instance
	-- must be for a "boxed primitive" type.
    (getUnique clas == cCallableClassKey   && not (ccallable_type   first_inst_tau)) ||
    (getUnique clas == cReturnableClassKey && not (creturnable_type first_inst_tau))
  = addErrTc (nonBoxedPrimCCallErr clas first_inst_tau)

  	-- DERIVING CHECK
	-- It is obviously illegal to have an explicit instance
	-- for something that we are also planning to `derive'
  | maybeToBool alg_tycon_app_maybe && clas `elem` (tyConDerivings alg_tycon)
  = addErrTc (derivingWhenInstanceExistsErr clas first_inst_tau)
	   -- Kind check will have ensured inst_taus is of length 1

	-- Allow anything for AllowUndecidableInstances
  | opt_AllowUndecidableInstances
  = returnNF_Tc ()

	-- If GlasgowExts then check at least one isn't a type variable
  | opt_GlasgowExts 
  = if all isTyVarTy inst_taus then
	addErrTc (instTypeErr clas inst_taus (text "There must be at least one non-type-variable in the instance head"))
    else
	returnNF_Tc ()

	-- WITH HASKELL 1.4, MUST HAVE C (T a b c)
  |  not (length inst_taus == 1 &&
	  maybeToBool maybe_tycon_app &&	-- Yes, there's a type constuctor
          not (isSynTyCon tycon) &&		-- ...but not a synonym
          all isTyVarTy arg_tys && 		-- Applied to type variables
	  length (varSetElems (tyVarsOfTypes arg_tys)) == length arg_tys
		 -- This last condition checks that all the type variables are distinct
     )
  = addErrTc (instTypeErr clas inst_taus
			(text "the instance type must be of form (T a b c)" $$
			 text "where T is not a synonym, and a,b,c are distinct type variables")
    )

  | otherwise
  = returnNF_Tc ()

  where
    (first_inst_tau : _)       = inst_taus

	-- Stuff for algebraic or -> type
    maybe_tycon_app	  = splitTyConApp_maybe first_inst_tau
    Just (tycon, arg_tys) = maybe_tycon_app

	-- Stuff for an *algebraic* data type
    alg_tycon_app_maybe	   = splitAlgTyConApp_maybe first_inst_tau
				-- The "Alg" part looks through synonyms
    Just (alg_tycon, _, _) = alg_tycon_app_maybe
 
ccallable_type   ty = isFFIArgumentTy False {- Not safe call -} ty
creturnable_type ty = isFFIResultTy ty
\end{code}

\begin{code}
instConstraintErr clas tys
  = hang (ptext SLIT("Illegal constraint") <+> 
	  quotes (pprConstraint clas tys) <+> 
	  ptext SLIT("in instance context"))
	 4 (ptext SLIT("(Instance contexts must constrain only type variables)"))
	
instTypeErr clas tys msg
  = sep [ptext SLIT("Illegal instance declaration for") <+> quotes (pprConstraint clas tys),
	 nest 4 (parens msg)
    ]

derivingWhenInstanceExistsErr clas tycon
  = hang (hsep [ptext SLIT("Deriving class"), 
		       quotes (ppr clas), 
		       ptext SLIT("type"), quotes (ppr tycon)])
         4 (ptext SLIT("when an explicit instance exists"))

nonBoxedPrimCCallErr clas inst_ty
  = hang (ptext SLIT("Unacceptable instance type for ccall-ish class"))
	 4 (hsep [ ptext SLIT("class"), ppr clas, ptext SLIT("type"),
    		        ppr inst_ty])

methodCtxt     = ptext SLIT("When checking the methods of an instance declaration")
superClassCtxt = ptext SLIT("When checking the superclasses of an instance declaration")
\end{code}
