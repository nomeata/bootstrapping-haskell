%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnBinds]{Renaming and dependency analysis of bindings}

This module does renaming and dependency analysis on value bindings in
the abstract syntax.  It does {\em not} do cycle-checks on class or
type-synonym declarations; those cannot be done at this stage because
they may be affected by renaming (which isn't fully worked out yet).

\begin{code}
module RnBinds (
	rnTopBinds, rnTopMonoBinds,
	rnMethodBinds, renameSigs,
	rnBinds, rnMonoBinds
   ) where

#include "HsVersions.h"

import {-# SOURCE #-} RnSource ( rnHsSigType )

import HsSyn
import HsBinds		( sigsForMe )
import RdrHsSyn
import RnHsSyn
import RnMonad
import RnExpr		( rnMatch, rnGRHSsAndBinds, rnPat, checkPrecMatch )
import RnEnv		( bindLocatedLocalsRn, lookupBndrRn, lookupOccRn, lookupGlobalOccRn,
			  newLocalNames, isUnboundName, warnUnusedBinds
			)
import CmdLineOpts	( opt_SigsRequired )
import Digraph		( stronglyConnComp, SCC(..) )
import Name		( OccName(..), Provenance, 
			  Name, isExportedName,
			  NameSet, emptyNameSet, mkNameSet, unionNameSets, 
		 	  minusNameSet, unionManyNameSets, elemNameSet, unitNameSet, nameSetToList
			)
import BasicTypes	( RecFlag(..), TopLevelFlag(..) )
import Util		( thenCmp, removeDups, panic, panic#, assertPanic )
import UniqSet		( UniqSet )
import ListSetOps	( minusList )
import Bag		( bagToList )
import UniqFM		( UniqFM )
import Outputable
\end{code}

-- ToDo: Put the annotations into the monad, so that they arrive in the proper
-- place and can be used when complaining.

The code tree received by the function @rnBinds@ contains definitions
in where-clauses which are all apparently mutually recursive, but which may
not really depend upon each other. For example, in the top level program
\begin{verbatim}
f x = y where a = x
	      y = x
\end{verbatim}
the definitions of @a@ and @y@ do not depend on each other at all.
Unfortunately, the typechecker cannot always check such definitions.
\footnote{Mycroft, A. 1984. Polymorphic type schemes and recursive
definitions. In Proceedings of the International Symposium on Programming,
Toulouse, pp. 217-39. LNCS 167. Springer Verlag.}
However, the typechecker usually can check definitions in which only the
strongly connected components have been collected into recursive bindings.
This is precisely what the function @rnBinds@ does.

ToDo: deal with case where a single monobinds binds the same variable
twice.

The vertag tag is a unique @Int@; the tags only need to be unique
within one @MonoBinds@, so that unique-Int plumbing is done explicitly
(heavy monad machinery not needed).

\begin{code}
type VertexTag	= Int
type Cycle	= [VertexTag]
type Edge	= (VertexTag, VertexTag)
\end{code}

%************************************************************************
%*									*
%* naming conventions							*
%*									*
%************************************************************************

\subsection[name-conventions]{Name conventions}

The basic algorithm involves walking over the tree and returning a tuple
containing the new tree plus its free variables. Some functions, such
as those walking polymorphic bindings (HsBinds) and qualifier lists in
list comprehensions (@Quals@), return the variables bound in local
environments. These are then used to calculate the free variables of the
expression evaluated in these environments.

Conventions for variable names are as follows:
\begin{itemize}
\item
new code is given a prime to distinguish it from the old.

\item
a set of variables defined in @Exp@ is written @dvExp@

\item
a set of variables free in @Exp@ is written @fvExp@
\end{itemize}

%************************************************************************
%*									*
%* analysing polymorphic bindings (HsBinds, Bind, MonoBinds)		*
%*									*
%************************************************************************

\subsubsection[dep-HsBinds]{Polymorphic bindings}

Non-recursive expressions are reconstructed without any changes at top
level, although their component expressions may have to be altered.
However, non-recursive expressions are currently not expected as
\Haskell{} programs, and this code should not be executed.

Monomorphic bindings contain information that is returned in a tuple
(a @FlatMonoBindsInfo@) containing:

\begin{enumerate}
\item
a unique @Int@ that serves as the ``vertex tag'' for this binding.

\item
the name of a function or the names in a pattern. These are a set
referred to as @dvLhs@, the defined variables of the left hand side.

\item
the free variables of the body. These are referred to as @fvBody@.

\item
the definition's actual code. This is referred to as just @code@.
\end{enumerate}

The function @nonRecDvFv@ returns two sets of variables. The first is
the set of variables defined in the set of monomorphic bindings, while the
second is the set of free variables in those bindings.

The set of variables defined in a non-recursive binding is just the
union of all of them, as @union@ removes duplicates. However, the
free variables in each successive set of cumulative bindings is the
union of those in the previous set plus those of the newest binding after
the defined variables of the previous set have been removed.

@rnMethodBinds@ deals only with the declarations in class and
instance declarations.	It expects only to see @FunMonoBind@s, and
it expects the global environment to contain bindings for the binders
(which are all class operations).

%************************************************************************
%*									*
%* 		Top-level bindings
%*									*
%************************************************************************

@rnTopBinds@ assumes that the environment already
contains bindings for the binders of this particular binding.

\begin{code}
rnTopBinds    :: RdrNameHsBinds -> RnMS s RenamedHsBinds

rnTopBinds EmptyBinds		       	  = returnRn EmptyBinds
rnTopBinds (MonoBind bind sigs _) 	  = rnTopMonoBinds bind sigs
  -- The parser doesn't produce other forms


rnTopMonoBinds EmptyMonoBinds sigs 
  = returnRn EmptyBinds

rnTopMonoBinds mbinds sigs
 =  mapRn lookupBndrRn binder_rdr_names	`thenRn` \ binder_names ->
    let
	binder_set       = mkNameSet binder_names
	exported_binders = mkNameSet (filter isExportedName binder_names)
    in
    rn_mono_binds TopLevel
		  binder_set mbinds sigs		`thenRn` \ (new_binds, fv_set) ->
    let
	unused_binders = binder_set `minusNameSet` (fv_set `unionNameSets` exported_binders)
    in
    warnUnusedBinds unused_binders	`thenRn_`
    returnRn new_binds
  where
    binder_rdr_names = map fst (bagToList (collectMonoBinders mbinds))
\end{code}

%************************************************************************
%*									*
%* 		Nested binds
%*									*
%************************************************************************

@rnMonoBinds@
	- collects up the binders for this declaration group,
	- checks that they form a set
	- extends the environment to bind them to new local names
	- calls @rnMonoBinds@ to do the real work

\begin{code}
rnBinds	      :: RdrNameHsBinds 
	      -> (RenamedHsBinds -> RnMS s (result, FreeVars))
	      -> RnMS s (result, FreeVars)

rnBinds EmptyBinds	       thing_inside = thing_inside EmptyBinds
rnBinds (MonoBind bind sigs _) thing_inside = rnMonoBinds bind sigs thing_inside
  -- the parser doesn't produce other forms


rnMonoBinds :: RdrNameMonoBinds -> [RdrNameSig]
	    -> (RenamedHsBinds -> RnMS s (result, FreeVars))
	    -> RnMS s (result, FreeVars)

rnMonoBinds EmptyMonoBinds sigs thing_inside = thing_inside EmptyBinds

rnMonoBinds mbinds sigs	thing_inside -- Non-empty monobinds
  =	-- Extract all the binders in this group,
	-- and extend current scope, inventing new names for the new binders
	-- This also checks that the names form a set
    bindLocatedLocalsRn (text "binding group") mbinders_w_srclocs		$ \ new_mbinders ->
    let
	binder_set = mkNameSet new_mbinders
    in
    rn_mono_binds NotTopLevel
		  binder_set mbinds sigs	`thenRn` \ (binds,bind_fvs) ->

	-- Now do the "thing inside", and deal with the free-variable calculations
    thing_inside binds					`thenRn` \ (result,result_fvs) ->
    let
	all_fvs        = result_fvs  `unionNameSets` bind_fvs
	net_fvs        = all_fvs `minusNameSet` binder_set
	unused_binders = binder_set `minusNameSet` all_fvs
    in
    warnUnusedBinds unused_binders	`thenRn_`
    returnRn (result, net_fvs)
  where
    mbinders_w_srclocs = bagToList (collectMonoBinders mbinds)
\end{code}


%************************************************************************
%*									*
%* 		MonoBinds -- the main work is done here
%*									*
%************************************************************************

@rnMonoBinds@ is used by *both* top-level and nested bindings.  It
assumes that all variables bound in this group are already in scope.
This is done *either* by pass 3 (for the top-level bindings), *or* by
@rnNestedMonoBinds@ (for the nested ones).

\begin{code}
rn_mono_binds :: TopLevelFlag
	      -> NameSet		-- Binders of this group
	      -> RdrNameMonoBinds	
	      -> [RdrNameSig]		-- Signatures attached to this group
	      -> RnMS s (RenamedHsBinds, 	-- 
		         FreeVars)	-- Free variables

rn_mono_binds top_lev binders mbinds sigs
  =
	 -- Rename the bindings, returning a MonoBindsInfo
	 -- which is a list of indivisible vertices so far as
	 -- the strongly-connected-components (SCC) analysis is concerned
    renameSigs top_lev False binders sigs	`thenRn` \ siglist ->
    flattenMonoBinds siglist mbinds	`thenRn` \ mbinds_info ->

	 -- Do the SCC analysis
    let edges	    = mkEdges (mbinds_info `zip` [(0::Int)..])
	scc_result  = stronglyConnComp edges
	final_binds = foldr1 ThenBinds (map reconstructCycle scc_result)

	 -- Deal with bound and free-var calculation
	rhs_fvs = unionManyNameSets [fvs | (_,fvs,_,_) <- mbinds_info]
    in
    returnRn (final_binds, rhs_fvs)
\end{code}

@flattenMonoBinds@ is ever-so-slightly magical in that it sticks
unique ``vertex tags'' on its output; minor plumbing required.

\begin{code}
flattenMonoBinds :: [RenamedSig]		-- Signatures
		 -> RdrNameMonoBinds
		 -> RnMS s [FlatMonoBindsInfo]

flattenMonoBinds sigs EmptyMonoBinds = returnRn []

flattenMonoBinds sigs (AndMonoBinds bs1 bs2)
  = flattenMonoBinds sigs bs1	`thenRn` \ flat1 ->
    flattenMonoBinds sigs bs2	`thenRn` \ flat2 ->
    returnRn (flat1 ++ flat2)

flattenMonoBinds sigs (PatMonoBind pat grhss_and_binds locn)
  = pushSrcLocRn locn		 	$
    rnPat pat				`thenRn` \ pat' ->
    rnGRHSsAndBinds grhss_and_binds	`thenRn` \ (grhss_and_binds', fvs) ->

	 -- Find which things are bound in this group
    let
	names_bound_here = mkNameSet (collectPatBinders pat')
	sigs_for_me      = sigsForMe (`elemNameSet` names_bound_here) sigs
	sigs_fvs         = foldr sig_fv emptyNameSet sigs_for_me
    in
    returnRn 
	[(names_bound_here,
	  fvs `unionNameSets` sigs_fvs,
	  PatMonoBind pat' grhss_and_binds' locn,
	  sigs_for_me
	 )]

flattenMonoBinds sigs (FunMonoBind name inf matches locn)
  = pushSrcLocRn locn				 $
    mapRn (checkPrecMatch inf name) matches	`thenRn_`
    lookupBndrRn name				`thenRn` \ name' ->
    mapAndUnzipRn rnMatch matches		`thenRn` \ (new_matches, fv_lists) ->
    let
	fvs	    = unionManyNameSets fv_lists
	sigs_for_me = sigsForMe (name' ==) sigs
	sigs_fvs    = foldr sig_fv emptyNameSet sigs_for_me
    in
    returnRn
      [(unitNameSet name',
	fvs `unionNameSets` sigs_fvs,
	FunMonoBind name' inf new_matches locn,
	sigs_for_me
	)]
\end{code}


@rnMethodBinds@ is used for the method bindings of an instance
declaration.   like @rnMonoBinds@ but without dependency analysis.

\begin{code}
rnMethodBinds :: RdrNameMonoBinds -> RnMS s RenamedMonoBinds

rnMethodBinds EmptyMonoBinds = returnRn EmptyMonoBinds

rnMethodBinds (AndMonoBinds mb1 mb2)
  = andRn AndMonoBinds (rnMethodBinds mb1)
		       (rnMethodBinds mb2)

rnMethodBinds (FunMonoBind name inf matches locn)
  = pushSrcLocRn locn				   $
    mapRn (checkPrecMatch inf name) matches	`thenRn_`

    lookupGlobalOccRn name			`thenRn` \ sel_name -> 
	-- We use the selector name as the binder

    mapAndUnzipRn rnMatch matches		`thenRn` \ (new_matches, _) ->
    returnRn (FunMonoBind sel_name inf new_matches locn)

rnMethodBinds (PatMonoBind (VarPatIn name) grhss_and_binds locn)
  = pushSrcLocRn locn			$
    lookupGlobalOccRn name			`thenRn` \ sel_name -> 
    rnGRHSsAndBinds grhss_and_binds	`thenRn` \ (grhss_and_binds', _) ->
    returnRn (PatMonoBind (VarPatIn sel_name) grhss_and_binds' locn)

-- Can't handle method pattern-bindings which bind multiple methods.
rnMethodBinds mbind@(PatMonoBind other_pat _ locn)
  = pushSrcLocRn locn	$
    failWithRn EmptyMonoBinds (methodBindErr mbind)
\end{code}

\begin{code}
-- If a SPECIALIZE pragma is of the "... = blah" form,
-- then we'd better make sure "blah" is taken into
-- acct in the dependency analysis (or we get an
-- unexpected out-of-scope error)! WDP 95/07

sig_fv (SpecSig _ _ (Just blah) _) acc = acc `unionNameSets` (unitNameSet blah)
sig_fv _			   acc = acc
\end{code}

%************************************************************************
%*									*
\subsection[reconstruct-deps]{Reconstructing dependencies}
%*									*
%************************************************************************

This @MonoBinds@- and @ClassDecls@-specific code is segregated here,
as the two cases are similar.

\begin{code}
reconstructCycle :: SCC FlatMonoBindsInfo
		 -> RenamedHsBinds

reconstructCycle (AcyclicSCC (_, _, binds, sigs))
  = MonoBind binds sigs NonRecursive

reconstructCycle (CyclicSCC cycle)
  = MonoBind this_gp_binds this_gp_sigs Recursive
  where
    this_gp_binds      = foldr1 AndMonoBinds [binds | (_, _, binds, _) <- cycle]
    this_gp_sigs       = foldr1 (++)	     [sigs  | (_, _, _, sigs) <- cycle]
\end{code}

%************************************************************************
%*									*
%*	Manipulating FlatMonoBindInfo					*
%*									*
%************************************************************************

During analysis a @MonoBinds@ is flattened to a @FlatMonoBindsInfo@.
The @RenamedMonoBinds@ is always an empty bind, a pattern binding or
a function binding, and has itself been dependency-analysed and
renamed.

\begin{code}
type FlatMonoBindsInfo
  = (NameSet,			-- Set of names defined in this vertex
     NameSet,			-- Set of names used in this vertex
     RenamedMonoBinds,
     [RenamedSig])		-- Signatures, if any, for this vertex

mkEdges :: [(FlatMonoBindsInfo, VertexTag)] -> [(FlatMonoBindsInfo, VertexTag, [VertexTag])]

mkEdges flat_info
  = [ (info, tag, dest_vertices (nameSetToList names_used))
    | (info@(names_defined, names_used, mbind, sigs), tag) <- flat_info
    ]
  where
 	 -- An edge (v,v') indicates that v depends on v'
    dest_vertices src_mentions = [ target_vertex
			         | ((names_defined, _, _, _), target_vertex) <- flat_info,
				   mentioned_name <- src_mentions,
				   mentioned_name `elemNameSet` names_defined
			         ]
\end{code}


%************************************************************************
%*									*
\subsubsection[dep-Sigs]{Signatures (and user-pragmas for values)}
%*									*
%************************************************************************

@renameSigs@ checks for: (a)~more than one sig for one thing;
(b)~signatures given for things not bound here; (c)~with suitably
flaggery, that all top-level things have type signatures.

\begin{code}
renameSigs :: TopLevelFlag
	    -> Bool			-- True <-> sigs for an instance decl
					-- hence SPECIALISE instance prags ok
	    -> NameSet			-- Set of names bound in this group
	    -> [RdrNameSig]
	    -> RnMS s [RenamedSig]		 -- List of Sig constructors

renameSigs top_lev inst_decl binders sigs
  =	 -- Rename the signatures
    mapRn renameSig sigs   	`thenRn` \ sigs' ->

	-- Check for (a) duplicate signatures
	--	     (b) signatures for things not in this group
	--	     (c) optionally, bindings with no signature
    let
	(goodies, dups) = removeDups cmp_sig (sigsForMe (not . isUnboundName) sigs')
	not_this_group  = sigsForMe (not . (`elemNameSet` binders)) goodies
	spec_inst_sigs  = [s | s@(SpecInstSig _ _) <- goodies]
	type_sig_vars	= [n | Sig n _ _ <- goodies]
	sigs_required   = case top_lev of {TopLevel -> opt_SigsRequired; NotTopLevel -> False}
	un_sigd_binders | sigs_required = nameSetToList binders `minusList` type_sig_vars
			| otherwise	= []
    in
    mapRn dupSigDeclErr dups 				`thenRn_`
    mapRn unknownSigErr not_this_group			`thenRn_`
    (if not inst_decl then
	mapRn unknownSigErr spec_inst_sigs
     else
	returnRn []
    )							`thenRn_`
    mapRn (addErrRn.missingSigErr) un_sigd_binders	`thenRn_`

    returnRn sigs' -- bad ones and all:
		   -- we need bindings of *some* sort for every name


renameSig (Sig v ty src_loc)
  = pushSrcLocRn src_loc $
    lookupBndrRn v				`thenRn` \ new_v ->
    rnHsSigType (quotes (ppr v)) ty		`thenRn` \ new_ty ->
    returnRn (Sig new_v new_ty src_loc)

renameSig (SpecInstSig ty src_loc)
  = pushSrcLocRn src_loc $
    rnHsSigType (text "A SPECIALISE instance pragma") ty		`thenRn` \ new_ty ->
    returnRn (SpecInstSig new_ty src_loc)

renameSig (SpecSig v ty using src_loc)
  = pushSrcLocRn src_loc $
    lookupBndrRn v			`thenRn` \ new_v ->
    rnHsSigType (quotes (ppr v)) ty	`thenRn` \ new_ty ->
    rn_using using			`thenRn` \ new_using ->
    returnRn (SpecSig new_v new_ty new_using src_loc)
  where
    rn_using Nothing  = returnRn Nothing
    rn_using (Just x) = lookupOccRn x `thenRn` \ new_x ->
			returnRn (Just new_x)

renameSig (InlineSig v src_loc)
  = pushSrcLocRn src_loc $
    lookupBndrRn v		`thenRn` \ new_v ->
    returnRn (InlineSig new_v src_loc)

renameSig (NoInlineSig v src_loc)
  = pushSrcLocRn src_loc $
    lookupBndrRn v		`thenRn` \ new_v ->
    returnRn (NoInlineSig new_v src_loc)
\end{code}

Checking for distinct signatures; oh, so boring

\begin{code}
cmp_sig :: RenamedSig -> RenamedSig -> Ordering
cmp_sig (Sig n1 _ _)	     (Sig n2 _ _)    	  = n1 `compare` n2
cmp_sig (InlineSig n1 _)     (InlineSig n2 _) 	  = n1 `compare` n2
cmp_sig (NoInlineSig n1 _)   (NoInlineSig n2 _)	  = n1 `compare` n2
cmp_sig (SpecInstSig ty1 _)  (SpecInstSig ty2 _)  = cmpHsType compare ty1 ty2
cmp_sig (SpecSig n1 ty1 _ _) (SpecSig n2 ty2 _ _) 
  = -- may have many specialisations for one value;
	-- but not ones that are exactly the same...
	thenCmp (n1 `compare` n2) (cmpHsType compare ty1 ty2)

cmp_sig other_1 other_2					-- Tags *must* be different
  | (sig_tag other_1) _LT_ (sig_tag other_2) = LT 
  | otherwise				     = GT

sig_tag (Sig n1 _ _)    	   = (ILIT(1) :: FAST_INT)
sig_tag (SpecSig n1 _ _ _)    	   = ILIT(2)
sig_tag (InlineSig n1 _)  	   = ILIT(3)
sig_tag (NoInlineSig n1 _)  	   = ILIT(4)
sig_tag (SpecInstSig _ _)	   = ILIT(5)
sig_tag _			   = panic# "tag(RnBinds)"
\end{code}

%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
dupSigDeclErr (sig:sigs)
  = pushSrcLocRn loc $
    addErrRn (sep [ptext SLIT("Duplicate"),
		   ptext what_it_is <> colon,
		   ppr sig])
  where
    (what_it_is, loc) = sig_doc sig

unknownSigErr sig
  = pushSrcLocRn loc $
    addErrRn (sep [ptext SLIT("Misplaced"),
		   ptext what_it_is <> colon,
		   ppr sig])
  where
    (what_it_is, loc) = sig_doc sig

sig_doc (Sig        _ _ loc) 	    = (SLIT("type signature"),loc)
sig_doc (ClassOpSig _ _ _ loc) 	    = (SLIT("class-method type signature"), loc)
sig_doc (SpecSig    _ _ _ loc) 	    = (SLIT("SPECIALISE pragma"),loc)
sig_doc (InlineSig  _     loc) 	    = (SLIT("INLINE pragma"),loc)
sig_doc (NoInlineSig  _   loc) 	    = (SLIT("NOINLINE pragma"),loc)
sig_doc (SpecInstSig _ loc)	    = (SLIT("SPECIALISE instance pragma"),loc)

missingSigErr var
  = sep [ptext SLIT("Definition but no type signature for"), quotes (ppr var)]

methodBindErr mbind
 =  hang (ptext SLIT("Can't handle multiple methods defined by one pattern binding"))
       4 (ppr mbind)
\end{code}
