%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[StgSyn]{Shared term graph (STG) syntax for spineless-tagless code generation}

This data type represents programs just before code generation
(conversion to @AbstractC@): basically, what we have is a stylised
form of @CoreSyntax@, the style being one that happens to be ideally
suited to spineless tagless code generation.

\begin{code}
module StgSyn (
	GenStgArg(..),
	GenStgLiveVars,

	GenStgBinding(..), GenStgExpr(..), GenStgRhs(..),
	GenStgCaseAlts(..), GenStgCaseDefault(..),

	UpdateFlag(..),

	StgBinderInfo(..),
	stgArgOcc, stgUnsatOcc, stgStdHeapOcc, stgNoUpdHeapOcc,
	stgNormalOcc, stgFakeFunAppOcc,
	combineStgBinderInfo,

	-- a set of synonyms for the most common (only :-) parameterisation
	StgArg, StgLiveVars,
	StgBinding, StgExpr, StgRhs,
	StgCaseAlts, StgCaseDefault,

	pprStgBinding, pprStgBindings,
	getArgPrimRep,
	isLitLitArg,
	stgArity,
	collectFinalStgBinders
    ) where

#include "HsVersions.h"

import CostCentre	( showCostCentre, CostCentre )
import Id		( idPrimRep, DataCon, 
			  GenId{-instance NamedThing-}, Id )
import Literal		( literalPrimRep, isLitLitLit, Literal{-instance Outputable-} )
import Outputable
import PrimOp		( PrimOp{-instance Outputable-} )
import Type             ( Type )
import Unique		( pprUnique, Unique )
import UniqSet		( isEmptyUniqSet, uniqSetToList, UniqSet )
import Util		( panic )
\end{code}

%************************************************************************
%*									*
\subsection{@GenStgBinding@}
%*									*
%************************************************************************

As usual, expressions are interesting; other things are boring.  Here
are the boring things [except note the @GenStgRhs@], parameterised
with respect to binder and occurrence information (just as in
@CoreSyn@):

\begin{code}
data GenStgBinding bndr occ
  = StgNonRec	bndr (GenStgRhs bndr occ)
  | StgRec	[(bndr, GenStgRhs bndr occ)]
\end{code}

%************************************************************************
%*									*
\subsection{@GenStgArg@}
%*									*
%************************************************************************

\begin{code}
data GenStgArg occ
  = StgVarArg	occ
  | StgLitArg	Literal
  | StgConArg   DataCon		-- A nullary data constructor
\end{code}

\begin{code}
getArgPrimRep (StgVarArg  local) = idPrimRep local
getArgPrimRep (StgConArg  con)	 = idPrimRep con
getArgPrimRep (StgLitArg  lit)	 = literalPrimRep lit

isLitLitArg (StgLitArg x) = isLitLitLit x
isLitLitArg _		  = False
\end{code}

%************************************************************************
%*									*
\subsection{STG expressions}
%*									*
%************************************************************************

The @GenStgExpr@ data type is parameterised on binder and occurrence
info, as before.

%************************************************************************
%*									*
\subsubsection{@GenStgExpr@ application}
%*									*
%************************************************************************

An application is of a function to a list of atoms [not expressions].
Operationally, we want to push the arguments on the stack and call the
function.  (If the arguments were expressions, we would have to build
their closures first.)

There is no constructor for a lone variable; it would appear as
@StgApp var [] _@.
\begin{code}
type GenStgLiveVars occ = UniqSet occ

data GenStgExpr bndr occ
  = StgApp
	(GenStgArg occ)	-- function
	[GenStgArg occ]	-- arguments
	(GenStgLiveVars occ)	-- Live vars in continuation; ie not
				-- including the function and args

    -- NB: a literal is: StgApp <lit-atom> [] ...
\end{code}

%************************************************************************
%*									*
\subsubsection{@StgCon@ and @StgPrim@---saturated applications}
%*									*
%************************************************************************

There are two specialised forms of application, for
constructors and primitives.
\begin{code}
  | StgCon			-- always saturated
	Id -- data constructor
	[GenStgArg occ]
	(GenStgLiveVars occ)	-- Live vars in continuation; ie not
				-- including the constr and args

  | StgPrim			-- always saturated
	PrimOp
	[GenStgArg occ]
	(GenStgLiveVars occ)	-- Live vars in continuation; ie not
				-- including the op and args
\end{code}
These forms are to do ``inline versions,'' as it were.
An example might be: @f x = x:[]@.

%************************************************************************
%*									*
\subsubsection{@GenStgExpr@: case-expressions}
%*									*
%************************************************************************

This has the same boxed/unboxed business as Core case expressions.
\begin{code}
  | StgCase
	(GenStgExpr bndr occ)
			-- the thing to examine

	(GenStgLiveVars occ) -- Live vars of whole case
			-- expression; i.e., those which mustn't be
			-- overwritten

	(GenStgLiveVars occ) -- Live vars of RHSs;
			-- i.e., those which must be saved before eval.
			--
			-- note that an alt's constructor's
			-- binder-variables are NOT counted in the
			-- free vars for the alt's RHS

	Unique		-- Occasionally needed to compile case
			-- statements, as the uniq for a local
			-- variable to hold the tag of a primop with
			-- algebraic result

	(GenStgCaseAlts bndr occ)
\end{code}

%************************************************************************
%*									*
\subsubsection{@GenStgExpr@:  @let(rec)@-expressions}
%*									*
%************************************************************************

The various forms of let(rec)-expression encode most of the
interesting things we want to do.
\begin{enumerate}
\item
\begin{verbatim}
let-closure x = [free-vars] expr [args]
in e
\end{verbatim}
is equivalent to
\begin{verbatim}
let x = (\free-vars -> \args -> expr) free-vars
\end{verbatim}
\tr{args} may be empty (and is for most closures).  It isn't under
circumstances like this:
\begin{verbatim}
let x = (\y -> y+z)
\end{verbatim}
This gets mangled to
\begin{verbatim}
let-closure x = [z] [y] (y+z)
\end{verbatim}
The idea is that we compile code for @(y+z)@ in an environment in which
@z@ is bound to an offset from \tr{Node}, and @y@ is bound to an
offset from the stack pointer.

(A let-closure is an @StgLet@ with a @StgRhsClosure@ RHS.)

\item
\begin{verbatim}
let-constructor x = Constructor [args]
in e
\end{verbatim}

(A let-constructor is an @StgLet@ with a @StgRhsCon@ RHS.)

\item
Letrec-expressions are essentially the same deal as
let-closure/let-constructor, so we use a common structure and
distinguish between them with an @is_recursive@ boolean flag.

\item
\begin{verbatim}
let-unboxed u = an arbitrary arithmetic expression in unboxed values
in e
\end{verbatim}
All the stuff on the RHS must be fully evaluated.  No function calls either!

(We've backed away from this toward case-expressions with
suitably-magical alts ...)

\item
~[Advanced stuff here!  Not to start with, but makes pattern matching
generate more efficient code.]

\begin{verbatim}
let-escapes-not fail = expr
in e'
\end{verbatim}
Here the idea is that @e'@ guarantees not to put @fail@ in a data structure,
or pass it to another function.  All @e'@ will ever do is tail-call @fail@.
Rather than build a closure for @fail@, all we need do is to record the stack
level at the moment of the @let-escapes-not@; then entering @fail@ is just
a matter of adjusting the stack pointer back down to that point and entering
the code for it.

Another example:
\begin{verbatim}
f x y = let z = huge-expression in
	if y==1 then z else
	if y==2 then z else
	1
\end{verbatim}

(A let-escapes-not is an @StgLetNoEscape@.)

\item
We may eventually want:
\begin{verbatim}
let-literal x = Literal
in e
\end{verbatim}

(ToDo: is this obsolete?)
\end{enumerate}

And so the code for let(rec)-things:
\begin{code}
  | StgLet
	(GenStgBinding bndr occ)	-- right hand sides (see below)
	(GenStgExpr bndr occ)		-- body

  | StgLetNoEscape			-- remember: ``advanced stuff''
	(GenStgLiveVars occ)		-- Live in the whole let-expression
					-- Mustn't overwrite these stack slots
    	    	    	    	    	-- *Doesn't* include binders of the let(rec).

	(GenStgLiveVars occ)		-- Live in the right hand sides (only)
					-- These are the ones which must be saved on
					-- the stack if they aren't there already
    	    	    	    	    	-- *Does* include binders of the let(rec) if recursive.

	(GenStgBinding bndr occ)	-- right hand sides (see below)
	(GenStgExpr bndr occ)		-- body
\end{code}

%************************************************************************
%*									*
\subsubsection{@GenStgExpr@: @scc@ expressions}
%*									*
%************************************************************************

Finally for @scc@ expressions we introduce a new STG construct.

\begin{code}
  | StgSCC
	Type			-- the type of the body
	CostCentre		-- label of SCC expression
	(GenStgExpr bndr occ)	-- scc expression
  -- end of GenStgExpr
\end{code}

%************************************************************************
%*									*
\subsection{STG right-hand sides}
%*									*
%************************************************************************

Here's the rest of the interesting stuff for @StgLet@s; the first
flavour is for closures:
\begin{code}
data GenStgRhs bndr occ
  = StgRhsClosure
	CostCentre		-- cost centre to be attached (default is CCC)
	StgBinderInfo		-- Info about how this binder is used (see below)
	[occ]			-- non-global free vars; a list, rather than
				-- a set, because order is important
	UpdateFlag		-- ReEntrant | Updatable | SingleEntry
	[bndr]			-- arguments; if empty, then not a function;
				-- as above, order is important
	(GenStgExpr bndr occ)	-- body
\end{code}
An example may be in order.  Consider:
\begin{verbatim}
let t = \x -> \y -> ... x ... y ... p ... q in e
\end{verbatim}
Pulling out the free vars and stylising somewhat, we get the equivalent:
\begin{verbatim}
let t = (\[p,q] -> \[x,y] -> ... x ... y ... p ...q) p q
\end{verbatim}
Stg-operationally, the @[x,y]@ are on the stack, the @[p,q]@ are
offsets from @Node@ into the closure, and the code ptr for the closure
will be exactly that in parentheses above.

The second flavour of right-hand-side is for constructors (simple but important):
\begin{code}
  | StgRhsCon
	CostCentre		-- Cost centre to be attached (default is CCC).
				-- Top-level (static) ones will end up with
				-- DontCareCC, because we don't count static
				-- data in heap profiles, and we don't set CCC
				-- from static closure.
	Id			-- constructor
	[GenStgArg occ]	-- args
\end{code}

Here's the @StgBinderInfo@ type, and its combining op:
\begin{code}
data StgBinderInfo
  = NoStgBinderInfo
  | StgBinderInfo
	Bool		-- At least one occurrence as an argument

	Bool		-- At least one occurrence in an unsaturated application

	Bool		-- This thing (f) has at least occurrence of the form:
			--    x = [..] \u [] -> f a b c
			-- where the application is saturated

	Bool		-- Ditto for non-updatable x.

	Bool		-- At least one fake application occurrence, that is
			-- an StgApp f args where args is an empty list
			-- This is due to the fact that we do not have a
			-- StgVar constructor.
			-- Used by the lambda lifter.
			-- True => "at least one unsat app" is True too

stgArgOcc        = StgBinderInfo True  False False False False
stgUnsatOcc      = StgBinderInfo False True  False False False
stgStdHeapOcc    = StgBinderInfo False False True  False False
stgNoUpdHeapOcc  = StgBinderInfo False False False True  False
stgNormalOcc     = StgBinderInfo False False False False False
-- [Andre] can't think of a good name for the last one.
stgFakeFunAppOcc = StgBinderInfo False True  False False True

combineStgBinderInfo :: StgBinderInfo -> StgBinderInfo -> StgBinderInfo

combineStgBinderInfo NoStgBinderInfo info2 = info2
combineStgBinderInfo info1 NoStgBinderInfo = info1
combineStgBinderInfo (StgBinderInfo arg1 unsat1 std_heap1 upd_heap1 fkap1)
		     (StgBinderInfo arg2 unsat2 std_heap2 upd_heap2 fkap2)
  = StgBinderInfo (arg1      || arg2)
		  (unsat1    || unsat2)
		  (std_heap1 || std_heap2)
		  (upd_heap1 || upd_heap2)
		  (fkap1     || fkap2)
\end{code}

%************************************************************************
%*									*
\subsection[Stg-case-alternatives]{STG case alternatives}
%*									*
%************************************************************************

Just like in @CoreSyntax@ (except no type-world stuff).

\begin{code}
data GenStgCaseAlts bndr occ
  = StgAlgAlts	Type	-- so we can find out things about constructor family
		[(Id,				-- alts: data constructor,
		  [bndr],			-- constructor's parameters,
		  [Bool],			-- "use mask", same length as
						-- parameters; a True in a
						-- param's position if it is
						-- used in the ...
		  GenStgExpr bndr occ)]	-- ...right-hand side.
		(GenStgCaseDefault bndr occ)
  | StgPrimAlts	Type	-- so we can find out things about constructor family
		[(Literal,			-- alts: unboxed literal,
		  GenStgExpr bndr occ)]	-- rhs.
		(GenStgCaseDefault bndr occ)

data GenStgCaseDefault bndr occ
  = StgNoDefault				-- small con family: all
						-- constructor accounted for
  | StgBindDefault  bndr			-- form: var -> expr
		    Bool			-- True <=> var is used in rhs
						-- i.e., False <=> "_ -> expr"
		    (GenStgExpr bndr occ)
\end{code}

%************************************************************************
%*									*
\subsection[Stg]{The Plain STG parameterisation}
%*									*
%************************************************************************

This happens to be the only one we use at the moment.

\begin{code}
type StgBinding     = GenStgBinding	Id Id
type StgArg         = GenStgArg		Id
type StgLiveVars    = GenStgLiveVars	Id
type StgExpr        = GenStgExpr	Id Id
type StgRhs         = GenStgRhs		Id Id
type StgCaseAlts    = GenStgCaseAlts	Id Id
type StgCaseDefault = GenStgCaseDefault	Id Id
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection[UpdateFlag-datatype]{@UpdateFlag@}
%*                                                                      *
%************************************************************************

This is also used in @LambdaFormInfo@ in the @ClosureInfo@ module.

\begin{code}
data UpdateFlag = ReEntrant | Updatable | SingleEntry

instance Outputable UpdateFlag where
    ppr u
      = char (case u of { ReEntrant -> 'r';  Updatable -> 'u';  SingleEntry -> 's' })
\end{code}

%************************************************************************
%*									*
\subsection[Stg-utility-functions]{Utility functions}
%*									*
%************************************************************************


For doing interfaces, we want the exported top-level Ids from the
final pre-codegen STG code, so as to be sure we have the
latest/greatest pragma info.

\begin{code}
collectFinalStgBinders
	:: [StgBinding]	-- input program
	-> [Id]

collectFinalStgBinders [] = []
collectFinalStgBinders (StgNonRec b _ : binds) = b : collectFinalStgBinders binds
collectFinalStgBinders (StgRec bs     : binds) = map fst bs ++ collectFinalStgBinders binds
\end{code}

%************************************************************************
%*									*
\subsection[Stg-pretty-printing]{Pretty-printing}
%*									*
%************************************************************************

Robin Popplestone asked for semi-colon separators on STG binds; here's
hoping he likes terminators instead...  Ditto for case alternatives.

\begin{code}
pprGenStgBinding :: (Outputable bndr, Outputable bdee, Ord bdee)
		 => GenStgBinding bndr bdee -> SDoc

pprGenStgBinding (StgNonRec bndr rhs)
  = hang (hsep [ppr bndr, equals])
    	 4 ((<>) (ppr rhs) semi)

pprGenStgBinding (StgRec pairs)
  = vcat ((ifPprDebug (ptext SLIT("{- StgRec (begin) -}"))) :
	      (map (ppr_bind) pairs) ++ [(ifPprDebug (ptext SLIT("{- StgRec (end) -}")))])
  where
    ppr_bind (bndr, expr)
      = hang (hsep [ppr bndr, equals])
	     4 ((<>) (ppr expr) semi)

pprStgBinding  :: StgBinding -> SDoc
pprStgBinding  bind  = pprGenStgBinding bind

pprStgBindings :: [StgBinding] -> SDoc
pprStgBindings binds = vcat (map (pprGenStgBinding) binds)
\end{code}

\begin{code}
instance (Outputable bdee) => Outputable (GenStgArg bdee) where
    ppr = pprStgArg

instance (Outputable bndr, Outputable bdee, Ord bdee)
		=> Outputable (GenStgBinding bndr bdee) where
    ppr = pprGenStgBinding

instance (Outputable bndr, Outputable bdee, Ord bdee)
		=> Outputable (GenStgExpr bndr bdee) where
    ppr = pprStgExpr

instance (Outputable bndr, Outputable bdee, Ord bdee)
		=> Outputable (GenStgRhs bndr bdee) where
    ppr rhs = pprStgRhs rhs
\end{code}

\begin{code}
pprStgArg :: (Outputable bdee) => GenStgArg bdee -> SDoc

pprStgArg (StgVarArg var) = ppr var
pprStgArg (StgConArg con) = ppr con
pprStgArg (StgLitArg lit) = ppr lit
\end{code}

\begin{code}
pprStgExpr :: (Outputable bndr, Outputable bdee, Ord bdee)
	   => GenStgExpr bndr bdee -> SDoc
-- special case
pprStgExpr (StgApp func [] lvs)
  = (<>) (ppr func) (pprStgLVs lvs)

-- general case
pprStgExpr (StgApp func args lvs)
  = hang ((<>) (ppr func) (pprStgLVs lvs))
	 4 (sep (map (ppr) args))
\end{code}

\begin{code}
pprStgExpr (StgCon con args lvs)
  = hcat [ (<>) (ppr con) (pprStgLVs lvs),
		ptext SLIT("! ["), interppSP args, char ']' ]

pprStgExpr (StgPrim op args lvs)
  = hcat [ ppr op, char '#', pprStgLVs lvs,
		ptext SLIT(" ["), interppSP args, char ']' ]
\end{code}

\begin{code}
-- special case: let v = <very specific thing>
--		 in
--		 let ...
--		 in
--		 ...
--
-- Very special!  Suspicious! (SLPJ)

pprStgExpr (StgLet (StgNonRec bndr (StgRhsClosure cc bi free_vars upd_flag args rhs))
		    	expr@(StgLet _ _))
  = ($$)
      (hang (hcat [ptext SLIT("let { "), ppr bndr, ptext SLIT(" = "),
			  text (showCostCentre True{-as string-} cc),
			  pp_binder_info bi,
			  ptext SLIT(" ["), ifPprDebug (interppSP free_vars), ptext SLIT("] \\"),
			  ppr upd_flag, ptext SLIT(" ["),
			  interppSP args, char ']'])
	    8 (sep [hsep [ppr rhs, ptext SLIT("} in")]]))
      (ppr expr)

-- special case: let ... in let ...

pprStgExpr (StgLet bind expr@(StgLet _ _))
  = ($$)
      (sep [hang (ptext SLIT("let {")) 2 (hsep [pprGenStgBinding bind, ptext SLIT("} in")])])
      (ppr expr)

-- general case
pprStgExpr (StgLet bind expr)
  = sep [hang (ptext SLIT("let {")) 2 (pprGenStgBinding bind),
	   hang (ptext SLIT("} in ")) 2 (ppr expr)]

pprStgExpr (StgLetNoEscape lvs_whole lvs_rhss bind expr)
  = sep [hang (ptext SLIT("let-no-escape {"))
	    	2 (pprGenStgBinding bind),
	   hang ((<>) (ptext SLIT("} in "))
		   (ifPprDebug (
		    nest 4 (
		      hcat [ptext  SLIT("-- lvs: ["), interppSP (uniqSetToList lvs_whole),
			     ptext SLIT("]; rhs lvs: ["), interppSP (uniqSetToList lvs_rhss),
			     char ']']))))
		2 (ppr expr)]
\end{code}

\begin{code}
pprStgExpr (StgSCC ty cc expr)
  = sep [ hsep [ptext SLIT("_scc_"), text (showCostCentre True{-as string-} cc)],
	    pprStgExpr expr ]
\end{code}

\begin{code}
pprStgExpr (StgCase expr lvs_whole lvs_rhss uniq alts)
  = sep [sep [ptext SLIT("case"),
	   nest 4 (hsep [pprStgExpr expr,
	     ifPprDebug (ptext SLIT("::") <> pp_ty alts)]),
	   ptext SLIT("of {")],
	   ifPprDebug (
	   nest 4 (
	     hcat [ptext  SLIT("-- lvs: ["), interppSP (uniqSetToList lvs_whole),
		    ptext SLIT("]; rhs lvs: ["), interppSP (uniqSetToList lvs_rhss),
		    ptext SLIT("]; uniq: "), pprUnique uniq])),
	   nest 2 (ppr_alts alts),
	   char '}']
  where
    ppr_default StgNoDefault = empty
    ppr_default (StgBindDefault bndr used expr)
      = hang (hsep [pp_binder, ptext SLIT("->")]) 4 (ppr expr)
      where
    	pp_binder = if used then ppr bndr else char '_'

    pp_ty (StgAlgAlts  ty _ _) = ppr ty
    pp_ty (StgPrimAlts ty _ _) = ppr ty

    ppr_alts (StgAlgAlts ty alts deflt)
      = vcat [ vcat (map (ppr_bxd_alt) alts),
		   ppr_default deflt ]
      where
	ppr_bxd_alt (con, params, use_mask, expr)
	  = hang (hsep [ppr con, interppSP params, ptext SLIT("->")])
		   4 ((<>) (ppr expr) semi)

    ppr_alts (StgPrimAlts ty alts deflt)
      = vcat [ vcat (map (ppr_ubxd_alt) alts),
		   ppr_default deflt ]
      where
	ppr_ubxd_alt (lit, expr)
	  = hang (hsep [ppr lit, ptext SLIT("->")])
		 4 ((<>) (ppr expr) semi)
\end{code}

\begin{code}
pprStgLVs :: Outputable occ => GenStgLiveVars occ -> SDoc
pprStgLVs lvs
  = getPprStyle $ \ sty ->
    if userStyle sty || isEmptyUniqSet lvs then
	empty
    else
	hcat [text "{-lvs:", interpp'SP (uniqSetToList lvs), text "-}"]
\end{code}

\begin{code}
pprStgRhs :: (Outputable bndr, Outputable bdee, Ord bdee)
	  => GenStgRhs bndr bdee -> SDoc

-- special case
pprStgRhs (StgRhsClosure cc bi [free_var] upd_flag [{-no args-}] (StgApp func [] lvs))
  = hcat [ text (showCostCentre True{-as String-} cc),
	   pp_binder_info bi,
	   brackets (ifPprDebug (ppr free_var)),
	   ptext SLIT(" \\"), ppr upd_flag, ptext SLIT(" [] "), ppr func ]

-- general case
pprStgRhs (StgRhsClosure cc bi free_vars upd_flag args body)
  = hang (hcat [text (showCostCentre True{-as String-} cc),
		pp_binder_info bi,
		brackets (ifPprDebug (interppSP free_vars)),
		ptext SLIT(" \\"), ppr upd_flag, brackets (interppSP args)])
	 4 (ppr body)

pprStgRhs (StgRhsCon cc con args)
  = hcat [ text (showCostCentre True{-as String-} cc),
	   space, ppr con, ptext SLIT("! "), brackets (interppSP args)]

--------------

pp_binder_info NoStgBinderInfo = empty

-- cases so boring that we print nothing
pp_binder_info (StgBinderInfo True b c d e) = empty

-- general case
pp_binder_info (StgBinderInfo a b c d e)
  = getPprStyle $ \ sty -> 
    if userStyle sty then
       empty
    else
       parens (hsep (punctuate comma (map ppr [a,b,c,d,e])))
\end{code}

Collect @IdInfo@ stuff that is most easily just snaffled straight
from the STG bindings.

\begin{code}
stgArity :: StgRhs -> Int

stgArity (StgRhsCon _ _ _)	         = 0 -- it's a constructor, fully applied
stgArity (StgRhsClosure _ _ _ _ args _ ) = length args
\end{code}
