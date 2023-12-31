%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CostCentre]{The @CostCentre@ data type}

\begin{code}
module CostCentre (
	CostCentre(..), CcName, IsDupdCC(..), IsCafCC(..),
		-- All abstract except to friend: ParseIface.y

	CostCentreStack,
	noCCS, subsumedCCS, currentCCS, overheadCCS, dontCareCCS,
	noCostCentre, noCCAttached,
	noCCSAttached, isCurrentCCS,  isSubsumedCCS, currentOrSubsumedCCS,

	mkUserCC, mkAutoCC, mkAllCafsCC, 
	mkSingletonCCS, cafifyCC, dupifyCC,
	isCafCC, isDupdCC, isEmptyCC, isCafCCS,
	isSccCountCostCentre,
	sccAbleCostCentre,
	ccFromThisModule,

	pprCostCentreDecl, pprCostCentreStackDecl, pprCostCentreCore,

	cmpCostCentre	-- used for removing dups in a list
    ) where

#include "HsVersions.h"

import Var		( Id )
import Name		( UserFS, EncodedFS, encodeFS, decode,
			  getOccName, occNameFS
			)
import Module		( Module, ModuleName, moduleName,
			  pprModuleName, moduleNameUserString
			)
import Outputable	
import CStrings		( pprStringInCStyle )
import Util	        ( thenCmp )
\end{code}

A Cost Centre Stack is something that can be attached to a closure.
This is either:
	
	- the current cost centre stack (CCCS)
	- a pre-defined cost centre stack (there are several
	  pre-defined CCSs, see below).

\begin{code}
data CostCentreStack
  = NoCCS

  | CurrentCCS		-- Pinned on a let(rec)-bound 
			-- thunk/function/constructor, this says that the 
			-- cost centre to be attached to the object, when it 
			-- is allocated, is whatever is in the 
			-- current-cost-centre-stack register.

  | SubsumedCCS		-- Cost centre stack for top-level subsumed functions
			-- (CAFs get an AllCafsCC).
			-- Its execution costs get subsumed into the caller.
			-- This guy is *only* ever pinned on static closures,
			-- and is *never* the cost centre for an SCC construct.

  | OverheadCCS		-- We charge costs due to the profiling-system
  			-- doing its work to "overhead".
			--
			-- Objects whose CCS is "Overhead"
			-- have their *allocation* charged to "overhead",
			-- but have the current CCS put into the object
			-- itself.

			-- For example, if we transform "f g" to "let
			-- g' = g in f g'" (so that something about
			-- profiling works better...), then we charge
			-- the *allocation* of g' to OverheadCCS, but
			-- we put the cost-centre of the call to f
			-- (i.e., current CCS) into the g' object.  When
			-- g' is entered, the CCS of the call
			-- to f will be set.

  | DontCareCCS		-- We need a CCS to stick in static closures
			-- (for data), but we *don't* expect them to
			-- accumulate any costs.  But we still need
			-- the placeholder.  This CCS is it.

  | SingletonCCS CostCentre
			-- This is primarily for CAF cost centres, which
			-- are attached to top-level thunks right at the
			-- end of STG processing, before code generation.
			-- Hence, a CAF cost centre never appears as the
			-- argument of an _scc_.
			-- Also, we generate these singleton CCSs statically
			-- as part of code generation.

  deriving (Eq, Ord)	-- needed for Ord on CLabel
\end{code}

A Cost Centre is the argument of an _scc_ expression.
 
\begin{code}
data CostCentre
  = NoCostCentre	-- Having this constructor avoids having
			-- to use "Maybe CostCentre" all the time.

  | NormalCC {  
		cc_name :: CcName,	-- Name of the cost centre itself
		cc_mod  :: ModuleName,	-- Name of module defining this CC.
		cc_is_dupd :: IsDupdCC,	-- see below
		cc_is_caf  :: IsCafCC	-- see below
    }

  | AllCafsCC {	
		cc_mod  :: ModuleName	-- Name of module defining this CC.
    }

type CcName = EncodedFS

data IsDupdCC
  = OriginalCC	-- This says how the CC is *used*.  Saying that
  | DupdCC		-- it is DupdCC doesn't make it a different
			-- CC, just that it a sub-expression which has
			-- been moved ("dupd") into a different scope.
			--
			-- The point about a dupd SCC is that we don't
			-- count entries to it, because it's not the
			-- "original" one.
			--
			-- In the papers, it's called "SCCsub",
			--  i.e. SCCsub CC == SCC DupdCC,
			-- but we are trying to avoid confusion between
			-- "subd" and "subsumed".  So we call the former
			-- "dupd".

data IsCafCC = CafCC | NotCafCC
\end{code}

WILL: Would there be any merit to recording ``I am now using a
cost-centre from another module''?  I don't know if this would help a
user; it might be interesting to us to know how much computation is
being moved across module boundaries.

SIMON: Maybe later...

\begin{code}

noCCS 			= NoCCS
subsumedCCS 		= SubsumedCCS
currentCCS	 	= CurrentCCS
overheadCCS	 	= OverheadCCS
dontCareCCS	 	= DontCareCCS

noCostCentre  		= NoCostCentre
\end{code}

Predicates on Cost-Centre Stacks

\begin{code}
noCCSAttached NoCCS			= True
noCCSAttached _				= False

noCCAttached NoCostCentre		= True
noCCAttached _				= False

isCurrentCCS CurrentCCS			= True
isCurrentCCS _	      			= False

isSubsumedCCS SubsumedCCS 		= True
isSubsumedCCS _		     		= False

isCafCCS (SingletonCCS cc)		= isCafCC cc
isCafCCS _				= False

currentOrSubsumedCCS SubsumedCCS	= True
currentOrSubsumedCCS CurrentCCS		= True
currentOrSubsumedCCS _			= False
\end{code}

Building cost centres

\begin{code}
mkUserCC :: UserFS -> Module -> CostCentre

mkUserCC cc_name mod
  = NormalCC { cc_name = encodeFS cc_name, cc_mod =  moduleName mod,
	       cc_is_dupd = OriginalCC, cc_is_caf = NotCafCC {-might be changed-}
    }

mkAutoCC :: Id -> Module -> IsCafCC -> CostCentre

mkAutoCC id mod is_caf
  = NormalCC { cc_name = occNameFS (getOccName id), cc_mod =  moduleName mod,
	       cc_is_dupd = OriginalCC, cc_is_caf = is_caf
    }

mkAllCafsCC m = AllCafsCC  { cc_mod = moduleName m }

mkSingletonCCS :: CostCentre -> CostCentreStack
mkSingletonCCS cc = SingletonCCS cc

cafifyCC, dupifyCC  :: CostCentre -> CostCentre

cafifyCC cc@(NormalCC {cc_is_caf = is_caf})
  = ASSERT(not_a_caf_already is_caf)
    cc {cc_is_caf = CafCC}
  where
    not_a_caf_already CafCC = False
    not_a_caf_already _       = True
cafifyCC cc = pprPanic "cafifyCC" (ppr cc)

dupifyCC cc = cc {cc_is_dupd = DupdCC}

isEmptyCC, isCafCC, isDupdCC :: CostCentre -> Bool

isEmptyCC (NoCostCentre)		= True
isEmptyCC _				= False

isCafCC (AllCafsCC {})		         = True
isCafCC (NormalCC {cc_is_caf = CafCC}) = True
isCafCC _		                 = False

isDupdCC (NormalCC   {cc_is_dupd = DupdCC}) = True
isDupdCC _		                     = False

isSccCountCostCentre :: CostCentre -> Bool
  -- Is this a cost-centre which records scc counts

#if DEBUG
isSccCountCostCentre NoCostCentre  = panic "isSccCount:NoCostCentre"
#endif
isSccCountCostCentre cc | isCafCC cc  = False
                        | isDupdCC cc = False
			| otherwise   = True

sccAbleCostCentre :: CostCentre -> Bool
  -- Is this a cost-centre which can be sccd ?

#if DEBUG
sccAbleCostCentre NoCostCentre  = panic "sccAbleCC:NoCostCentre"
#endif
sccAbleCostCentre cc | isCafCC cc = False
		     | otherwise  = True

ccFromThisModule :: CostCentre -> Module -> Bool
ccFromThisModule cc m = cc_mod cc == moduleName m
\end{code}

\begin{code}
instance Eq CostCentre where
	c1 == c2 = case c1 `cmpCostCentre` c2 of { EQ -> True; _ -> False }

instance Ord CostCentre where
	compare = cmpCostCentre

cmpCostCentre :: CostCentre -> CostCentre -> Ordering

cmpCostCentre (AllCafsCC  {cc_mod = m1}) (AllCafsCC  {cc_mod = m2}) = m1 `compare` m2

cmpCostCentre (NormalCC {cc_name = n1, cc_mod =  m1, cc_is_caf = c1}) 
	      (NormalCC {cc_name = n2, cc_mod =  m2, cc_is_caf = c2}) 
    -- first key is module name, then we use "kinds" (which include
    -- names) and finally the caf flag
  = (m1 `compare` m2) `thenCmp` (n1 `compare` n2) `thenCmp` (c1 `cmp_caf` c2)

cmpCostCentre other_1 other_2
  = let
	tag1 = tag_CC other_1
	tag2 = tag_CC other_2
    in
    if tag1 _LT_ tag2 then LT else GT
  where
    tag_CC (NormalCC   {}) = (ILIT(1) :: FAST_INT)
    tag_CC (AllCafsCC  {}) = ILIT(2)

cmp_caf NotCafCC CafCC     = LT
cmp_caf NotCafCC NotCafCC  = EQ
cmp_caf CafCC    CafCC     = EQ
cmp_caf CafCC    NotCafCC  = GT
\end{code}

-----------------------------------------------------------------------------
Printing Cost Centre Stacks.

There are two ways to print a CCS:

	- for debugging output (i.e. -ddump-whatever),
	- as a C label

\begin{code}
instance Outputable CostCentreStack where
  ppr ccs = case ccs of
		NoCCS		-> ptext SLIT("NO_CCS")
		CurrentCCS	-> ptext SLIT("CCCS")
		OverheadCCS	-> ptext SLIT("CCS_OVERHEAD")
		DontCareCCS	-> ptext SLIT("CCS_DONT_CARE")
		SubsumedCCS	-> ptext SLIT("CCS_SUBSUMED")
		SingletonCCS cc -> ppr cc <> ptext SLIT("_ccs")

pprCostCentreStackDecl :: CostCentreStack -> SDoc
pprCostCentreStackDecl ccs@(SingletonCCS cc)
  = hcat [ ptext SLIT("CCS_DECLARE"), char '(',
    	   ppr ccs,	 	comma,	-- better be codeStyle
    	   ppCostCentreLbl cc, 	comma,
	   empty,	-- Now always externally visible
    	   text ");"
	 ]

pprCostCentreStackDecl ccs 
  = pprPanic "pprCostCentreStackDecl: " (ppr ccs)
\end{code}

-----------------------------------------------------------------------------
Printing Cost Centres.

There are several different ways in which we might want to print a
cost centre:

	- the name of the cost centre, for profiling output (a C string)
	- the label, i.e. C label for cost centre in .hc file.
	- the debugging name, for output in -ddump things
	- the interface name, for printing in _scc_ exprs in iface files.

The last 3 are derived from costCentreStr below.  The first is given
by costCentreName.

\begin{code}
instance Outputable CostCentre where
  ppr cc = getPprStyle $ \ sty ->
	   if codeStyle sty
  	   then ppCostCentreLbl cc
	   else text (costCentreUserName cc)

-- Printing in an interface file or in Core generally
pprCostCentreCore (AllCafsCC {cc_mod = m})
  = text "__sccC" <+> braces (pprModuleName m)
pprCostCentreCore (NormalCC {cc_name = n, cc_mod = m,
			     cc_is_caf = caf, cc_is_dupd = dup})
  = text "__scc" <+> braces (hsep [
	ptext n,
	pprModuleName m,	
	pp_dup dup,
	pp_caf caf
    ])

pp_dup DupdCC = char '!'
pp_dup other   = empty

pp_caf CafCC = text "__C"
pp_caf other   = empty


-- Printing as a C label
ppCostCentreLbl (NoCostCentre)		  = text "NONE_cc"
ppCostCentreLbl (AllCafsCC  {cc_mod = m}) = pprModuleName m <> text "_CAFs_cc"
ppCostCentreLbl (NormalCC {cc_name = n, cc_mod = m, cc_is_caf = is_caf}) 
  = pprModuleName m <> ptext n <> 
	text (case is_caf of { CafCC -> "_CAF"; _ -> "" }) <> text "_cc"

-- This is the name to go in the user-displayed string, 
-- recorded in the cost centre declaration
costCentreUserName (NoCostCentre)  = "NO_CC"
costCentreUserName (AllCafsCC {})  = "CAF"
costCentreUserName cc@(NormalCC {cc_name = name, cc_is_caf = is_caf})
  =  case is_caf of { CafCC -> "CAF:";   _ -> "" } ++ decode (_UNPK_ name)
\end{code}

Cost Centre Declarations

\begin{code}
#ifdef DEBUG
pprCostCentreDecl is_local (NoCostCentre)
  = panic "pprCostCentreDecl: no cost centre!"
#endif
pprCostCentreDecl is_local cc
  = if is_local then
	hcat [
	    ptext SLIT("CC_DECLARE"),char '(',
	    cc_ident, 		  					comma,
	    pprStringInCStyle (costCentreUserName cc),			comma,
	    pprStringInCStyle (moduleNameUserString mod_name),		comma,
	    ptext is_subsumed,						comma,
	    empty,	-- Now always externally visible
	    text ");"]
    else
	hcat [ ptext SLIT("CC_EXTERN"),char '(', cc_ident, text ");" ]
  where
    cc_ident    = ppCostCentreLbl cc
    mod_name 	= cc_mod cc
    is_subsumed = ccSubsumed cc

ccSubsumed :: CostCentre -> FAST_STRING		-- subsumed value
ccSubsumed cc | isCafCC  cc = SLIT("CC_IS_CAF")
	      | otherwise   = SLIT("CC_IS_BORING")
\end{code}
