%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreSyn]{A data type for the Haskell compiler midsection}

\begin{code}
module CoreSyn (
	Expr(..), Alt, Bind(..), AltCon(..), Arg, Note(..),
	CoreExpr, CoreAlt, CoreBind, CoreArg, CoreBndr,
	TaggedExpr, TaggedAlt, TaggedBind, TaggedArg,

	mkLets, mkLams, 
	mkApps, mkTyApps, mkValApps, mkVarApps,
	mkLit, mkIntLitInt, mkIntLit, 
	mkStringLit, mkStringLitFS, mkConApp, 
	varToCoreExpr,

	bindersOf, bindersOfBinds, rhssOfBind, rhssOfAlts, isTyVar, isId,
	collectBinders, collectTyBinders, collectValBinders, collectTyAndValBinders,
	collectArgs, collectBindersIgnoringNotes,
	coreExprCc,
	flattenBinds, 

	isValArg, isTypeArg, valArgCount, valBndrCount,

	-- Unfoldings
	Unfolding(..),	UnfoldingGuidance(..), 	-- Both abstract everywhere but in CoreUnfold.lhs
	noUnfolding, mkOtherCon,
	unfoldingTemplate, maybeUnfoldingTemplate, otherCons, 
	isValueUnfolding, isEvaldUnfolding, isCheapUnfolding, isCompulsoryUnfolding,
	hasUnfolding, hasSomeUnfolding,

	-- Seq stuff
	seqRules, seqExpr, seqExprs, seqUnfolding,

	-- Annotated expressions
	AnnExpr, AnnExpr'(..), AnnBind(..), AnnAlt, deAnnotate, deAnnotate',

	-- Core rules
	CoreRules(..), 	-- Representation needed by friends
	CoreRule(..),	-- CoreSubst, CoreTidy, CoreFVs, PprCore only
	RuleName,
	emptyCoreRules, isEmptyCoreRules, rulesRhsFreeVars, rulesRules
    ) where

#include "HsVersions.h"

import CostCentre	( CostCentre, noCostCentre )
import Var		( Var, Id, TyVar, isTyVar, isId, idType )
import VarEnv
import Type		( Type, UsageAnn, mkTyVarTy, isUnLiftedType, seqType )
import Literal	        ( Literal(MachStr), mkMachInt )
import PrimOp		( PrimOp )
import DataCon		( DataCon, dataConId )
import ThinAir		( unpackCStringId, unpackCString2Id )
import VarSet
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{The main data types}
%*									*
%************************************************************************

These data types are the heart of the compiler

\begin{code}
infixl 8 `App`	-- App brackets to the left

data Expr b	-- "b" for the type of binders, 
  = Var	  Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b [Alt b]  	-- Binder gets bound to value of scrutinee
				-- DEFAULT case must be last, if it occurs at all
  | Note  Note (Expr b)
  | Type  Type			-- This should only show up at the top
				-- level of an Arg

type Arg b = Expr b		-- Can be a Type

type Alt b = (AltCon, [b], Expr b)	-- (DEFAULT, [], rhs) is the default alternative

data AltCon = DataAlt DataCon
	    | LitAlt  Literal
	    | DEFAULT
	 deriving (Eq, Ord)

data Bind b = NonRec b (Expr b)
	      | Rec [(b, (Expr b))]

data Note
  = SCC CostCentre

  | Coerce	
	Type		-- The to-type:   type of whole coerce expression
	Type		-- The from-type: type of enclosed expression

  | InlineCall		-- Instructs simplifier to inline
			-- the enclosed call

  | InlineMe		-- Instructs simplifer to treat the enclosed expression
			-- as very small, and inline it at its call sites

  | TermUsg             -- A term-level usage annotation
        UsageAnn        -- (should not be a variable except during UsageSP inference)
\end{code}


%************************************************************************
%*									*
\subsection{Transformation rules}
%*									*
%************************************************************************

The CoreRule type and its friends are dealt with mainly in CoreRules,
but CoreFVs, Subst, PprCore, CoreTidy also inspect the representation.

\begin{code}
data CoreRules 
  = Rules [CoreRule]
	  VarSet		-- Locally-defined free vars of RHSs

type RuleName = FAST_STRING

data CoreRule
  = Rule RuleName
	 [CoreBndr]	-- Forall'd variables
	 [CoreExpr]	-- LHS args
	 CoreExpr	-- RHS

  | BuiltinRule		-- Built-in rules are used for constant folding
			-- and suchlike.  It has no free variables.
	([CoreExpr] -> Maybe (RuleName, CoreExpr))

emptyCoreRules :: CoreRules
emptyCoreRules = Rules [] emptyVarSet

isEmptyCoreRules :: CoreRules -> Bool
isEmptyCoreRules (Rules rs _) = null rs

rulesRhsFreeVars :: CoreRules -> VarSet
rulesRhsFreeVars (Rules _ fvs) = fvs

rulesRules :: CoreRules -> [CoreRule]
rulesRules (Rules rules _) = rules
\end{code}


%************************************************************************
%*									*
\subsection{@Unfolding@ type}
%*									*
%************************************************************************

The @Unfolding@ type is declared here to avoid numerous loops, but it
should be abstract everywhere except in CoreUnfold.lhs

\begin{code}
data Unfolding
  = NoUnfolding

  | OtherCon [AltCon]		-- It ain't one of these
				-- (OtherCon xs) also indicates that something has been evaluated
				-- and hence there's no point in re-evaluating it.
				-- OtherCon [] is used even for non-data-type values
				-- to indicated evaluated-ness.  Notably:
				--	data C = C !(Int -> Int)
				-- 	case x of { C f -> ... }
				-- Here, f gets an OtherCon [] unfolding.

  | CompulsoryUnfolding CoreExpr	-- There is no "original" definition,
					-- so you'd better unfold.

  | CoreUnfolding			-- An unfolding with redundant cached information
		CoreExpr		-- Template; binder-info is correct
		Bool			-- This is a top-level binding
		Bool			-- exprIsCheap template (cached); it won't duplicate (much) work 
					--	if you inline this in more than one place
		Bool			-- exprIsValue template (cached); it is ok to discard a `seq` on
					--	this variable
		Bool			-- exprIsBottom template (cached)
		UnfoldingGuidance	-- Tells about the *size* of the template.


data UnfoldingGuidance
  = UnfoldNever
  | UnfoldIfGoodArgs	Int	-- and "n" value args

			[Int]	-- Discount if the argument is evaluated.
				-- (i.e., a simplification will definitely
				-- be possible).  One elt of the list per *value* arg.

			Int	-- The "size" of the unfolding; to be elaborated
				-- later. ToDo

			Int	-- Scrutinee discount: the discount to substract if the thing is in
				-- a context (case (thing args) of ...),
				-- (where there are the right number of arguments.)

noUnfolding = NoUnfolding
mkOtherCon  = OtherCon

seqUnfolding :: Unfolding -> ()
seqUnfolding (CoreUnfolding e top b1 b2 b3 g)
  = seqExpr e `seq` top `seq` b1 `seq` b2 `seq` b3 `seq` seqGuidance g
seqUnfolding other = ()

seqGuidance (UnfoldIfGoodArgs n ns a b) = n `seq` sum ns `seq` a `seq` b `seq` ()
seqGuidance other			= ()
\end{code}

\begin{code}
unfoldingTemplate :: Unfolding -> CoreExpr
unfoldingTemplate (CoreUnfolding expr _ _ _ _ _) = expr
unfoldingTemplate (CompulsoryUnfolding expr)     = expr
unfoldingTemplate other = panic "getUnfoldingTemplate"

maybeUnfoldingTemplate :: Unfolding -> Maybe CoreExpr
maybeUnfoldingTemplate (CoreUnfolding expr _ _ _ _ _) = Just expr
maybeUnfoldingTemplate (CompulsoryUnfolding expr)     = Just expr
maybeUnfoldingTemplate other 			      = Nothing

otherCons :: Unfolding -> [AltCon]
otherCons (OtherCon cons) = cons
otherCons other		  = []

isValueUnfolding :: Unfolding -> Bool
	-- Returns False for OtherCon
isValueUnfolding (CoreUnfolding _ _ _ is_evald _ _) = is_evald
isValueUnfolding other			            = False

isEvaldUnfolding :: Unfolding -> Bool
	-- Returns True for OtherCon
isEvaldUnfolding (OtherCon _)		            = True
isEvaldUnfolding (CoreUnfolding _ _ _ is_evald _ _) = is_evald
isEvaldUnfolding other			            = False

isCheapUnfolding :: Unfolding -> Bool
isCheapUnfolding (CoreUnfolding _ _ is_cheap _ _ _) = is_cheap
isCheapUnfolding other				    = False

isCompulsoryUnfolding :: Unfolding -> Bool
isCompulsoryUnfolding (CompulsoryUnfolding _) = True
isCompulsoryUnfolding other		      = False

hasUnfolding :: Unfolding -> Bool
hasUnfolding (CoreUnfolding _ _ _ _ _ _) = True
hasUnfolding (CompulsoryUnfolding _)     = True
hasUnfolding other 	 	         = False

hasSomeUnfolding :: Unfolding -> Bool
hasSomeUnfolding NoUnfolding = False
hasSomeUnfolding other	     = True
\end{code}


%************************************************************************
%*									*
\subsection{The main data type}
%*									*
%************************************************************************

\begin{code}
-- The Ord is needed for the FiniteMap used in the lookForConstructor
-- in SimplEnv.  If you declared that lookForConstructor *ignores*
-- constructor-applications with LitArg args, then you could get
-- rid of this Ord.

instance Outputable AltCon where
  ppr (DataAlt dc) = ppr dc
  ppr (LitAlt lit) = ppr lit
  ppr DEFAULT      = ptext SLIT("__DEFAULT")

instance Show AltCon where
  showsPrec p con = showsPrecSDoc p (ppr con)
\end{code}


%************************************************************************
%*									*
\subsection{Useful synonyms}
%*									*
%************************************************************************

The common case

\begin{code}
type CoreBndr = Var
type CoreExpr = Expr CoreBndr
type CoreArg  = Arg  CoreBndr
type CoreBind = Bind CoreBndr
type CoreAlt  = Alt  CoreBndr
type CoreNote = Note
\end{code}

Binders are ``tagged'' with a \tr{t}:

\begin{code}
type Tagged t = (CoreBndr, t)

type TaggedBind t = Bind (Tagged t)
type TaggedExpr t = Expr (Tagged t)
type TaggedArg  t = Arg  (Tagged t)
type TaggedAlt  t = Alt  (Tagged t)
\end{code}


%************************************************************************
%*									*
\subsection{Core-constructing functions with checking}
%*									*
%************************************************************************

\begin{code}
mkApps    :: Expr b -> [Arg b]  -> Expr b
mkTyApps  :: Expr b -> [Type]   -> Expr b
mkValApps :: Expr b -> [Expr b] -> Expr b
mkVarApps :: Expr b -> [Var] -> Expr b

mkApps    f args = foldl App		  	   f args
mkTyApps  f args = foldl (\ e a -> App e (Type a)) f args
mkValApps f args = foldl (\ e a -> App e a)	   f args
mkVarApps f vars = foldl (\ e a -> App e (varToCoreExpr a)) f vars

mkLit         :: Literal -> Expr b
mkIntLit      :: Integer -> Expr b
mkIntLitInt   :: Int     -> Expr b
mkStringLit   :: String  -> Expr b	-- Makes a [Char] literal
mkStringLitFS :: FAST_STRING  -> Expr b -- Makes a [Char] literal
mkConApp      :: DataCon -> [Arg b] -> Expr b
mkLets	      :: [Bind b] -> Expr b -> Expr b
mkLams	      :: [b] -> Expr b -> Expr b

mkLit lit	  = Lit lit
mkConApp con args = mkApps (Var (dataConId con)) args

mkLams binders body = foldr Lam body binders
mkLets binds body   = foldr Let body binds

mkIntLit    n = Lit (mkMachInt n)
mkIntLitInt n = Lit (mkMachInt (toInteger n))

mkStringLit str	= mkStringLitFS (_PK_ str)

mkStringLitFS str
  | any is_NUL (_UNPK_ str)
  = 	 -- Must cater for NULs in literal string
    mkApps (Var unpackCString2Id)
		[Lit (MachStr str),
		 mkIntLitInt (_LENGTH_ str)]

  | otherwise
  =	-- No NULs in the string
    App (Var unpackCStringId) (Lit (MachStr str))

  where
    is_NUL c = c == '\0'

varToCoreExpr :: CoreBndr -> Expr b
varToCoreExpr v | isId v    = Var v
                | otherwise = Type (mkTyVarTy v)
\end{code}


%************************************************************************
%*									*
\subsection{Simple access functions}
%*									*
%************************************************************************

\begin{code}
bindersOf  :: Bind b -> [b]
bindersOf (NonRec binder _) = [binder]
bindersOf (Rec pairs)       = [binder | (binder, _) <- pairs]

bindersOfBinds :: [Bind b] -> [b]
bindersOfBinds binds = foldr ((++) . bindersOf) [] binds

rhssOfBind :: Bind b -> [Expr b]
rhssOfBind (NonRec _ rhs) = [rhs]
rhssOfBind (Rec pairs)    = [rhs | (_,rhs) <- pairs]

rhssOfAlts :: [Alt b] -> [Expr b]
rhssOfAlts alts = [e | (_,_,e) <- alts]

flattenBinds :: [Bind b] -> [(b, Expr b)]	-- Get all the lhs/rhs pairs
flattenBinds (NonRec b r : binds) = (b,r) : flattenBinds binds
flattenBinds (Rec prs1   : binds) = prs1 ++ flattenBinds binds
flattenBinds []			  = []
\end{code}

We often want to strip off leading lambdas before getting down to
business.  @collectBinders@ is your friend.

We expect (by convention) type-, and value- lambdas in that
order.

\begin{code}
collectBinders	             :: Expr b -> ([b],         Expr b)
collectBindersIgnoringNotes  :: Expr b -> ([b],         Expr b)
collectTyBinders       	     :: CoreExpr -> ([TyVar],     CoreExpr)
collectValBinders      	     :: CoreExpr -> ([Id],        CoreExpr)
collectTyAndValBinders 	     :: CoreExpr -> ([TyVar], [Id], CoreExpr)

collectBinders expr
  = go [] expr
  where
    go bs (Lam b e) = go (b:bs) e
    go bs e	     = (reverse bs, e)

-- This one ignores notes.  It's used in CoreUnfold and StrAnal
-- when we aren't going to put the expression back together from
-- the pieces, so we don't mind losing the Notes
collectBindersIgnoringNotes expr
  = go [] expr
  where
    go bs (Lam b e)  = go (b:bs) e
    go bs (Note _ e) = go    bs  e
    go bs e	     = (reverse bs, e)

collectTyAndValBinders expr
  = (tvs, ids, body)
  where
    (tvs, body1) = collectTyBinders expr
    (ids, body)  = collectValBinders body1

collectTyBinders expr
  = go [] expr
  where
    go tvs (Lam b e) | isTyVar b = go (b:tvs) e
    go tvs e			 = (reverse tvs, e)

collectValBinders expr
  = go [] expr
  where
    go ids (Lam b e) | isId b = go (b:ids) e
    go ids body		      = (reverse ids, body)
\end{code}


@collectArgs@ takes an application expression, returning the function
and the arguments to which it is applied.

\begin{code}
collectArgs :: Expr b -> (Expr b, [Arg b])
collectArgs expr
  = go expr []
  where
    go (App f a) as = go f (a:as)
    go e 	 as = (e, as)
\end{code}

coreExprCc gets the cost centre enclosing an expression, if any.
It looks inside lambdas because (scc "foo" \x.e) = \x.scc "foo" e

\begin{code}
coreExprCc :: Expr b -> CostCentre
coreExprCc (Note (SCC cc) e)   = cc
coreExprCc (Note other_note e) = coreExprCc e
coreExprCc (Lam _ e)           = coreExprCc e
coreExprCc other               = noCostCentre
\end{code}


%************************************************************************
%*									*
\subsection{Predicates}
%*									*
%************************************************************************

\begin{code}
isValArg (Type _) = False
isValArg other    = True

isTypeArg (Type _) = True
isTypeArg other    = False

valBndrCount :: [CoreBndr] -> Int
valBndrCount []		    	  = 0
valBndrCount (b : bs) | isId b    = 1 + valBndrCount bs
		      | otherwise = valBndrCount bs

valArgCount :: [Arg b] -> Int
valArgCount []		    = 0
valArgCount (Type _ : args) = valArgCount args
valArgCount (other  : args) = 1 + valArgCount args
\end{code}


%************************************************************************
%*									*
\subsection{Seq stuff}
%*									*
%************************************************************************

\begin{code}
seqExpr :: CoreExpr -> ()
seqExpr (Var v)       = v `seq` ()
seqExpr (Lit lit)     = lit `seq` ()
seqExpr (App f a)     = seqExpr f `seq` seqExpr a
seqExpr (Lam b e)     = seqBndr b `seq` seqExpr e
seqExpr (Let b e)     = seqBind b `seq` seqExpr e
seqExpr (Case e b as) = seqExpr e `seq` seqBndr b `seq` seqAlts as
seqExpr (Note n e)    = seqNote n `seq` seqExpr e
seqExpr (Type t)      = seqType t

seqExprs [] = ()
seqExprs (e:es) = seqExpr e `seq` seqExprs es

seqNote (Coerce t1 t2) = seqType t1 `seq` seqType t2
seqNote other	       = ()

seqBndr b = b `seq` ()

seqBndrs [] = ()
seqBndrs (b:bs) = seqBndr b `seq` seqBndrs bs

seqBind (NonRec b e) = seqBndr b `seq` seqExpr e
seqBind (Rec prs)    = seqPairs prs

seqPairs [] = ()
seqPairs ((b,e):prs) = seqBndr b `seq` seqExpr e `seq` seqPairs prs

seqAlts [] = ()
seqAlts ((c,bs,e):alts) = seqBndrs bs `seq` seqExpr e `seq` seqAlts alts

seqRules :: CoreRules -> ()
seqRules (Rules rules fvs) = seq_rules rules `seq` seqVarSet fvs

seq_rules [] = ()
seq_rules (Rule fs bs es e : rules) = seqBndrs bs `seq` seqExprs (e:es) `seq` seq_rules rules
seq_rules (BuiltinRule _ : rules) = seq_rules rules
\end{code}



%************************************************************************
%*									*
\subsection{Annotated core; annotation at every node in the tree}
%*									*
%************************************************************************

\begin{code}
type AnnExpr bndr annot = (annot, AnnExpr' bndr annot)

data AnnExpr' bndr annot
  = AnnVar	Id
  | AnnLit	Literal
  | AnnLam	bndr (AnnExpr bndr annot)
  | AnnApp	(AnnExpr bndr annot) (AnnExpr bndr annot)
  | AnnCase	(AnnExpr bndr annot) bndr [AnnAlt bndr annot]
  | AnnLet	(AnnBind bndr annot) (AnnExpr bndr annot)
  | AnnNote	Note (AnnExpr bndr annot)
  | AnnType	Type

type AnnAlt bndr annot = (AltCon, [bndr], AnnExpr bndr annot)

data AnnBind bndr annot
  = AnnNonRec bndr (AnnExpr bndr annot)
  | AnnRec    [(bndr, AnnExpr bndr annot)]
\end{code}

\begin{code}
deAnnotate :: AnnExpr bndr annot -> Expr bndr
deAnnotate (_, e) = deAnnotate' e

deAnnotate' (AnnType t)           = Type t
deAnnotate' (AnnVar  v)           = Var v
deAnnotate' (AnnLit  lit)         = Lit lit
deAnnotate' (AnnLam  binder body) = Lam binder (deAnnotate body)
deAnnotate' (AnnApp  fun arg)     = App (deAnnotate fun) (deAnnotate arg)
deAnnotate' (AnnNote note body)   = Note note (deAnnotate body)

deAnnotate' (AnnLet bind body)
  = Let (deAnnBind bind) (deAnnotate body)
  where
    deAnnBind (AnnNonRec var rhs) = NonRec var (deAnnotate rhs)
    deAnnBind (AnnRec pairs) = Rec [(v,deAnnotate rhs) | (v,rhs) <- pairs]

deAnnotate' (AnnCase scrut v alts)
  = Case (deAnnotate scrut) v (map deAnnAlt alts)
  where
    deAnnAlt (con,args,rhs) = (con,args,deAnnotate rhs)
\end{code}

