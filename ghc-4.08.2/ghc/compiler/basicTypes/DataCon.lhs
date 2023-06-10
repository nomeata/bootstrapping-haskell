%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[DataCon]{@DataCon@: Data Constructors}

\begin{code}
module DataCon (
	DataCon,
	ConTag, fIRST_TAG,
	mkDataCon,
	dataConRepType, dataConSig, dataConName, dataConTag, dataConTyCon,
	dataConArgTys, dataConOrigArgTys, dataConInstOrigArgTys,
	dataConRepArgTys, dataConTheta,
	dataConFieldLabels, dataConStrictMarks, 
	dataConSourceArity, dataConRepArity,
	dataConNumInstArgs, dataConId, dataConWrapId, dataConRepStrictness,
	isNullaryDataCon, isTupleCon, isUnboxedTupleCon, 
	isExistentialDataCon, 

	splitProductType_maybe, splitProductType,

	StrictnessMark(..), 	-- Representation visible to MkId only
	markedStrict, notMarkedStrict, markedUnboxed, maybeMarkedUnboxed
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Subst( substTy, mkTyVarSubst )

import CmdLineOpts	( opt_DictsStrict )
import Type		( Type, ThetaType, TauType, ClassContext,
			  mkForAllTys, mkFunTys, mkTyConApp, 
			  mkTyVarTys, mkDictTys,
			  splitAlgTyConApp_maybe, classesToPreds
			)
import TyCon		( TyCon, tyConDataCons, isDataTyCon, isProductTyCon,
			  isTupleTyCon, isUnboxedTupleTyCon, isRecursiveTyCon )
import Class		( classTyCon )
import Name		( Name, NamedThing(..), nameUnique, isLocallyDefined )
import Var		( TyVar, Id )
import FieldLabel	( FieldLabel )
import BasicTypes	( Arity )
import Demand		( Demand, wwStrict, wwLazy )
import Outputable
import Unique		( Unique, Uniquable(..) )
import CmdLineOpts	( opt_UnboxStrictFields )
import PprType		()	-- Instances
import Maybes		( maybeToBool )
import Maybe
import Util		( assoc )
\end{code}


Stuff about data constructors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Every constructor, C, comes with a 

  *wrapper*, called C, whose type is exactly what it looks like
	in the source program. It is an ordinary function,
	and it gets a top-level binding like any other function

  *worker*, called $wC, which is the actual data constructor.
	Its type may be different to C, because:
		- useless dict args are dropped
		- strict args may be flattened
	It does not have a binding.

  The worker is very like a primop, in that it has no binding,



%************************************************************************
%*									*
\subsection{Data constructors}
%*									*
%************************************************************************

\begin{code}
data DataCon
  = MkData {			-- Used for data constructors only;
				-- there *is* no constructor for a newtype
	dcName   :: Name,
	dcUnique :: Unique, 		-- Cached from Name
	dcTag    :: ConTag,

	-- Running example:
	--
	--	data Eq a => T a = forall b. Ord b => MkT a [b]

	dcRepType   :: Type,	-- Type of the constructor 
				-- 	forall ab . Ord b => a -> [b] -> MkT a
				-- (this is *not* of the constructor Id: 
				--  see notes after this data type declaration)

	-- The next six fields express the type of the constructor, in pieces
	-- e.g.
	--
	--	dcTyVars   = [a]
	-- 	dcTheta    = [Eq a]
	--	dcExTyVars = [b]
	--	dcExTheta  = [Ord b]
	--	dcOrigArgTys   = [a,List b]
	--	dcTyCon    = T

	dcTyVars :: [TyVar], 		-- Type vars and context for the data type decl
					-- These are ALWAYS THE SAME AS THE TYVARS 
					-- FOR THE PARENT TyCon.  We occasionally rely on
					-- this just to avoid redundant instantiation
	dcTheta  ::  ClassContext,

	dcExTyVars :: [TyVar], 		-- Ditto for the context of the constructor, 
	dcExTheta  :: ClassContext,	-- the existentially quantified stuff
					
	dcOrigArgTys :: [Type],		-- Original argument types
					-- (before unboxing and flattening of
					--  strict fields)

	dcRepArgTys :: [Type],		-- Final, representation argument types, after unboxing and flattening,
					-- and including existential dictionaries

	dcTyCon  :: TyCon,		-- Result tycon 

	-- Now the strictness annotations and field labels of the constructor
	dcUserStricts :: [StrictnessMark], 
		-- Strictness annotations, as placed on the data type defn,
		-- in the same order as the argument types;
		-- length = dataConSourceArity dataCon

	dcRealStricts :: [StrictnessMark],
		-- Strictness annotations as deduced by the compiler.  May
		-- include some MarkedUnboxed fields that are merely MarkedStrict
		-- in dcUserStricts.  Also includes the existential dictionaries.
		-- length = length dcExTheta + dataConSourceArity dataCon

	dcFields  :: [FieldLabel],
		-- Field labels for this constructor, in the
		-- same order as the argument types; 
		-- length = 0 (if not a record) or dataConSourceArity.

	-- Finally, the curried worker function that corresponds to the constructor
	-- It doesn't have an unfolding; the code generator saturates these Ids
	-- and allocates a real constructor when it finds one.
	--
	-- An entirely separate wrapper function is built in TcTyDecls

	dcId :: Id,		-- The corresponding worker Id
				-- Takes dcRepArgTys as its arguments

	dcWrapId :: Id		-- The wrapper Id
  }

type ConTag = Int

fIRST_TAG :: ConTag
fIRST_TAG =  1	-- Tags allocated from here for real constructors
\end{code}

The dcRepType field contains the type of the representation of a contructor
This may differ from the type of the contructor *Id* (built
by MkId.mkDataConId) for two reasons:
	a) the constructor Id may be overloaded, but the dictionary isn't stored
	   e.g.    data Eq a => T a = MkT a a

	b) the constructor may store an unboxed version of a strict field.

Here's an example illustrating both:
	data Ord a => T a = MkT Int! a
Here
	T :: Ord a => Int -> a -> T a
but the rep type is
	Trep :: Int# -> a -> T a
Actually, the unboxed part isn't implemented yet!


%************************************************************************
%*									*
\subsection{Strictness indication}
%*									*
%************************************************************************

\begin{code}
data StrictnessMark = MarkedStrict
		    | MarkedUnboxed DataCon [Type]
		    | NotMarkedStrict

markedStrict    = MarkedStrict
notMarkedStrict = NotMarkedStrict
markedUnboxed   = MarkedUnboxed (panic "markedUnboxed1") (panic "markedUnboxed2")

maybeMarkedUnboxed (MarkedUnboxed dc tys) = Just (dc,tys)
maybeMarkedUnboxed other		  = Nothing
\end{code}


%************************************************************************
%*									*
\subsection{Instances}
%*									*
%************************************************************************

\begin{code}
instance Eq DataCon where
    a == b = getUnique a == getUnique b
    a /= b = getUnique a /= getUnique b

instance Ord DataCon where
    a <= b = getUnique a <= getUnique b
    a <	 b = getUnique a <  getUnique b
    a >= b = getUnique a >= getUnique b
    a >	 b = getUnique a > getUnique b
    compare a b = getUnique a `compare` getUnique b

instance Uniquable DataCon where
    getUnique = dcUnique

instance NamedThing DataCon where
    getName = dcName

instance Outputable DataCon where
    ppr con = ppr (dataConName con)

instance Show DataCon where
    showsPrec p con = showsPrecSDoc p (ppr con)
\end{code}


%************************************************************************
%*									*
\subsection{Consruction}
%*									*
%************************************************************************

\begin{code}
mkDataCon :: Name
	  -> [StrictnessMark] -> [FieldLabel]
	  -> [TyVar] -> ClassContext
	  -> [TyVar] -> ClassContext
	  -> [TauType] -> TyCon
	  -> Id -> Id
	  -> DataCon
  -- Can get the tag from the TyCon

mkDataCon name arg_stricts fields 
	  tyvars theta ex_tyvars ex_theta orig_arg_tys tycon 
	  work_id wrap_id
  = ASSERT(length arg_stricts == length orig_arg_tys)
	-- The 'stricts' passed to mkDataCon are simply those for the
	-- source-language arguments.  We add extra ones for the
	-- dictionary arguments right here.
    con
  where
    con = MkData {dcName = name, dcUnique = nameUnique name,
	  	  dcTyVars = tyvars, dcTheta = theta, 
		  dcOrigArgTys = orig_arg_tys, 
		  dcRepArgTys = rep_arg_tys,
	     	  dcExTyVars = ex_tyvars, dcExTheta = ex_theta,
		  dcRealStricts = all_stricts, dcUserStricts = user_stricts,
		  dcFields = fields, dcTag = tag, dcTyCon = tycon, dcRepType = ty,
		  dcId = work_id, dcWrapId = wrap_id}

    (real_arg_stricts, strict_arg_tyss) 
	= unzip (zipWith (unbox_strict_arg_ty tycon) arg_stricts orig_arg_tys)
    rep_arg_tys = mkDictTys ex_theta ++ concat strict_arg_tyss
	
    ex_dict_stricts = map mk_dict_strict_mark ex_theta
	-- Add a strictness flag for the existential dictionary arguments
    all_stricts     = ex_dict_stricts ++ real_arg_stricts
    user_stricts    = ex_dict_stricts ++ arg_stricts

    tag = assoc "mkDataCon" (tyConDataCons tycon `zip` [fIRST_TAG..]) con
    ty  = mkForAllTys (tyvars ++ ex_tyvars) 
	              (mkFunTys rep_arg_tys result_ty)
		-- NB: the existential dict args are already in rep_arg_tys

    result_ty = mkTyConApp tycon (mkTyVarTys tyvars)

mk_dict_strict_mark (clas,tys)
  | opt_DictsStrict &&
	-- Don't mark newtype things as strict!
    isDataTyCon (classTyCon clas) = MarkedStrict
  | otherwise			  = NotMarkedStrict
\end{code}

\begin{code}
dataConName :: DataCon -> Name
dataConName = dcName

dataConTag :: DataCon -> ConTag
dataConTag  = dcTag

dataConTyCon :: DataCon -> TyCon
dataConTyCon = dcTyCon

dataConRepType :: DataCon -> Type
dataConRepType = dcRepType

dataConId :: DataCon -> Id
dataConId = dcId

dataConWrapId :: DataCon -> Id
dataConWrapId = dcWrapId

dataConFieldLabels :: DataCon -> [FieldLabel]
dataConFieldLabels = dcFields

dataConStrictMarks :: DataCon -> [StrictnessMark]
dataConStrictMarks = dcRealStricts

-- Number of type-instantiation arguments
-- All the remaining arguments of the DataCon are (notionally)
-- stored in the DataCon, and are matched in a case expression
dataConNumInstArgs (MkData {dcTyVars = tyvars}) = length tyvars

dataConSourceArity :: DataCon -> Arity
	-- Source-level arity of the data constructor
dataConSourceArity dc = length (dcOrigArgTys dc)

-- dataConRepArity gives the number of actual fields in the
-- {\em representation} of the data constructor.  This may be more than appear
-- in the source code; the extra ones are the existentially quantified
-- dictionaries
dataConRepArity (MkData {dcRepArgTys = arg_tys}) = length arg_tys

isNullaryDataCon con  = dataConRepArity con == 0

dataConRepStrictness :: DataCon -> [Demand]
	-- Give the demands on the arguments of a 
	-- Core constructor application (Con dc args)
dataConRepStrictness dc
  = go (dcRealStricts dc) 
  where
    go []			  = []
    go (MarkedStrict        : ss) = wwStrict : go ss
    go (NotMarkedStrict     : ss) = wwLazy   : go ss
    go (MarkedUnboxed con _ : ss) = go (dcRealStricts con ++ ss)

dataConSig :: DataCon -> ([TyVar], ClassContext,
			  [TyVar], ClassContext,
			  [TauType], TyCon)

dataConSig (MkData {dcTyVars = tyvars, dcTheta = theta,
		     dcExTyVars = ex_tyvars, dcExTheta = ex_theta,
		     dcOrigArgTys = arg_tys, dcTyCon = tycon})
  = (tyvars, theta, ex_tyvars, ex_theta, arg_tys, tycon)

dataConArgTys :: DataCon 
	      -> [Type] 	-- Instantiated at these types
				-- NB: these INCLUDE the existentially quantified arg types
	      -> [Type]		-- Needs arguments of these types
				-- NB: these INCLUDE the existentially quantified dict args
				--     but EXCLUDE the data-decl context which is discarded
				-- It's all post-flattening etc; this is a representation type

dataConArgTys (MkData {dcRepArgTys = arg_tys, dcTyVars = tyvars, 
		       dcExTyVars = ex_tyvars}) inst_tys
 = map (substTy (mkTyVarSubst (tyvars ++ ex_tyvars) inst_tys)) arg_tys

dataConTheta :: DataCon -> ClassContext
dataConTheta dc = dcTheta dc

-- And the same deal for the original arg tys:

dataConInstOrigArgTys :: DataCon -> [Type] -> [Type]
dataConInstOrigArgTys (MkData {dcOrigArgTys = arg_tys, dcTyVars = tyvars, 
		       dcExTyVars = ex_tyvars}) inst_tys
 = map (substTy (mkTyVarSubst (tyvars ++ ex_tyvars) inst_tys)) arg_tys
\end{code}

These two functions get the real argument types of the constructor,
without substituting for any type variables.    

dataConOrigArgTys returns the arg types of the wrapper, excluding all dictionary args.

dataConRepArgTys retuns the arg types of the worker, including all dictionaries, and
after any flattening has been done.

\begin{code}
dataConOrigArgTys :: DataCon -> [Type]
dataConOrigArgTys dc = dcOrigArgTys dc

dataConRepArgTys :: DataCon -> [TauType]
dataConRepArgTys dc = dcRepArgTys dc
\end{code}


\begin{code}
isTupleCon :: DataCon -> Bool
isTupleCon (MkData {dcTyCon = tc}) = isTupleTyCon tc
	
isUnboxedTupleCon :: DataCon -> Bool
isUnboxedTupleCon (MkData {dcTyCon = tc}) = isUnboxedTupleTyCon tc

isExistentialDataCon :: DataCon -> Bool
isExistentialDataCon (MkData {dcExTyVars = tvs}) = not (null tvs)
\end{code}


%************************************************************************
%*									*
\subsection{Splitting products}
%*									*
%************************************************************************

\begin{code}	
splitProductType_maybe
	:: Type 			-- A product type, perhaps
	-> Maybe (TyCon, 		-- The type constructor
		  [Type],		-- Type args of the tycon
		  DataCon,		-- The data constructor
		  [Type])		-- Its *representation* arg types

	-- Returns (Just ...) for any 
	--	single-constructor
	--	not existentially quantified
	-- type whether a data type or a new type
	--
	-- Rejecing existentials is conservative.  Maybe some things
	-- could be made to work with them, but I'm not going to sweat
	-- it through till someone finds it's important.

splitProductType_maybe ty
  = case splitAlgTyConApp_maybe ty of
	Just (tycon,ty_args,[data_con]) 
	   | isProductTyCon tycon  		-- Includes check for non-existential
	   -> Just (tycon, ty_args, data_con, dataConArgTys data_con ty_args)
	other -> Nothing

splitProductType str ty
  = case splitProductType_maybe ty of
	Just stuff -> stuff
	Nothing    -> pprPanic (str ++ ": not a product") (ppr ty)

-- We attempt to unbox/unpack a strict field when either:
--   (i)  The tycon is imported, and the field is marked '! !', or
--   (ii) The tycon is defined in this module, the field is marked '!', 
--	  and the -funbox-strict-fields flag is on.
--
-- This ensures that if we compile some modules with -funbox-strict-fields and
-- some without, the compiler doesn't get confused about the constructor
-- representations.

unbox_strict_arg_ty :: TyCon -> StrictnessMark -> Type -> (StrictnessMark, [Type])

unbox_strict_arg_ty tycon strict_mark ty
  | case strict_mark of 
	NotMarkedStrict   -> False
	MarkedUnboxed _ _ -> True
	MarkedStrict      -> opt_UnboxStrictFields && 
			     isLocallyDefined tycon &&
			     maybeToBool maybe_product &&
			     not (isRecursiveTyCon tycon) &&
			     isDataTyCon arg_tycon
	-- We can't look through newtypes in arguments (yet)
  = (MarkedUnboxed con arg_tys, arg_tys)

  | otherwise
  = (strict_mark, [ty])

  where
    maybe_product = splitProductType_maybe ty
    Just (arg_tycon, _, con, arg_tys) = maybe_product
\end{code}


