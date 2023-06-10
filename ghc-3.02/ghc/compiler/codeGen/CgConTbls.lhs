%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[CgConTbls]{Info tables and update bits for constructors}

\begin{code}
module CgConTbls ( genStaticConBits ) where

#include "HsVersions.h"

import AbsCSyn
import CgMonad

import AbsCUtils	( mkAbsCStmts, mkAbstractCs, magicIdPrimRep )
import Constants	( uF_UPDATEE )
import CgHeapery	( heapCheck, allocDynClosure )
import CgRetConv	( dataReturnConvAlg, ctrlReturnConvAlg,
			  CtrlReturnConvention(..),
			  DataReturnConvention(..)
			)
import CgTailCall	( performReturn, mkStaticAlgReturnCode )
import CgUsages		( getHpRelOffset )
import CLabel		( mkConEntryLabel, mkStaticClosureLabel,
			  mkConUpdCodePtrVecLabel,
			  mkStdUpdCodePtrVecLabel, mkStdUpdVecTblLabel
			)
import ClosureInfo	( layOutStaticClosure, layOutDynCon,
			  layOutPhantomClosure, closurePtrsSize,
			  fitsMinUpdSize, mkConLFInfo,
			  infoTableLabelFromCI, dataConLiveness,
			  ClosureInfo
			)
import CostCentre	( dontCareCostCentre, CostCentre )
import FiniteMap	( fmToList, FiniteMap )
import HeapOffs		( zeroOff, VirtualHeapOffset )
import Id		( dataConTag, dataConRawArgTys,
			  dataConNumFields, fIRST_TAG,
			  emptyIdSet,
			  GenId{-instance NamedThing-}, Id
			)
import Name		( getOccString )
import PrimRep		( getPrimRepSize, PrimRep(..) )
import TyCon		( tyConDataCons, mkSpecTyCon, TyCon )
import Type		( typePrimRep, Type )
import Util		( panic )

mkSameSpecCon = panic "CgConTbls.mkSameSpecCon (ToDo)"
\end{code}

For every constructor we generate the following info tables:
	A static info table, for static instances of the constructor,

	For constructors which return in registers (and only them),
		an "inregs" info table.  This info table is rather emaciated;
		it only contains update code and tag.

	Plus:

\begin{tabular}{lll}
Info tbls &	 Macro  &     	     Kind of constructor \\
\hline
info & @CONST_INFO_TABLE@&    Zero arity (no info -- compiler uses static closure)\\
info & @CHARLIKE_INFO_TABLE@& Charlike   (no info -- compiler indexes fixed array)\\
info & @INTLIKE_INFO_TABLE@&  Intlike; the one macro generates both info tbls\\
info & @SPEC_INFO_TABLE@&     SPECish, and bigger than or equal to @MIN_UPD_SIZE@\\
info & @GEN_INFO_TABLE@&      GENish (hence bigger than or equal to @MIN_UPD_SIZE@)\\
\end{tabular}

Possible info tables for constructor con:

\begin{description}
\item[@con_info@:]
Used for dynamically let(rec)-bound occurrences of
the constructor, and for updates.  For constructors
which are int-like, char-like or nullary, when GC occurs,
the closure tries to get rid of itself.

\item[@con_inregs_info@:]
Used when returning a new constructor in registers.
Only for return-in-regs constructors.
Macro: @INREGS_INFO_TABLE@.

\item[@con_static_info@:]
Static occurrences of the constructor
macro: @STATIC_INFO_TABLE@.
\end{description}

For zero-arity constructors, \tr{con}, we also generate a static closure:

\begin{description}
\item[@con_closure@:]
A single static copy of the (zero-arity) constructor itself.
\end{description}

For charlike and intlike closures there is a fixed array of static
closures predeclared.

\begin{code}
genStaticConBits :: CompilationInfo 	-- global info about the compilation
		 -> [TyCon]		-- tycons to generate
	  	 -> FiniteMap TyCon [(Bool, [Maybe Type])]
					-- tycon specialisation info
		 -> AbstractC		-- output

genStaticConBits comp_info gen_tycons tycon_specs
  = -- for each type constructor:
    --	 grab all its data constructors;
    --	    for each one, generate an info table
    -- for each specialised type constructor
    --   for each specialisation of the type constructor
    --     grab data constructors, and generate info tables

    -- ToDo: for tycons and specialisations which are not
    --       declared in this module we must ensure that the
    --       C labels are local to this module i.e. static
    --	     since they may be duplicated in other modules

    mkAbstractCs [ gen_for_tycon tc | tc <- gen_tycons ]
      `mkAbsCStmts`
    mkAbstractCs [ mkAbstractCs [ gen_for_spec_tycon tc spec
				| (imported_spec, spec) <- specs,
				  -- no code generated if spec is imported
				  not imported_spec
				]
		 | (tc, specs) <- fmToList tycon_specs ]
  where
    gen_for_tycon :: TyCon -> AbstractC
    gen_for_tycon tycon
      = mkAbstractCs (map (genConInfo comp_info tycon) data_cons)
	  `mkAbsCStmts`
	maybe_tycon_vtbl
      where
    	data_cons   	= tyConDataCons tycon
    	tycon_upd_label = mkStdUpdVecTblLabel tycon

    	maybe_tycon_vtbl =
	  case ctrlReturnConvAlg tycon of
    	    UnvectoredReturn 1 -> CRetUnVector tycon_upd_label
					(mk_upd_label tycon (head data_cons))
    	    UnvectoredReturn _ -> AbsCNop
    	    VectoredReturn   _ -> CFlatRetVector tycon_upd_label
    	    	    	    	    	(map (mk_upd_label tycon) data_cons)
    ------------------
    gen_for_spec_tycon :: TyCon -> [Maybe Type] -> AbstractC

    gen_for_spec_tycon tycon ty_maybes
      = mkAbstractCs (map (genConInfo comp_info spec_tycon) spec_data_cons)
	  `mkAbsCStmts`
	maybe_spec_tycon_vtbl
      where
	data_cons      = tyConDataCons tycon

	spec_tycon     = mkSpecTyCon tycon ty_maybes
    	spec_data_cons = map (mkSameSpecCon ty_maybes) data_cons

    	spec_tycon_upd_label = mkStdUpdVecTblLabel spec_tycon

    	maybe_spec_tycon_vtbl =
	  case ctrlReturnConvAlg spec_tycon of
    	    UnvectoredReturn 1 -> CRetUnVector spec_tycon_upd_label
    	    	    	    	    	(mk_upd_label spec_tycon (head spec_data_cons))
    	    UnvectoredReturn _ -> AbsCNop
    	    VectoredReturn   _ -> CFlatRetVector spec_tycon_upd_label
					(map (mk_upd_label spec_tycon) spec_data_cons)
    ------------------
    mk_upd_label tycon con
      = CLbl
	(case (dataReturnConvAlg con) of
	  ReturnInRegs _ -> mkConUpdCodePtrVecLabel tycon tag
	  ReturnInHeap   -> mkStdUpdCodePtrVecLabel tycon tag)
	CodePtrRep
      where
	tag = dataConTag con
\end{code}

%************************************************************************
%*									*
\subsection[CgConTbls-info-tables]{Generating info tables for constructors}
%*									*
%************************************************************************

Generate the entry code, info tables, and (for niladic constructor) the
static closure, for a constructor.

\begin{code}
genConInfo :: CompilationInfo -> TyCon -> Id -> AbstractC

genConInfo comp_info tycon data_con
  = mkAbstractCs [
		  CSplitMarker,
		  inregs_upd_maybe,
		  closure_code,
    	    	  static_code,
		  closure_maybe]
	-- Order of things is to reduce forward references
  where
    (closure_info, body_code) = mkConCodeAndInfo data_con

    -- To allow the debuggers, interpreters, etc to cope with static
    -- data structures (ie those built at compile time), we take care that
    -- info-table contains the information we need.
    (static_ci,_) = layOutStaticClosure data_con typePrimRep arg_tys (mkConLFInfo data_con)

    body       = (initC comp_info (
	    	      profCtrC SLIT("ENT_CON") [CReg node] `thenC`
		      body_code))

    entry_addr = CLbl entry_label CodePtrRep
    con_descr  = getOccString data_con

    closure_code        = CClosureInfoAndCode closure_info body Nothing
					      stdUpd con_descr
					      (dataConLiveness closure_info)
    static_code         = CClosureInfoAndCode static_ci body Nothing
					      stdUpd con_descr
					      (dataConLiveness static_ci)

    inregs_upd_maybe    = genPhantomUpdInfo comp_info tycon data_con

    stdUpd  	    	= CLbl (mkStdUpdCodePtrVecLabel tycon tag) CodePtrRep

    tag	    	    	= dataConTag data_con

    cost_centre = mkCCostCentre dontCareCostCentre -- not worried about static data costs

    -- For zero-arity data constructors, or, more accurately,
    -- 	 those which only have VoidRep args (or none):
    -- 	We make the closure too (not just info tbl), so that we can share
    --  one copy throughout.
    closure_maybe = if not (all zero_size arg_tys) then
		    	AbsCNop
		    else
		    	CStaticClosure  closure_label		-- Label for closure
					static_ci		-- Info table
					cost_centre
					[{-No args!  A slight lie for constrs with VoidRep args-}]

    zero_size arg_ty = getPrimRepSize (typePrimRep arg_ty) == 0

    arg_tys	    = dataConRawArgTys 	   data_con
    entry_label     = mkConEntryLabel      data_con
    closure_label   = mkStaticClosureLabel data_con
\end{code}

The entry code for a constructor now loads the info ptr by indirecting
node.  The alternative is to load the info ptr in the enter-via-node
sequence.  There's is a trade-off here:

	* If the architecture can perform an indirect jump through a
	  register in one instruction, or if the info ptr is not a
	  real register, then *not* loading the info ptr on an enter
	  is a win.

	* If the enter-via-node code is identical whether we load the
	  info ptr or not, then doing it is a win (it means we don't
	  have to do it here).

However, the gratuitous load here is miniscule compared to the
gratuitous loads of the info ptr on each enter, so we go for the first
option.

-- Simon M. (6/5/96)

\begin{code}
mkConCodeAndInfo :: Id 			-- Data constructor
		 -> (ClosureInfo, Code)	-- The info table

mkConCodeAndInfo con
  = case (dataReturnConvAlg con) of

    ReturnInRegs regs ->
	let
	    (closure_info, regs_w_offsets)
	      = layOutDynCon con magicIdPrimRep regs

	    body_code
	      = profCtrC SLIT("RET_OLD_IN_REGS") [mkIntCLit (length regs_w_offsets)] `thenC`

		performReturn (mkAbstractCs (load_infoptr : map move_to_reg regs_w_offsets))
			      (mkStaticAlgReturnCode con Nothing {- Info-ptr already loaded-})
			      emptyIdSet{-no live vars-}
	in
	(closure_info, body_code)

    ReturnInHeap ->
	let
	    arg_tys = dataConRawArgTys con

	    (closure_info, arg_things)
		= layOutDynCon con typePrimRep arg_tys

	    body_code
		= -- NB: We don't set CC when entering data (WDP 94/06)
		  profCtrC SLIT("RET_OLD_IN_HEAP") [mkIntCLit (length arg_things)] `thenC`

		  performReturn (mkAbstractCs [load_infoptr])	-- Ptr to thing already in Node
				(mkStaticAlgReturnCode con Nothing {- Info-ptr already loaded-})
				emptyIdSet{-no live vars-}
	in
	(closure_info, body_code)

  where
    move_to_reg :: (MagicId, VirtualHeapOffset {-from Node-}) -> AbstractC
    move_to_reg (reg, offset)
      = CAssign (CReg reg) (CVal (NodeRel offset) (magicIdPrimRep reg))

    load_infoptr 
      = CAssign (CReg infoptr) (CMacroExpr DataPtrRep INFO_PTR [CReg node])
\end{code}

%************************************************************************
%*									*
\subsection[CgConTbls-updates]{Generating update bits for constructors}
%*									*
%************************************************************************

Generate the "phantom" info table and update code, iff the constructor returns in regs

\begin{code}

genPhantomUpdInfo :: CompilationInfo -> TyCon -> Id{-data con-} -> AbstractC

genPhantomUpdInfo comp_info tycon data_con
  = case (dataReturnConvAlg data_con) of

      ReturnInHeap -> AbsCNop	-- No need for a phantom update

      ReturnInRegs regs ->
	let
	    phantom_itbl = CClosureInfoAndCode phantom_ci AbsCNop Nothing
				upd_code con_descr
				(dataConLiveness phantom_ci)

	    phantom_ci = layOutPhantomClosure data_con (mkConLFInfo data_con)

	    con_descr = getOccString data_con

	    con_arity = dataConNumFields data_con

	    upd_code = CLabelledCode upd_label (mkAbsCStmts build_closure perform_return)
    	    upd_label = mkConUpdCodePtrVecLabel tycon tag
	    tag = dataConTag data_con

	    updatee = CVal (SpBRel 0 (- uF_UPDATEE)) PtrRep

	    perform_return = mkAbstractCs
	      [
		CMacroStmt POP_STD_UPD_FRAME [],
		CReturn (CReg RetReg) return_info
	      ]

	    return_info =
    	      case (ctrlReturnConvAlg tycon) of
		UnvectoredReturn _ -> DirectReturn
		VectoredReturn   _ -> StaticVectoredReturn (tag - fIRST_TAG)

    	    -- Determine cost centre for the updated closures CC (and allocation)
    	    -- CCC for lexical (now your only choice)
    	    use_cc = CReg CurCostCentre -- what to put in the closure
	    blame_cc = use_cc -- who to blame for allocation

	    do_move (reg, virt_offset) =
    	    	CAssign (CVal (NodeRel virt_offset) (magicIdPrimRep reg)) (CReg reg)


    	    -- Code for building a new constructor in place over the updatee
       	    overwrite_code
	      = profCtrC SLIT("UPD_CON_IN_PLACE")
			 [mkIntCLit (length regs_w_offsets)]	`thenC`
	    	absC (mkAbstractCs
	    	  [
    	            CAssign (CReg node) updatee,

		    -- Tell the storage mgr that we intend to update in place
		    -- This may (in complicated mgrs eg generational) cause gc,
		    -- and it may modify Node to point to another place to
		    -- actually update into.
	    	    CMacroStmt upd_inplace_macro [liveness_mask],

		    -- Initialise the closure pointed to by node
	    	    CInitHdr closure_info (NodeRel zeroOff) use_cc True,
	    	    mkAbstractCs (map do_move regs_w_offsets),
    	    	    if con_arity /= 0 then
    	    	        CAssign (CReg infoptr) (CLbl info_label DataPtrRep)
		    else
    	    	        AbsCNop
	    	  ])

    	    upd_inplace_macro = if closurePtrsSize closure_info == 0
    	    	    	    	then UPD_INPLACE_NOPTRS
    	    	    	    	else UPD_INPLACE_PTRS

    	    -- Code for allocating a new constructor in the heap
    	    alloc_code
	      = let
		    amodes_w_offsets = [ (CReg r, o) | (r,o) <- regs_w_offsets ]
	    	in
		    -- Allocate and build closure specifying upd_new_w_regs
	    	    allocDynClosure closure_info use_cc blame_cc amodes_w_offsets
						    	`thenFC` \ hp_offset ->
	    	    getHpRelOffset hp_offset		`thenFC` \ hp_rel ->
	    	    let
			amode = CAddr hp_rel
    	    	    in
		    profCtrC SLIT("UPD_CON_IN_NEW")
			     [mkIntCLit (length amodes_w_offsets)] `thenC`
		    absC (mkAbstractCs
		      [ CMacroStmt UPD_IND [updatee, amode],
			CAssign (CReg node) amode,
			CAssign (CReg infoptr) (CLbl info_label DataPtrRep)
		      ])

	    (closure_info, regs_w_offsets) = layOutDynCon data_con magicIdPrimRep regs
	    info_label = infoTableLabelFromCI closure_info
	    liveness_mask = mkIntCLit (mkLiveRegsMask (node:regs))

	    build_closure =
	      if fitsMinUpdSize closure_info then
		initC comp_info overwrite_code
	      else
		initC comp_info (heapCheck regs False alloc_code)

	in CClosureUpdInfo phantom_itbl

\end{code}

