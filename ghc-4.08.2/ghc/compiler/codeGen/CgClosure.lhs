%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgClosure.lhs,v 1.39 2000/01/13 14:33:58 hwloidl Exp $
%
\section[CgClosure]{Code generation for closures}

This module provides the support code for @StgToAbstractC@ to deal
with {\em closures} on the RHSs of let(rec)s.  See also
@CgCon@, which deals with constructors.

\begin{code}
module CgClosure ( cgTopRhsClosure, 
		   cgStdRhsClosure, 
		   cgRhsClosure, 
		   closureCodeBody ) where

#include "HsVersions.h"

import {-# SOURCE #-} CgExpr ( cgExpr )

import CgMonad
import AbsCSyn
import StgSyn

import AbsCUtils	( mkAbstractCs, getAmodeRep )
import CgBindery	( getCAddrMode, getArgAmodes,
			  getCAddrModeAndInfo, bindNewToNode,
			  bindNewToStack,
			  bindNewToReg, bindArgsToRegs,
			  stableAmodeIdInfo, heapIdInfo, CgIdInfo
			)
import CgUpdate		( pushUpdateFrame )
import CgHeapery	( allocDynClosure, 
			  fetchAndReschedule, yield,  -- HWL
			  fastEntryChecks, thunkChecks
			)
import CgStackery	( mkTaggedVirtStkOffsets, freeStackSlots )
import CgUsages		( adjustSpAndHp, setRealAndVirtualSp, getVirtSp,
			  getSpRelOffset, getHpRelOffset
			)
import CLabel		( CLabel, mkClosureLabel, mkFastEntryLabel,
			  mkRednCountsLabel, mkInfoTableLabel,
                          pprCLabel
			)
import ClosureInfo	-- lots and lots of stuff
import CmdLineOpts	( opt_GranMacros, opt_SccProfilingOn, opt_DoTickyProfiling )
import CostCentre	
import Id		( Id, idName, idType, idPrimRep )
import Name		( Name, isLocalName )
import Module		( Module, pprModule )
import ListSetOps	( minusList )
import PrimRep		( PrimRep(..) )
import PprType          ( showTypeCategory )
import Util		( isIn )
import CmdLineOpts	( opt_SccProfilingOn )
import Outputable

import Name             ( nameOccName )
import OccName          ( occNameFS )

getWrapperArgTypeCategories = panic "CgClosure.getWrapperArgTypeCategories (ToDo)"
\end{code}

%********************************************************
%*							*
\subsection[closures-no-free-vars]{Top-level closures}
%*							*
%********************************************************

For closures bound at top level, allocate in static space.
They should have no free variables.

\begin{code}
cgTopRhsClosure :: Id
		-> CostCentreStack	-- Optional cost centre annotation
		-> StgBinderInfo
		-> [Id]		-- Args
		-> StgExpr
		-> LambdaFormInfo
		-> FCode (Id, CgIdInfo)

cgTopRhsClosure id ccs binder_info args body lf_info
  = 	-- LAY OUT THE OBJECT
    let
	closure_info = layOutStaticNoFVClosure name lf_info
    in

	-- BUILD THE OBJECT (IF NECESSARY)
    ({- if staticClosureRequired name binder_info lf_info
     then -}
	(if opt_SccProfilingOn 
	  then
	     absC (CStaticClosure
		closure_label	-- Labelled with the name on lhs of defn
		closure_info
	    	(mkCCostCentreStack ccs)
		[])		-- No fields
	  else
	     absC (CStaticClosure
		closure_label	-- Labelled with the name on lhs of defn
		closure_info
	    	(panic "absent cc")
		[])		-- No fields
	)

     {- else
	nopC -}
    							`thenC`

	-- GENERATE THE INFO TABLE (IF NECESSARY)
    forkClosureBody (closureCodeBody binder_info closure_info
					 ccs args body)

    ) `thenC`

    returnFC (id, cg_id_info)
  where
    name	  = idName id
    closure_label = mkClosureLabel name
    cg_id_info    = stableAmodeIdInfo id (CLbl closure_label PtrRep) lf_info
\end{code}

%********************************************************
%*							*
\subsection[non-top-level-closures]{Non top-level closures}
%*							*
%********************************************************

For closures with free vars, allocate in heap.

\begin{code}
cgStdRhsClosure
	:: Id
	-> CostCentreStack	-- Optional cost centre annotation
	-> StgBinderInfo
	-> [Id]			-- Free vars
	-> [Id]			-- Args
	-> StgExpr
	-> LambdaFormInfo
	-> [StgArg]		-- payload
	-> FCode (Id, CgIdInfo)

cgStdRhsClosure binder cc binder_info fvs args body lf_info payload
		-- AHA!  A STANDARD-FORM THUNK
  = (
	-- LAY OUT THE OBJECT
    getArgAmodes payload			`thenFC` \ amodes ->
    let
	(closure_info, amodes_w_offsets)
	  = layOutDynClosure (idName binder) getAmodeRep amodes lf_info

	(use_cc, blame_cc) = chooseDynCostCentres cc args fvs body
    in
	-- BUILD THE OBJECT
    allocDynClosure closure_info use_cc blame_cc amodes_w_offsets
    )
		`thenFC` \ heap_offset ->

	-- RETURN
    returnFC (binder, heapIdInfo binder heap_offset lf_info)

  where
    is_std_thunk	   = isStandardFormThunk lf_info
\end{code}

Here's the general case.

\begin{code}
cgRhsClosure	:: Id
		-> CostCentreStack	-- Optional cost centre annotation
		-> StgBinderInfo
		-> [Id]			-- Free vars
		-> [Id]			-- Args
		-> StgExpr
		-> LambdaFormInfo
		-> FCode (Id, CgIdInfo)

cgRhsClosure binder cc binder_info fvs args body lf_info
  = (
  	-- LAY OUT THE OBJECT
	--
	-- If the binder is itself a free variable, then don't store
	-- it in the closure.  Instead, just bind it to Node on entry.
	-- NB we can be sure that Node will point to it, because we
	-- havn't told mkClosureLFInfo about this; so if the binder
	-- *was* a free var of its RHS, mkClosureLFInfo thinks it *is*
	-- stored in the closure itself, so it will make sure that
	-- Node points to it...
    let
	is_elem	       = isIn "cgRhsClosure"

	binder_is_a_fv = binder `is_elem` fvs
	reduced_fvs    = if binder_is_a_fv
			 then fvs `minusList` [binder]
			 else fvs
    in
    mapFCs getCAddrModeAndInfo reduced_fvs	`thenFC` \ amodes_and_info ->
    let
	fvs_w_amodes_and_info	      = reduced_fvs `zip` amodes_and_info

	closure_info :: ClosureInfo
	bind_details :: [((Id, (CAddrMode, LambdaFormInfo)), VirtualHeapOffset)]

	(closure_info, bind_details)
	  = layOutDynClosure (idName binder) get_kind fvs_w_amodes_and_info lf_info

	bind_fv ((id, (_, lf_info)), offset) = bindNewToNode id offset lf_info

	amodes_w_offsets = [(amode,offset) | ((_, (amode,_)), offset) <- bind_details]

	get_kind (id, amode_and_info) = idPrimRep id
    in
	-- BUILD ITS INFO TABLE AND CODE
    forkClosureBody (
		-- Bind the fvs
	    mapCs bind_fv bind_details `thenC`

	  	-- Bind the binder itself, if it is a free var
	    (if binder_is_a_fv then
		bindNewToReg binder node lf_info
	    else
		nopC)					`thenC`

		-- Compile the body
	    closureCodeBody binder_info closure_info cc args body
    )	`thenC`

	-- BUILD THE OBJECT
    let
	(use_cc, blame_cc) = chooseDynCostCentres cc args fvs body
    in
    allocDynClosure closure_info use_cc blame_cc amodes_w_offsets
    )		`thenFC` \ heap_offset ->

	-- RETURN
    returnFC (binder, heapIdInfo binder heap_offset lf_info)
\end{code}

%************************************************************************
%*									*
\subsection[code-for-closures]{The code for closures}
%*									*
%************************************************************************

\begin{code}
closureCodeBody :: StgBinderInfo
		-> ClosureInfo	   -- Lots of information about this closure
		-> CostCentreStack -- Optional cost centre attached to closure
		-> [Id]
		-> StgExpr
		-> Code
\end{code}

There are two main cases for the code for closures.  If there are {\em
no arguments}, then the closure is a thunk, and not in normal form.
So it should set up an update frame (if it is shared).  Also, it has
no argument satisfaction check, so fast and slow entry-point labels
are the same.

\begin{code}
closureCodeBody binder_info closure_info cc [] body
  = -- thunks cannot have a primitive type!
    getAbsC body_code 	`thenFC` \ body_absC ->
    moduleName		`thenFC` \ mod_name ->

    absC (CClosureInfoAndCode closure_info body_absC Nothing
			      (cl_descr mod_name))
  where
    cl_descr mod_name = closureDescription mod_name (closureName closure_info)

    body_label   = entryLabelFromCI closure_info
    
    is_box  = case body of { StgApp fun [] -> True; _ -> False }

    body_code   = profCtrC SLIT("TICK_ENT_THK") []		`thenC`
		  thunkWrapper closure_info body_label (
			-- We only enter cc after setting up update so that cc
			-- of enclosing scope will be recorded in update frame
			-- CAF/DICT functions will be subsumed by this enclosing cc
		    enterCostCentreCode closure_info cc IsThunk	is_box `thenC`
		    cgExpr body)
\end{code}

If there is {\em at least one argument}, then this closure is in
normal form, so there is no need to set up an update frame.  On the
other hand, we do have to check that there are enough args, and
perform an update if not!

The Macros for GrAnSim are produced at the beginning of the
argSatisfactionCheck (by calling fetchAndReschedule).  There info if
Node points to closure is available. -- HWL

\begin{code}
closureCodeBody binder_info closure_info cc all_args body
  = getEntryConvention name lf_info
		       (map idPrimRep all_args)		`thenFC` \ entry_conv ->

    -- get the current virtual Sp (it might not be zero, eg. if we're
    -- compiling a let-no-escape).
    getVirtSp `thenFC` \vSp ->

    let
    	-- Figure out what is needed and what isn't

	-- SDM: need everything for now in case the heap/stack check refers
	-- to it. (ToDo)
	slow_code_needed   = True 
		   --slowFunEntryCodeRequired name binder_info entry_conv
	info_table_needed  = True
		   --funInfoTableRequired name binder_info lf_info

	-- Arg mapping for standard (slow) entry point; all args on stack,
	-- with tagging.
    	(sp_all_args, arg_offsets, arg_tags)
	   = mkTaggedVirtStkOffsets vSp idPrimRep all_args

	-- Arg mapping for the fast entry point; as many args as poss in
	-- registers; the rest on the stack
    	-- 	arg_regs are the registers used for arg passing
	-- 	stk_args are the args which are passed on the stack
	--
	-- Args passed on the stack are tagged, but the tags may not
	-- actually be present (just gaps) if the function is called 
	-- by jumping directly to the fast entry point.
	--
    	arg_regs = case entry_conv of
		DirectEntry lbl arity regs -> regs
		other 		           -> trace ("*** closureCodeBody:arg_regs " ++ (pprHWL entry_conv) ++ "(HWL ignored; no args passed in regs)") []

        pprHWL :: EntryConvention -> String    
        pprHWL (ViaNode) = "ViaNode"
        pprHWL (StdEntry cl) = "StdEntry"
        pprHWL (DirectEntry cl i l) = "DirectEntry"

	num_arg_regs = length arg_regs
	
    	(reg_args, stk_args) = splitAt num_arg_regs all_args

    	(sp_stk_args, stk_offsets, stk_tags)
	  = mkTaggedVirtStkOffsets vSp idPrimRep stk_args

	-- HWL; Note: empty list of live regs in slow entry code
	-- Old version (reschedule combined with heap check);
	-- see argSatisfactionCheck for new version
	--slow_entry_code = forceHeapCheck [node] True slow_entry_code'
	--		  where node = UnusedReg PtrRep 1
	--slow_entry_code = forceHeapCheck [] True slow_entry_code'

    	slow_entry_code
	  = profCtrC SLIT("TICK_ENT_FUN_STD") [
		    CLbl ticky_ctr_label DataPtrRep
	    ] `thenC`

	    -- Bind args, and record expected position of stk ptrs
	    mapCs bindNewToStack arg_offsets	  	    `thenC`
	    setRealAndVirtualSp sp_all_args		    `thenC`

	    argSatisfactionCheck closure_info	arg_regs	    `thenC`

	    -- OK, so there are enough args.  Now we need to stuff as
	    -- many of them in registers as the fast-entry code
	    -- expects. Note that the zipWith will give up when it hits
	    -- the end of arg_regs.

	    mapFCs getCAddrMode all_args	    `thenFC` \ stk_amodes ->
	    absC (mkAbstractCs (zipWith assign_to_reg arg_regs stk_amodes)) 
							    `thenC`

	    -- Now adjust real stack pointers (no need to adjust Hp,
	    -- but call this function for convenience).
	    adjustSpAndHp sp_stk_args			`thenC`

    	    absC (CFallThrough (CLbl fast_label CodePtrRep))

	assign_to_reg reg_id amode = CAssign (CReg reg_id) amode

	-- HWL
	-- Old version (reschedule combined with heap check);
	-- see argSatisfactionCheck for new version
	-- fast_entry_code = forceHeapCheck [] True fast_entry_code'

	fast_entry_code
	  = moduleName		`thenFC` \ mod_name ->
	    profCtrC SLIT("TICK_CTR") [ 
		CLbl ticky_ctr_label DataPtrRep,
		mkCString (_PK_ (ppr_for_ticky_name mod_name name)),
		mkIntCLit stg_arity,	-- total # of args
		mkIntCLit sp_stk_args,	-- # passed on stk
		mkCString (_PK_ (map (showTypeCategory . idType) all_args))
      	    ] `thenC`

	    profCtrC SLIT("TICK_ENT_FUN_DIRECT") [
		    CLbl ticky_ctr_label DataPtrRep
	    ] `thenC`

-- Nuked for now; see comment at end of file
--		    CString (_PK_ (show_wrapper_name wrapper_maybe)),
--		    CString (_PK_ (show_wrapper_arg_kinds wrapper_maybe))


		-- Bind args to regs/stack as appropriate, and
		-- record expected position of sps.
	    bindArgsToRegs reg_args arg_regs		    `thenC`
	    mapCs bindNewToStack stk_offsets		    `thenC`
	    setRealAndVirtualSp sp_stk_args		    `thenC`

	    	-- free up the stack slots containing tags
	    freeStackSlots (map fst stk_tags)		    `thenC`

		-- Enter the closures cc, if required
	    enterCostCentreCode closure_info cc IsFunction False `thenC`

		-- Do the business
	    funWrapper closure_info arg_regs stk_tags info_label (cgExpr body)
    in

    setTickyCtrLabel ticky_ctr_label (

 	-- Make a labelled code-block for the slow and fast entry code
      forkAbsC (if slow_code_needed then slow_entry_code else absC AbsCNop)
				`thenFC` \ slow_abs_c ->
      forkAbsC fast_entry_code	`thenFC` \ fast_abs_c ->
      moduleName			`thenFC` \ mod_name ->

	-- Now either construct the info table, or put the fast code in alone
	-- (We never have slow code without an info table)
	-- XXX probably need the info table and slow entry code in case of
	-- a heap check failure.
      absC (
       if info_table_needed then
	  CClosureInfoAndCode closure_info slow_abs_c (Just fast_abs_c)
			(cl_descr mod_name)
       else
	CCodeBlock fast_label fast_abs_c
       )
    )
  where
    ticky_ctr_label = mkRednCountsLabel name

    stg_arity = length all_args
    lf_info = closureLFInfo closure_info

    cl_descr mod_name = closureDescription mod_name name

	-- Manufacture labels
    name       = closureName closure_info
    fast_label = mkFastEntryLabel name stg_arity
    info_label = mkInfoTableLabel name


-- When printing the name of a thing in a ticky file, we want to
-- give the module name even for *local* things.   We print
-- just "x (M)" rather that "M.x" to distinguish them from the global kind.
ppr_for_ticky_name mod_name name
  | isLocalName name = showSDocDebug (ppr name <+> (parens (ppr mod_name)))
  | otherwise	     = showSDocDebug (ppr name)
\end{code}

For lexically scoped profiling we have to load the cost centre from
the closure entered, if the costs are not supposed to be inherited.
This is done immediately on entering the fast entry point.

Load current cost centre from closure, if not inherited.
Node is guaranteed to point to it, if profiling and not inherited.

\begin{code}
data IsThunk = IsThunk | IsFunction -- Bool-like, local
-- #ifdef DEBUG
	deriving Eq
-- #endif

enterCostCentreCode 
   :: ClosureInfo -> CostCentreStack
   -> IsThunk
   -> Bool	-- is_box: this closure is a special box introduced by SCCfinal
   -> Code

enterCostCentreCode closure_info ccs is_thunk is_box
  = if not opt_SccProfilingOn then
	nopC
    else
	ASSERT(not (noCCSAttached ccs))

	if isSubsumedCCS ccs then
	    ASSERT(isToplevClosure closure_info)
	    ASSERT(is_thunk == IsFunction)
	    costCentresC SLIT("ENTER_CCS_FSUB") []
 
	else if isCurrentCCS ccs then 
	    if re_entrant && not is_box
		then costCentresC SLIT("ENTER_CCS_FCL") [CReg node]
		else costCentresC SLIT("ENTER_CCS_TCL") [CReg node]

	else if isCafCCS ccs then
	    ASSERT(isToplevClosure closure_info)
	    ASSERT(is_thunk == IsThunk)
		-- might be a PAP, in which case we want to subsume costs
	    if re_entrant
		then costCentresC SLIT("ENTER_CCS_FSUB") []
		else costCentresC SLIT("ENTER_CCS_CAF") c_ccs

	else panic "enterCostCentreCode"

   where
	c_ccs = [mkCCostCentreStack ccs]
	re_entrant = closureReEntrant closure_info
\end{code}

%************************************************************************
%*									*
\subsubsection[pre-closure-code-stuff]{Pre-closure-code code}
%*									*
%************************************************************************

The argument-satisfaction check code is placed after binding
the arguments to their stack locations. Hence, the virtual stack
pointer is pointing after all the args, and virtual offset 1 means
the base of frame and hence most distant arg.  Hence
virtual offset 0 is just beyond the most distant argument; the
relative offset of this word tells how many words of arguments
are expected.

\begin{code}
argSatisfactionCheck :: ClosureInfo -> [MagicId] {-GRAN-} -> Code

argSatisfactionCheck closure_info arg_regs

  = nodeMustPointToIt (closureLFInfo closure_info)   `thenFC` \ node_points ->

--      let
--         emit_gran_macros = opt_GranMacros
--      in

    -- HWL  ngo' ngoq:
    -- absC (CMacroStmt GRAN_FETCH []) 			`thenC`
    -- forceHeapCheck [] node_points (absC AbsCNop)			`thenC`
    --(if opt_GranMacros
    --  then if node_points 
    --         then fetchAndReschedule  arg_regs node_points 
    --         else yield arg_regs node_points
    --  else absC AbsCNop)                       `thenC`

        getSpRelOffset 0 	`thenFC` \ (SpRel sp) ->
	let
	    off = I# sp
	    rel_arg = mkIntCLit off
	in
	ASSERT(off /= 0)
	if node_points then
	    absC (CMacroStmt ARGS_CHK [rel_arg]) -- node already points
	else
	    absC (CMacroStmt ARGS_CHK_LOAD_NODE [rel_arg, set_Node_to_this])
  where
    -- We must tell the arg-satis macro whether Node is pointing to
    -- the closure or not.  If it isn't so pointing, then we give to
    -- the macro the (static) address of the closure.

    set_Node_to_this = CLbl (closureLabelFromCI closure_info) PtrRep
\end{code}

%************************************************************************
%*									*
\subsubsection[closure-code-wrappers]{Wrappers around closure code}
%*									*
%************************************************************************

\begin{code}
thunkWrapper:: ClosureInfo -> CLabel -> Code -> Code
thunkWrapper closure_info lbl thunk_code
  = 	-- Stack and heap overflow checks
    nodeMustPointToIt (closureLFInfo closure_info) `thenFC` \ node_points ->

    -- HWL: insert macros for GrAnSim; 2 versions depending on liveness of node
    -- (we prefer fetchAndReschedule-style context switches to yield ones)
    (if opt_GranMacros
       then if node_points 
              then fetchAndReschedule [] node_points 
              else yield [] node_points
       else absC AbsCNop)                       `thenC`

        -- stack and/or heap checks
    thunkChecks lbl node_points (

	-- Overwrite with black hole if necessary
    blackHoleIt closure_info node_points  `thenC`

    setupUpdate closure_info (			-- setupUpdate *encloses* the rest

	-- Finally, do the business
    thunk_code
    ))

funWrapper :: ClosureInfo 	-- Closure whose code body this is
	   -> [MagicId] 	-- List of argument registers (if any)
	   -> [(VirtualSpOffset,Int)] -- tagged stack slots
	   -> CLabel		-- info table for heap check ret.
	   -> Code		-- Body of function being compiled
	   -> Code
funWrapper closure_info arg_regs stk_tags info_label fun_body
  = 	-- Stack overflow check
    nodeMustPointToIt (closureLFInfo closure_info)  	`thenFC` \ node_points ->
    -- HWL   chu' ngoq:
    (if opt_GranMacros
       then yield arg_regs node_points
       else absC AbsCNop)                                 `thenC`

        -- heap and/or stack checks
    fastEntryChecks arg_regs stk_tags info_label node_points (

	-- Finally, do the business
    fun_body
    )
\end{code}


%************************************************************************
%*									*
\subsubsubsection[update-and-BHs]{Update and black-hole wrappers}
%*									*
%************************************************************************


\begin{code}
blackHoleIt :: ClosureInfo -> Bool -> Code	-- Only called for closures with no args

blackHoleIt closure_info node_points
  = if blackHoleOnEntry closure_info && node_points
    then
	let
	  info_label = infoTableLabelFromCI closure_info
	  args = [ CLbl info_label DataPtrRep ]
	in
	absC (if closureSingleEntry(closure_info) then
		CMacroStmt UPD_BH_SINGLE_ENTRY args
	      else
		CMacroStmt UPD_BH_UPDATABLE args)
    else
	nopC
\end{code}

\begin{code}
setupUpdate :: ClosureInfo -> Code -> Code	-- Only called for closures with no args
	-- Nota Bene: this function does not change Node (even if it's a CAF),
	-- so that the cost centre in the original closure can still be
	-- extracted by a subsequent ENTER_CC_TCL

-- I've tidied up the code for this function, but it should still do the same as
-- it did before (modulo ticky stuff).  KSW 1999-04.
setupUpdate closure_info code
 = if closureReEntrant closure_info
   then
     code
   else
     case (closureUpdReqd closure_info, isStaticClosure closure_info) of
       (False,False) -> profCtrC SLIT("TICK_UPDF_OMITTED") [] `thenC`
	                code
       (False,True ) -> (if opt_DoTickyProfiling
                         then
                         -- blackhole the SE CAF
                           link_caf seCafBlackHoleClosureInfo `thenFC` \ _ -> nopC
                         else
                           nopC)                                                       `thenC`
                        profCtrC SLIT("TICK_UPD_CAF_BH_SINGLE_ENTRY") [mkCString cl_name] `thenC`
                        profCtrC SLIT("TICK_UPDF_OMITTED") []                           `thenC`
	                code
       (True ,False) -> pushUpdateFrame (CReg node) code
       (True ,True ) -> -- blackhole the (updatable) CAF:
                        link_caf cafBlackHoleClosureInfo           `thenFC` \ update_closure ->
                        profCtrC SLIT("TICK_UPD_CAF_BH_UPDATABLE") [mkCString cl_name]    `thenC`
                        pushUpdateFrame update_closure code
 where
   cl_name :: FAST_STRING
   cl_name  = (occNameFS . nameOccName . closureName) closure_info

   link_caf :: (ClosureInfo -> ClosureInfo)  -- function yielding BH closure_info
            -> FCode CAddrMode	             -- Returns amode for closure to be updated
   link_caf bhCI
     = -- To update a CAF we must allocate a black hole, link the CAF onto the
       -- CAF list, then update the CAF to point to the fresh black hole.
       -- This function returns the address of the black hole, so it can be
       -- updated with the new value when available.

             -- Alloc black hole specifying CC_HDR(Node) as the cost centre
             --   Hack Warning: Using a CLitLit to get CAddrMode !
       let
           use_cc   = CLitLit SLIT("CCS_HDR(R1.p)") PtrRep
           blame_cc = use_cc
       in
       allocDynClosure (bhCI closure_info) use_cc blame_cc []  `thenFC` \ heap_offset ->
       getHpRelOffset heap_offset                              `thenFC` \ hp_rel ->
       let  amode = CAddr hp_rel
       in
       absC (CMacroStmt UPD_CAF [CReg node, amode])            `thenC`
       returnFC amode
\end{code}

%************************************************************************
%*									*
\subsection[CgClosure-Description]{Profiling Closure Description.}
%*									*
%************************************************************************

For "global" data constructors the description is simply occurrence
name of the data constructor itself (see \ref{CgConTbls-info-tables}).

Otherwise it is determind by @closureDescription@ from the let
binding information.

\begin{code}
closureDescription :: Module		-- Module
		   -> Name		-- Id of closure binding
		   -> String

	-- Not called for StgRhsCon which have global info tables built in
	-- CgConTbls.lhs with a description generated from the data constructor

closureDescription mod_name name
  = showSDoc (
	hcat [char '<',
		   pprModule mod_name,
		   char '.',
		   ppr name,
		   char '>'])
\end{code}

\begin{code}
chooseDynCostCentres ccs args fvs body
  = let
	use_cc -- cost-centre we record in the object
	  = if currentOrSubsumedCCS ccs
	    then CReg CurCostCentre
	    else mkCCostCentreStack ccs

	blame_cc -- cost-centre on whom we blame the allocation
	  = case (args, fvs, body) of
	      ([], _, StgApp fun [{-no args-}])
		-> mkCCostCentreStack overheadCCS
	      _ -> use_cc

	    -- if it's an utterly trivial RHS, then it must be
	    -- one introduced by boxHigherOrderArgs for profiling,
	    -- so we charge it to "OVERHEAD".

	    -- This looks like a HACK to me --SDM
    in
    (use_cc, blame_cc)
\end{code}



========================================================================
OLD CODE THAT EMITTED INFORMATON FOR QUANTITATIVE ANALYSIS

It's pretty wierd, so I've nuked it for now.  SLPJ Nov 96

\begin{pseudocode}
getWrapperArgTypeCategories
	:: Type				-- wrapper's type
	-> StrictnessInfo bdee		-- strictness info about its args
	-> Maybe String

getWrapperArgTypeCategories _ NoStrictnessInfo	    = Nothing
getWrapperArgTypeCategories _ BottomGuaranteed
  = trace "getWrapperArgTypeCategories:BottomGuaranteed!" Nothing  -- wrong
getWrapperArgTypeCategories _ (StrictnessInfo [] _) = Nothing

getWrapperArgTypeCategories ty (StrictnessInfo arg_info _)
  = Just (mkWrapperArgTypeCategories ty arg_info)

mkWrapperArgTypeCategories
	:: Type		-- wrapper's type
	-> [Demand]	-- info about its arguments
	-> String	-- a string saying lots about the args

mkWrapperArgTypeCategories wrapper_ty wrap_info
  = case (splitFunTy_maybe wrapper_ty) of { Just (arg_tys,_) ->
    map do_one (wrap_info `zip` (map showTypeCategory arg_tys)) }
  where
    -- ToDo: this needs FIXING UP (it was a hack anyway...)
    do_one (WwPrim, _) = 'P'
    do_one (WwEnum, _) = 'E'
    do_one (WwStrict, arg_ty_char) = arg_ty_char
    do_one (WwUnpack _ _ _, arg_ty_char)
      = if arg_ty_char `elem` "CIJFDTS"
	then toLower arg_ty_char
	else if arg_ty_char == '+' then 't'
	else trace ("mkWrapp..:funny char:"++[arg_ty_char]) '-'
    do_one (other_wrap_info, _) = '-'
\end{pseudocode}

