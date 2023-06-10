%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
%************************************************************************
%*									*
\section[PprAbsC]{Pretty-printing Abstract~C}
%*									*
%************************************************************************

\begin{code}
module PprAbsC (
	writeRealC,
	dumpRealC
#ifdef DEBUG
	, pprAmode -- otherwise, not exported
#endif
    ) where

#include "HsVersions.h"

import IO	( Handle )

import AbsCSyn
import ClosureInfo
import AbsCUtils	( getAmodeRep, nonemptyAbsC,
			  mixedPtrLocn, mixedTypeLocn
			)
import Constants	( spARelToInt, spBRelToInt, mIN_UPD_SIZE )
import CLabel		( externallyVisibleCLabel, mkErrorStdEntryLabel,
			  isReadOnly, needsCDecl, pprCLabel,
			  CLabel{-instance Ord-}
			)
import CmdLineOpts	( opt_SccProfilingOn )
import CostCentre	( uppCostCentre, uppCostCentreDecl )
import Costs		( costs, addrModeCosts, CostRes(..), Side(..) )
import CStrings		( stringToC )
import FiniteMap	( addToFM, emptyFM, lookupFM, FiniteMap )
import HeapOffs		( isZeroOff, subOff, pprHeapOffset )
import Literal		( showLiteral, Literal(..) )
import Maybes		( maybeToBool, catMaybes )
import PrimOp		( primOpNeedsWrapper, pprPrimOp, PrimOp(..) )
import PrimRep		( isFloatingRep, PrimRep(..) )
import SMRep		( getSMInfoStr, getSMInitHdrStr, getSMUpdInplaceHdrStr,
			  isConstantRep, isSpecRep, isPhantomRep
			)
import Unique		( pprUnique, Unique{-instance NamedThing-} )
import UniqSet		( emptyUniqSet, elementOfUniqSet,
			  addOneToUniqSet, UniqSet
			)
import Outputable
import Util		( nOfThem, panic, assertPanic )

infixr 9 `thenTE`
\end{code}

For spitting out the costs of an abstract~C expression, @writeRealC@
now not only prints the C~code of the @absC@ arg but also adds a macro
call to a cost evaluation function @GRAN_EXEC@. For that,
@pprAbsC@ has a new ``costs'' argument.  %% HWL

\begin{code}
writeRealC :: Handle -> AbstractC -> IO ()
--writeRealC handle absC = 
-- _scc_ "writeRealC" 
-- printDoc LeftMode handle (pprAbsC absC (costs absC))
writeRealC handle absC = 
 _scc_ "writeRealC" 
 printForC handle (pprAbsC absC (costs absC))

dumpRealC :: AbstractC -> SDoc
dumpRealC absC = pprAbsC absC (costs absC)
\end{code}

This emits the macro,  which is used in GrAnSim  to compute the total costs
from a cost 5 tuple. %%  HWL

\begin{code}
emitMacro :: CostRes -> SDoc

-- ToDo: Check a compile time flag to decide whether a macro should be emitted
emitMacro (Cost (i,b,l,s,f))
  = hcat [ ptext SLIT("GRAN_EXEC"), char '(',
                          int i, comma, int b, comma, int l, comma,
	                  int s, comma, int f, pp_paren_semi ]

pp_paren_semi = text ");"
\end{code}

New type: Now pprAbsC also takes the costs for evaluating the Abstract C
code as an argument (that's needed when spitting out the GRAN_EXEC macro
which must be done before the return i.e. inside absC code)   HWL

\begin{code}
pprAbsC :: AbstractC -> CostRes -> SDoc
pprAbsC AbsCNop _ = empty
pprAbsC (AbsCStmts s1 s2) c = ($$) (pprAbsC s1 c) (pprAbsC s2 c)

pprAbsC (CClosureUpdInfo info) c
  = pprAbsC info c

pprAbsC (CAssign dest src) _ = pprAssign (getAmodeRep dest) dest src
pprAbsC (CJump target) c
  = ($$) (hcat [emitMacro c {-WDP:, text "/* <--++  CJump */"-} ])
	     (hcat [ text jmp_lit, pprAmode target, pp_paren_semi ])

pprAbsC (CFallThrough target) c
  = ($$) (hcat [emitMacro c {-WDP:, text "/* <--++  CFallThrough */"-} ])
	     (hcat [ text jmp_lit, pprAmode target, pp_paren_semi ])

-- --------------------------------------------------------------------------
-- Spit out GRAN_EXEC macro immediately before the return                 HWL

pprAbsC (CReturn am return_info)  c
  = ($$) (hcat [emitMacro c {-WDP:, text "/* <----  CReturn */"-} ])
	     (hcat [text jmp_lit, target, pp_paren_semi ])
  where
   target = case return_info of
    	DirectReturn -> hcat [ptext SLIT("DIRECT"),char '(', pprAmode am, rparen]
	DynamicVectoredReturn am' -> mk_vector (pprAmode am')
	StaticVectoredReturn n -> mk_vector (int n)	-- Always positive
   mk_vector x = hcat [parens (pprAmode am), brackets (text "RVREL" <> parens x)]

pprAbsC (CSplitMarker) _ = ptext SLIT("/* SPLIT */")

-- we optimise various degenerate cases of CSwitches.

-- --------------------------------------------------------------------------
-- Assume: CSwitch is also end of basic block
--         costs function yields nullCosts for whole switch
--         ==> inherited costs c are those of basic block up to switch
--         ==> inherit c + costs for the corresponding branch
--                                                                       HWL
-- --------------------------------------------------------------------------

pprAbsC (CSwitch discrim [] deflt) c
  = pprAbsC deflt (c + costs deflt)
    -- Empty alternative list => no costs for discrim as nothing cond. here HWL

pprAbsC (CSwitch discrim [(tag,alt_code)] deflt) c -- only one alt
  = case (nonemptyAbsC deflt) of
      Nothing ->		-- one alt and no default
		 pprAbsC alt_code (c + costs alt_code)
		 -- Nothing conditional in here either  HWL

      Just dc ->		-- make it an "if"
		 do_if_stmt discrim tag alt_code dc c

pprAbsC (CSwitch discrim [(tag1@(MachInt i1 _), alt_code1),
			      (tag2@(MachInt i2 _), alt_code2)] deflt) c
  | empty_deflt && ((i1 == 0 && i2 == 1) || (i1 == 1 && i2 == 0))
  = if (i1 == 0) then
	do_if_stmt discrim tag1 alt_code1 alt_code2 c
    else
	do_if_stmt discrim tag2 alt_code2 alt_code1 c
  where
    empty_deflt = not (maybeToBool (nonemptyAbsC deflt))

pprAbsC (CSwitch discrim alts deflt) c -- general case
  | isFloatingRep (getAmodeRep discrim)
    = pprAbsC (foldr ( \ a -> CSwitch discrim [a]) deflt alts) c
  | otherwise
    = vcat [
	hcat [text "switch (", pp_discrim, text ") {"],
	nest 2 (vcat (map ppr_alt alts)),
	(case (nonemptyAbsC deflt) of
	   Nothing -> empty
	   Just dc ->
	    nest 2 (vcat [ptext SLIT("default:"),
				  pprAbsC dc (c + switch_head_cost
						    + costs dc),
				  ptext SLIT("break;")])),
	char '}' ]
  where
    pp_discrim
      = pprAmode discrim

    ppr_alt (lit, absC)
      = vcat [ hcat [ptext SLIT("case "), pprBasicLit lit, char ':'],
		   nest 2 (($$) (pprAbsC absC (c + switch_head_cost + costs absC))
				       (ptext SLIT("break;"))) ]

    -- Costs for addressing header of switch and cond. branching        -- HWL
    switch_head_cost = addrModeCosts discrim Rhs + (Cost (0, 1, 0, 0, 0))

pprAbsC stmt@(COpStmt results op@(CCallOp _ _ _ _ _) args liveness_mask vol_regs) _
  = pprCCall op args results liveness_mask vol_regs

pprAbsC stmt@(COpStmt results op args liveness_mask vol_regs) _
  = let
	non_void_args = grab_non_void_amodes args
	non_void_results = grab_non_void_amodes results
	-- if just one result, we print in the obvious "assignment" style;
	-- if 0 or many results, we emit a macro call, w/ the results
	-- followed by the arguments.  The macro presumably knows which
	-- are which :-)

    	the_op = ppr_op_call non_void_results non_void_args
		-- liveness mask is *in* the non_void_args
    in
    case (ppr_vol_regs vol_regs) of { (pp_saves, pp_restores) ->
    if primOpNeedsWrapper op then
    	vcat [  pp_saves,
    	    	the_op,
    	    	pp_restores
    	     ]
    else
    	the_op
    }
  where
    ppr_op_call results args
      = hcat [ pprPrimOp op, lparen,
	hcat (punctuate comma (map ppr_op_result results)),
	if null results || null args then empty else comma,
	hcat (punctuate comma (map pprAmode args)),
	pp_paren_semi ]

    ppr_op_result r = ppr_amode r
      -- primop macros do their own casting of result;
      -- hence we can toss the provided cast...

pprAbsC (CSimultaneous abs_c) c
  = hcat [ptext SLIT("{{"), pprAbsC abs_c c, ptext SLIT("}}")]

pprAbsC stmt@(CMacroStmt macro as) _
  = hcat [text (show macro), lparen,
	hcat (punctuate comma (map ppr_amode as)),pp_paren_semi] -- no casting
pprAbsC stmt@(CCallProfCtrMacro op as) _
  = hcat [ptext op, lparen,
	hcat (punctuate comma (map ppr_amode as)),pp_paren_semi]
pprAbsC stmt@(CCallProfCCMacro op as) _
  = hcat [ptext op, lparen,
	hcat (punctuate comma (map ppr_amode as)),pp_paren_semi]

pprAbsC (CCodeBlock label abs_C) _
  = ASSERT( maybeToBool(nonemptyAbsC abs_C) )
    case (pprTempAndExternDecls abs_C) of { (pp_temps, pp_exts) ->
    vcat [
	hcat [text (if (externallyVisibleCLabel label)
			  then "FN_("	-- abbreviations to save on output
			  else "IFN_("),
		   pprCLabel label, text ") {"],

	pp_exts, pp_temps,

	nest 8 (ptext SLIT("FB_")),
	nest 8 (pprAbsC abs_C (costs abs_C)),
	nest 8 (ptext SLIT("FE_")),
	char '}' ]
    }

pprAbsC (CInitHdr cl_info reg_rel cost_centre inplace_upd) _
  = hcat [ pp_init_hdr, text "_HDR(",
		ppr_amode (CAddr reg_rel), comma,
		pprCLabel info_lbl, comma,
		if_profiling (pprAmode cost_centre), comma,
		pprHeapOffset size, comma, int ptr_wds, pp_paren_semi ]
  where
    info_lbl	= infoTableLabelFromCI cl_info
    sm_rep	= closureSMRep	   cl_info
    size	= closureSizeWithoutFixedHdr cl_info
    ptr_wds	= closurePtrsSize  cl_info

    pp_init_hdr = text (if inplace_upd then
			    getSMUpdInplaceHdrStr sm_rep
		  	else
			    getSMInitHdrStr sm_rep)

pprAbsC stmt@(CStaticClosure closure_lbl cl_info cost_centre amodes) _
  = case (pprTempAndExternDecls stmt) of { (_, pp_exts) ->
    vcat [
	pp_exts,
	hcat [
		ptext SLIT("SET_STATIC_HDR"),char '(',
		pprCLabel closure_lbl,			comma,
		pprCLabel info_lbl,				comma,
		if_profiling (pprAmode cost_centre),	comma,
		ppLocalness closure_lbl,			comma,
		ppLocalnessMacro False{-for data-} info_lbl,
		char ')'
		],
	nest 2 (hcat (map ppr_item amodes)),
	nest 2 (hcat (map ppr_item padding_wds)),
	ptext SLIT("};") ]
    }
  where
    info_lbl = infoTableLabelFromCI cl_info

    ppr_item item
      = if getAmodeRep item == VoidRep
	then text ", (W_) 0" -- might not even need this...
	else (<>) (text ", (W_)") (ppr_amode item)

    padding_wds =
	if not (closureUpdReqd cl_info) then
	    []
    	else
	    case (max 0 (mIN_UPD_SIZE - length amodes)) of { still_needed ->
	    nOfThem still_needed (mkIntCLit 0) } -- a bunch of 0s

{-
   STATIC_INIT_HDR(c,i,localness) blows into:
	localness W_ c_closure [] = { i_info, extra_fixed_wd<1..n>

   then *NO VarHdr STUFF FOR STATIC*...

   then the amodes are dropped in...
	,a1 ,a2 ... ,aN
   then a close brace:
	};
-}

pprAbsC stmt@(CClosureInfoAndCode cl_info slow maybe_fast upd cl_descr liveness) _
  = vcat [
	hcat [
	    pp_info_rep,
	    ptext SLIT("_ITBL"),char '(',
	    pprCLabel info_lbl,			comma,

		-- CONST_ITBL needs an extra label for
		-- the static version of the object.
	    if isConstantRep sm_rep
	    then (<>) (pprCLabel (closureLabelFromCI cl_info)) comma
	    else empty,

	    pprCLabel slow_lbl,	comma,
    	    pprAmode upd,		comma,
	    int liveness,		comma,

	    pp_tag,			comma,
	    pp_size, 			comma,
	    pp_ptr_wds,			comma,

	    ppLocalness info_lbl,				comma,
	    ppLocalnessMacro True{-function-} slow_lbl,		comma,

	    if is_selector
	    then (<>) (int select_word_i) comma
	    else empty,

	    if_profiling pp_kind, comma,
	    if_profiling pp_descr, comma,
	    if_profiling pp_type,
	    text ");"
	],
	pp_slow,
	case maybe_fast of
	    Nothing -> empty
	    Just fast -> let stuff = CCodeBlock fast_lbl fast in
			 pprAbsC stuff (costs stuff)
    ]
  where
    info_lbl	= infoTableLabelFromCI cl_info
    fast_lbl    = fastLabelFromCI cl_info
    sm_rep	= closureSMRep	  cl_info

    (slow_lbl, pp_slow)
      = case (nonemptyAbsC slow) of
	  Nothing -> (mkErrorStdEntryLabel, empty)
	  Just xx -> (entryLabelFromCI cl_info,
		       let stuff = CCodeBlock slow_lbl xx in
		       pprAbsC stuff (costs stuff))

    maybe_selector = maybeSelectorInfo cl_info
    is_selector = maybeToBool maybe_selector
    (Just (_, select_word_i)) = maybe_selector

    pp_info_rep	    -- special stuff if it's a selector; otherwise, just the SMrep
      = text (if is_selector then "SELECT" else (getSMInfoStr sm_rep))

    pp_tag = int (closureSemiTag cl_info)

    is_phantom = isPhantomRep sm_rep

    pp_size = if isSpecRep sm_rep then	-- exploiting: SPEC_VHS == 0 (always)
		 int (closureNonHdrSize cl_info)

	      else if is_phantom then	-- do not have sizes for these
		 empty
	      else
		 pprHeapOffset (closureSizeWithoutFixedHdr cl_info)

    pp_ptr_wds	= if is_phantom then
		     empty
		  else
		     int (closurePtrsSize cl_info)

    pp_kind  = text (closureKind cl_info)
    pp_descr = hcat [char '"', text (stringToC cl_descr), char '"']
    pp_type  = hcat [char '"', text (stringToC (closureTypeDescr cl_info)), char '"']

pprAbsC (CRetVector lbl maybes deflt) c
  = vcat [ ptext SLIT("{ // CRetVector (lbl????)"),
	       nest 8 (sep (map ppr_maybe_amode maybes)),
	       text "} /*default=*/ {", pprAbsC deflt c,
	       char '}']
  where
    ppr_maybe_amode Nothing  = ptext SLIT("/*default*/")
    ppr_maybe_amode (Just a) = pprAmode a

pprAbsC stmt@(CRetUnVector label amode) _
  = hcat [ptext SLIT("UNVECTBL"),char '(', pp_static, comma, pprCLabel label, comma,
	    pprAmode amode, rparen]
  where
    pp_static = if externallyVisibleCLabel label then empty else ptext SLIT("static")

pprAbsC stmt@(CFlatRetVector label amodes) _
  =	case (pprTempAndExternDecls stmt) of { (_, pp_exts) ->
	vcat [
	    pp_exts,
	    hcat [ppLocalness label, ptext SLIT(" W_ "),
    	    	       pprCLabel label, text "[] = {"],
	    nest 2 (sep (punctuate comma (map ppr_item amodes))),
	    text "};" ] }
  where
    ppr_item item = (<>) (text "(W_) ") (ppr_amode item)

pprAbsC (CCostCentreDecl is_local cc) _ = uppCostCentreDecl is_local cc
\end{code}

\begin{code}
ppLocalness label
  = (<>) static const
  where
    static = if (externallyVisibleCLabel label) then empty else ptext SLIT("static ")
    const  = if not (isReadOnly label)	        then empty else ptext SLIT("const")

ppLocalnessMacro for_fun{-vs data-} clabel
  = hcat [ char (if externallyVisibleCLabel clabel then 'E' else 'I'),
                 if for_fun then 
                    ptext SLIT("F_") 
                 else 
                    (<>) (ptext SLIT("D_"))
                              (if isReadOnly clabel then 
			          ptext SLIT("RO_") 
			       else 
			          empty)]
\end{code}

\begin{code}
jmp_lit = "JMP_("

grab_non_void_amodes amodes
  = filter non_void amodes

non_void amode
  = case (getAmodeRep amode) of
      VoidRep -> False
      k	-> True
\end{code}

\begin{code}
ppr_vol_regs :: [MagicId] -> (SDoc, SDoc)

ppr_vol_regs [] = (empty, empty)
ppr_vol_regs (VoidReg:rs) = ppr_vol_regs rs
ppr_vol_regs (r:rs)
  = let pp_reg = case r of
    	    	    VanillaReg pk n -> pprVanillaReg n
    	    	    _ -> pprMagicId r
	(more_saves, more_restores) = ppr_vol_regs rs
    in
    (($$) ((<>) (ptext SLIT("CALLER_SAVE_"))    pp_reg) more_saves,
     ($$) ((<>) (ptext SLIT("CALLER_RESTORE_")) pp_reg) more_restores)

-- pp_basic_{saves,restores}: The BaseReg, SpA, SuA, SpB, SuB, Hp and
-- HpLim (see StgRegs.lh) may need to be saved/restored around CCalls,
-- depending on the platform.  (The "volatile regs" stuff handles all
-- other registers.)  Just be *sure* BaseReg is OK before trying to do
-- anything else.
pp_basic_saves
  = vcat [
	ptext SLIT("CALLER_SAVE_Base"),
	ptext SLIT("CALLER_SAVE_SpA"),
	ptext SLIT("CALLER_SAVE_SuA"),
	ptext SLIT("CALLER_SAVE_SpB"),
	ptext SLIT("CALLER_SAVE_SuB"),
	ptext SLIT("CALLER_SAVE_Ret"),
--	ptext SLIT("CALLER_SAVE_Activity"),
	ptext SLIT("CALLER_SAVE_Hp"),
	ptext SLIT("CALLER_SAVE_HpLim") ]

pp_basic_restores
  = vcat [
	ptext SLIT("CALLER_RESTORE_Base"), -- must be first!
	ptext SLIT("CALLER_RESTORE_SpA"),
	ptext SLIT("CALLER_RESTORE_SuA"),
	ptext SLIT("CALLER_RESTORE_SpB"),
	ptext SLIT("CALLER_RESTORE_SuB"),
	ptext SLIT("CALLER_RESTORE_Ret"),
--	ptext SLIT("CALLER_RESTORE_Activity"),
	ptext SLIT("CALLER_RESTORE_Hp"),
	ptext SLIT("CALLER_RESTORE_HpLim"),
	ptext SLIT("CALLER_RESTORE_StdUpdRetVec"),
	ptext SLIT("CALLER_RESTORE_StkStub") ]
\end{code}

\begin{code}
if_profiling pretty
  = if  opt_SccProfilingOn
    then pretty
    else char '0' -- leave it out!
-- ---------------------------------------------------------------------------
-- Changes for GrAnSim:
--  draw costs for computation in head of if into both branches;
--  as no abstractC data structure is given for the head, one is constructed
--  guessing unknown values and fed into the costs function
-- ---------------------------------------------------------------------------

do_if_stmt discrim tag alt_code deflt c
  = case tag of
      -- This special case happens when testing the result of a comparison.
      -- We can just avoid some redundant clutter in the output.
      MachInt n _ | n==0 -> ppr_if_stmt (pprAmode discrim)
				      deflt alt_code
				      (addrModeCosts discrim Rhs) c
      other              -> let
			       cond = hcat [ pprAmode discrim,
					  ptext SLIT(" == "),
					  pprAmode (CLit tag) ]
			    in
			    ppr_if_stmt cond
					 alt_code deflt
					 (addrModeCosts discrim Rhs) c

ppr_if_stmt pp_pred then_part else_part discrim_costs c
  = vcat [
      hcat [text "if (", pp_pred, text ") {"],
      nest 8 (pprAbsC then_part 	(c + discrim_costs +
				       	(Cost (0, 2, 0, 0, 0)) +
					costs then_part)),
      (case nonemptyAbsC else_part of Nothing -> empty; Just _ -> text "} else {"),
      nest 8 (pprAbsC else_part  (c + discrim_costs +
					(Cost (0, 1, 0, 0, 0)) +
					costs else_part)),
      char '}' ]
    {- Total costs = inherited costs (before if) + costs for accessing discrim
		     + costs for cond branch ( = (0, 1, 0, 0, 0) )
		     + costs for that alternative
    -}
\end{code}

Historical note: this used to be two separate cases -- one for `ccall'
and one for `casm'.  To get round a potential limitation to only 10
arguments, the numbering of arguments in @process_casm@ was beefed up a
bit. ADR

Some rough notes on generating code for @CCallOp@:

1) Evaluate all arguments and stuff them into registers. (done elsewhere)
2) Save any essential registers (heap, stack, etc).

   ToDo: If stable pointers are in use, these must be saved in a place
   where the runtime system can get at them so that the Stg world can
   be restarted during the call.

3) Save any temporary registers that are currently in use.
4) Do the call putting result into a local variable
5) Restore essential registers
6) Restore temporaries

   (This happens after restoration of essential registers because we
   might need the @Base@ register to access all the others correctly.)

{- Doesn't apply anymore with ForeignObj, structure created via the primop.
   makeForeignObj (i.e., ForeignObj is not CReturnable)
7) If returning Malloc Pointer, build a closure containing the
   appropriate value.
-}
   Otherwise, copy local variable into result register.

8) If ccall (not casm), declare the function being called as extern so
   that C knows if it returns anything other than an int.

\begin{pseudocode}
{ ResultType _ccall_result;
  basic_saves;
  saves;
  _ccall_result = f( args );
  basic_restores;
  restores;

  return_reg = _ccall_result;
}
\end{pseudocode}

Amendment to the above: if we can GC, we have to:

* make sure we save all our registers away where the garbage collector
  can get at them.
* be sure that there are no live registers or we're in trouble.
  (This can cause problems if you try something foolish like passing
   an array or foreign obj to a _ccall_GC_ thing.)
* increment/decrement the @inCCallGC@ counter before/after the call so
  that the runtime check that PerformGC is being used sensibly will work.

\begin{code}
pprCCall op@(CCallOp op_str is_asm may_gc _ _) args results liveness_mask vol_regs
  = if (may_gc && liveness_mask /= noLiveRegsMask)
    then pprPanic "Live register in _casm_GC_ " 
		  (doubleQuotes (text casm_str) <+> hsep pp_non_void_args)
    else
    vcat [
      char '{',
      declare_local_vars,   -- local var for *result*
      vcat local_arg_decls,
      -- if is_asm then empty else declareExtern,
      pp_save_context,
      process_casm local_vars pp_non_void_args casm_str,
      pp_restore_context,
      assign_results,
      char '}'
    ]
  where
    (pp_saves, pp_restores) = ppr_vol_regs vol_regs
    (pp_save_context, pp_restore_context) =
	if may_gc
	then (	text "do { extern StgInt inCCallGC; SaveAllStgRegs(); inCCallGC++;",
		text "inCCallGC--; RestoreAllStgRegs();} while(0);")
	else (	pp_basic_saves $$ pp_saves,
		pp_basic_restores $$ pp_restores)

    non_void_args =
	let nvas = tail args
	in ASSERT (all non_void nvas) nvas
    -- the first argument will be the "I/O world" token (a VoidRep)
    -- all others should be non-void

    non_void_results =
	let nvrs = grab_non_void_amodes results
	in ASSERT (length nvrs <= 1) nvrs
    -- there will usually be two results: a (void) state which we
    -- should ignore and a (possibly void) result.

    (local_arg_decls, pp_non_void_args)
      = unzip [ ppr_casm_arg a i | (a,i) <- non_void_args `zip` [1..] ]

    pp_liveness = pprAmode (mkIntCLit liveness_mask)

    (declare_local_vars, local_vars, assign_results)
      = ppr_casm_results non_void_results pp_liveness

    casm_str = if is_asm then _UNPK_ op_str else ccall_str

    -- Remainder only used for ccall

    ccall_str = showSDoc
	(hcat [
		if null non_void_results
		  then empty
		  else text "%r = ",
		lparen, ptext op_str, lparen,
		  hcat (punctuate comma ccall_args),
		text "));"
	])
    num_args = length non_void_args
    ccall_args = take num_args [ (<>) (char '%') (int i) | i <- [0..] ]
\end{code}

If the argument is a heap object, we need to reach inside and pull out
the bit the C world wants to see.  The only heap objects which can be
passed are @Array@s, @ByteArray@s and @ForeignObj@s.

\begin{code}
ppr_casm_arg :: CAddrMode -> Int -> (SDoc, SDoc)
    -- (a) decl and assignment, (b) local var to be used later

ppr_casm_arg amode a_num
  = let
	a_kind	 = getAmodeRep amode
	pp_amode = pprAmode amode
	pp_kind  = pprPrimKind a_kind

	local_var  = (<>) (ptext SLIT("_ccall_arg")) (int a_num)

	(arg_type, pp_amode2)
	  = case a_kind of

	      -- for array arguments, pass a pointer to the body of the array
	      -- (PTRS_ARR_CTS skips over all the header nonsense)
	      ArrayRep	    -> (pp_kind,
				hcat [ptext SLIT("PTRS_ARR_CTS"),char '(', pp_amode, rparen])
	      ByteArrayRep -> (pp_kind,
				hcat [ptext SLIT("BYTE_ARR_CTS"),char '(', pp_amode, rparen])

	      -- for ForeignObj, use FOREIGN_OBJ_DATA to fish out the contents.
	      ForeignObjRep -> (ptext SLIT("StgForeignObj"),
				hcat [ptext SLIT("ForeignObj_CLOSURE_DATA"),char '(', 
					    pp_amode, char ')'])
	      other	    -> (pp_kind, pp_amode)

	declare_local_var
	  = hcat [ arg_type, space, local_var, equals, pp_amode2, semi ]
    in
    (declare_local_var, local_var)
\end{code}

For l-values, the critical questions are:

1) Are there any results at all?

   We only allow zero or one results.

{- With the introduction of ForeignObj (MallocPtr++), no longer necess.
2) Is the result is a foreign obj?

   The mallocptr must be encapsulated immediately in a heap object.
-}
\begin{code}
ppr_casm_results
	:: [CAddrMode]	-- list of results (length <= 1)
	-> SDoc  	-- liveness mask
	->
	( SDoc,		-- declaration of any local vars
	  [SDoc],	-- list of result vars (same length as results)
	  SDoc )	-- assignment (if any) of results in local var to registers

ppr_casm_results [] liveness
  = (empty, [], empty) 	-- no results

ppr_casm_results [r] liveness
  = let
	result_reg = ppr_amode r
	r_kind	   = getAmodeRep r

	local_var  = ptext SLIT("_ccall_result")

	(result_type, assign_result)
	  = case r_kind of
{- 
   @ForeignObj@s replaces MallocPtrs and are *not* CReturnable.
   Instead, external references have to explicitly turned into ForeignObjs
   using the primop makeForeignObj#. Benefit: Multiple finalisation
   routines can be accommodated and the below special case is not needed.
   Price is, of course, that you have to explicitly wrap `foreign objects'
   with makeForeignObj#.

	      ForeignObjRep ->
		(ptext SLIT("StgForeignObj"),
		 hcat [ ptext SLIT("constructForeignObj"),char '(',
				liveness, comma,
				result_reg, comma,
				local_var,
			     pp_paren_semi ]) 
-}
	      _ ->
		(pprPrimKind r_kind,
		 hcat [ result_reg, equals, local_var, semi ])

	declare_local_var = hcat [ result_type, space, local_var, semi ]
    in
    (declare_local_var, [local_var], assign_result)

ppr_casm_results rs liveness
  = panic "ppr_casm_results: ccall/casm with many results"
\end{code}


Note the sneaky way _the_ result is represented by a list so that we
can complain if it's used twice.

ToDo: Any chance of giving line numbers when process-casm fails?
      Or maybe we should do a check _much earlier_ in compiler. ADR

\begin{code}
process_casm ::
	[SDoc]		-- results (length <= 1)
	-> [SDoc]		-- arguments
	-> String		-- format string (with embedded %'s)
	->
	SDoc			-- code being generated

process_casm results args string = process results args string
 where
  process []    _ "" = empty
  process (_:_) _ "" = error ("process_casm: non-void result not assigned while processing _casm_ \"" ++ string ++ "\"\n(Try changing result type to PrimIO ()\n")

  process ress args ('%':cs)
    = case cs of
	[] ->
	    error ("process_casm: lonely % while processing _casm_ \"" ++ string ++ "\".\n")

	('%':css) ->
	    (<>) (char '%') (process ress args css)

	('r':css)  ->
	  case ress of
	    []  -> error ("process_casm: no result to match %r while processing _casm_ \"" ++ string ++ "\".\nTry deleting %r or changing result type from PrimIO ()\n")
	    [r] -> (<>) r (process [] args css)
	    _   -> panic ("process_casm: casm with many results while processing _casm_ \"" ++ string ++ "\".\n")

	other ->
	  let
		read_int :: ReadS Int
		read_int = reads
	  in
	  case (read_int other) of
	    [(num,css)] ->
		  if 0 <= num && num < length args
		  then (<>) (parens (args !! num))
				 (process ress args css)
		    else error ("process_casm: no such arg #:"++(show num)++" while processing \"" ++ string ++ "\".\n")
	    _ -> error ("process_casm: not %<num> while processing _casm_ \"" ++ string ++ "\".\n")

  process ress args (other_c:cs)
    = (<>) (char other_c) (process ress args cs)
\end{code}

%************************************************************************
%*									*
\subsection[a2r-assignments]{Assignments}
%*									*
%************************************************************************

Printing assignments is a little tricky because of type coercion.

First of all, the kind of the thing being assigned can be gotten from
the destination addressing mode.  (It should be the same as the kind
of the source addressing mode.)  If the kind of the assignment is of
@VoidRep@, then don't generate any code at all.

\begin{code}
pprAssign :: PrimRep -> CAddrMode -> CAddrMode -> SDoc

pprAssign VoidRep dest src = empty
\end{code}

Special treatment for floats and doubles, to avoid unwanted conversions.

\begin{code}
pprAssign FloatRep dest@(CVal reg_rel _) src
  = hcat [ ptext SLIT("ASSIGN_FLT"),char '(', ppr_amode (CAddr reg_rel), comma, pprAmode src, pp_paren_semi ]

pprAssign DoubleRep dest@(CVal reg_rel _) src
  = hcat [ ptext SLIT("ASSIGN_DBL"),char '(', ppr_amode (CAddr reg_rel), comma, pprAmode src, pp_paren_semi ]
\end{code}

Lastly, the question is: will the C compiler think the types of the
two sides of the assignment match?

	We assume that the types will match
	if neither side is a @CVal@ addressing mode for any register
	which can point into the heap or B stack.

Why?  Because the heap and B stack are used to store miscellaneous things,
whereas the A stack, temporaries, registers, etc., are only used for things
of fixed type.

\begin{code}
pprAssign kind (CReg (VanillaReg _ dest)) (CReg (VanillaReg _ src))
  = hcat [ pprVanillaReg dest, equals,
		pprVanillaReg src, semi ]

pprAssign kind dest src
  | mixedTypeLocn dest
    -- Add in a cast to StgWord (a.k.a. W_) iff the destination is mixed
  = hcat [ ppr_amode dest, equals,
		text "(W_)(",	-- Here is the cast
		ppr_amode src, pp_paren_semi ]

pprAssign kind dest src
  | mixedPtrLocn dest && getAmodeRep src /= PtrRep
    -- Add in a cast to StgPtr (a.k.a. P_) iff the destination is mixed
  = hcat [ ppr_amode dest, equals,
		text "(P_)(",	-- Here is the cast
		ppr_amode src, pp_paren_semi ]

pprAssign ByteArrayRep dest src
  | mixedPtrLocn src
    -- Add in a cast to StgPtr (a.k.a. B_) iff the source is mixed
  = hcat [ ppr_amode dest, equals,
		text "(B_)(",	-- Here is the cast
		ppr_amode src, pp_paren_semi ]

pprAssign kind other_dest src
  = hcat [ ppr_amode other_dest, equals,
		pprAmode  src, semi ]
\end{code}


%************************************************************************
%*									*
\subsection[a2r-CAddrModes]{Addressing modes}
%*									*
%************************************************************************

@pprAmode@ is used to print r-values (which may need casts), whereas
@ppr_amode@ is used for l-values {\em and} as a help function for
@pprAmode@.

\begin{code}
pprAmode, ppr_amode :: CAddrMode -> SDoc
\end{code}

For reasons discussed above under assignments, @CVal@ modes need
to be treated carefully.  First come special cases for floats and doubles,
similar to those in @pprAssign@:

(NB: @PK_FLT@ and @PK_DBL@ require the {\em address} of the value in
question.)

\begin{code}
pprAmode (CVal reg_rel FloatRep)
  = hcat [ text "PK_FLT(", ppr_amode (CAddr reg_rel), rparen ]
pprAmode (CVal reg_rel DoubleRep)
  = hcat [ text "PK_DBL(", ppr_amode (CAddr reg_rel), rparen ]
\end{code}

Next comes the case where there is some other cast need, and the
no-cast case:

\begin{code}
pprAmode amode
  | mixedTypeLocn amode
  = parens (hcat [ pprPrimKind (getAmodeRep amode), ptext SLIT(")("),
		ppr_amode amode ])
  | otherwise	-- No cast needed
  = ppr_amode amode
\end{code}

Now the rest of the cases for ``workhorse'' @ppr_amode@:

\begin{code}
ppr_amode (CVal reg_rel _)
  = case (pprRegRelative False{-no sign wanted-} reg_rel) of
	(pp_reg, Nothing)     -> (<>)  (char '*') pp_reg
	(pp_reg, Just offset) -> hcat [ pp_reg, brackets offset ]

ppr_amode (CAddr reg_rel)
  = case (pprRegRelative True{-sign wanted-} reg_rel) of
	(pp_reg, Nothing)     -> pp_reg
	(pp_reg, Just offset) -> (<>) pp_reg offset

ppr_amode (CReg magic_id) = pprMagicId magic_id

ppr_amode (CTemp uniq kind) = pprUnique uniq <> char '_'

ppr_amode (CLbl label kind) = pprCLabel label

ppr_amode (CUnVecLbl direct vectored)
  = hcat [char '(',ptext SLIT("StgRetAddr"),char ')', ptext SLIT("UNVEC"),char '(', pprCLabel direct, comma,
	       pprCLabel vectored, rparen]

ppr_amode (CCharLike ch)
  = hcat [ptext SLIT("CHARLIKE_CLOSURE"), char '(', pprAmode ch, rparen ]
ppr_amode (CIntLike int)
  = hcat [ptext SLIT("INTLIKE_CLOSURE"), char '(', pprAmode int, rparen ]

ppr_amode (CString str) = hcat [char '"', text (stringToC (_UNPK_ str)), char '"']
  -- ToDo: are these *used* for anything?

ppr_amode (CLit lit) = pprBasicLit lit

ppr_amode (CLitLit str _) = ptext str

ppr_amode (COffset off) = pprHeapOffset off

ppr_amode (CCode abs_C)
  = vcat [ ptext SLIT("{ -- CCode"), nest 8 (pprAbsC abs_C (costs abs_C)), char '}' ]

ppr_amode (CLabelledCode label abs_C)
  = vcat [ hcat [pprCLabel label, ptext SLIT(" = { -- CLabelledCode")],
	       nest 8 (pprAbsC abs_C (costs abs_C)), char '}' ]

ppr_amode (CJoinPoint _ _)
  = panic "ppr_amode: CJoinPoint"

ppr_amode (CTableEntry base index kind)
  = hcat [text "((", pprPrimKind kind, text " *)(",
	       ppr_amode base, text "))[(I_)(", ppr_amode index,
    	       ptext SLIT(")]")]

ppr_amode (CMacroExpr pk macro as)
  = hcat [lparen, pprPrimKind pk, text ")(", text (show macro), lparen,
	       hcat (punctuate comma (map pprAmode as)), text "))"]

ppr_amode (CCostCentre cc print_as_string)
  = uppCostCentre print_as_string cc
\end{code}

%************************************************************************
%*									*
\subsection[a2r-MagicIds]{Magic ids}
%*									*
%************************************************************************

@pprRegRelative@ returns a pair of the @Doc@ for the register
(some casting may be required), and a @Maybe Doc@ for the offset
(zero offset gives a @Nothing@).

\begin{code}
addPlusSign :: Bool -> SDoc -> SDoc
addPlusSign False p = p
addPlusSign True  p = (<>) (char '+') p

pprSignedInt :: Bool -> Int -> Maybe SDoc	-- Nothing => 0
pprSignedInt sign_wanted n
 = if n == 0 then Nothing else
   if n > 0  then Just (addPlusSign sign_wanted (int n))
   else 	  Just (int n)

pprRegRelative :: Bool		-- True <=> Print leading plus sign (if +ve)
	       -> RegRelative
	       -> (SDoc, Maybe SDoc)

pprRegRelative sign_wanted (SpARel spA off)
  = (pprMagicId SpA, pprSignedInt sign_wanted (spARelToInt spA off))

pprRegRelative sign_wanted (SpBRel spB off)
  = (pprMagicId SpB, pprSignedInt sign_wanted (spBRelToInt spB off))

pprRegRelative sign_wanted r@(HpRel hp off)
  = let to_print = hp `subOff` off
	pp_Hp	 = pprMagicId Hp
    in
    if isZeroOff to_print then
	(pp_Hp, Nothing)
    else
	(pp_Hp, Just ((<>) (char '-') (pprHeapOffset to_print)))
				-- No parens needed because pprHeapOffset
				-- does them when necessary

pprRegRelative sign_wanted (NodeRel off)
  = let pp_Node = pprMagicId node
    in
    if isZeroOff off then
	(pp_Node, Nothing)
    else
	(pp_Node, Just (addPlusSign sign_wanted (pprHeapOffset off)))

\end{code}

@pprMagicId@ just prints the register name.  @VanillaReg@ registers are
represented by a discriminated union (@StgUnion@), so we use the @PrimRep@
to select the union tag.

\begin{code}
pprMagicId :: MagicId -> SDoc

pprMagicId BaseReg	    	    = ptext SLIT("BaseReg")
pprMagicId StkOReg		    = ptext SLIT("StkOReg")
pprMagicId (VanillaReg pk n)
				    = hcat [ pprVanillaReg n, char '.',
						  pprUnionTag pk ]
pprMagicId (FloatReg  n)        = (<>) (ptext SLIT("FltReg")) (int IBOX(n))
pprMagicId (DoubleReg n)	    = (<>) (ptext SLIT("DblReg")) (int IBOX(n))
pprMagicId TagReg		    = ptext SLIT("TagReg")
pprMagicId RetReg		    = ptext SLIT("RetReg")
pprMagicId SpA		    = ptext SLIT("SpA")
pprMagicId SuA		    = ptext SLIT("SuA")
pprMagicId SpB		    = ptext SLIT("SpB")
pprMagicId SuB		    = ptext SLIT("SuB")
pprMagicId Hp		    = ptext SLIT("Hp")
pprMagicId HpLim		    = ptext SLIT("HpLim")
pprMagicId LivenessReg	    = ptext SLIT("LivenessReg")
pprMagicId StdUpdRetVecReg      = ptext SLIT("StdUpdRetVecReg")
pprMagicId StkStubReg	    = ptext SLIT("StkStubReg")
pprMagicId CurCostCentre	    = ptext SLIT("CCC")
pprMagicId VoidReg		    = panic "pprMagicId:VoidReg!"

pprVanillaReg :: FAST_INT -> SDoc

pprVanillaReg n = (<>) (char 'R') (int IBOX(n))

pprUnionTag :: PrimRep -> SDoc

pprUnionTag PtrRep		= char 'p'
pprUnionTag CodePtrRep	    	= ptext SLIT("fp")
pprUnionTag DataPtrRep	    	= char 'd'
pprUnionTag RetRep 	    	= char 'r'
pprUnionTag CostCentreRep	= panic "pprUnionTag:CostCentre?"

pprUnionTag CharRep		= char 'c'
pprUnionTag IntRep		= char 'i'
pprUnionTag WordRep		= char 'w'
pprUnionTag AddrRep		= char 'v'
pprUnionTag FloatRep		= char 'f'
pprUnionTag DoubleRep		= panic "pprUnionTag:Double?"

pprUnionTag StablePtrRep	= char 'i'
pprUnionTag ForeignObjRep	= char 'p'

pprUnionTag ArrayRep		= char 'p'
pprUnionTag ByteArrayRep	= char 'b'

pprUnionTag _                   = panic "pprUnionTag:Odd kind"
\end{code}


Find and print local and external declarations for a list of
Abstract~C statements.
\begin{code}
pprTempAndExternDecls :: AbstractC -> (SDoc{-temps-}, SDoc{-externs-})
pprTempAndExternDecls AbsCNop = (empty, empty)

pprTempAndExternDecls (AbsCStmts stmt1 stmt2)
  = initTE (ppr_decls_AbsC stmt1	`thenTE` \ (t_p1, e_p1) ->
	    ppr_decls_AbsC stmt2	`thenTE` \ (t_p2, e_p2) ->
	    case (catMaybes [t_p1, t_p2])	 of { real_temps ->
	    case (catMaybes [e_p1, e_p2])	 of { real_exts ->
	    returnTE (vcat real_temps, vcat real_exts) }}
	   )

pprTempAndExternDecls other_stmt
  = initTE (ppr_decls_AbsC other_stmt `thenTE` \ (maybe_t, maybe_e) ->
	    returnTE (
		case maybe_t of
		  Nothing -> empty
		  Just pp -> pp,

		case maybe_e of
		  Nothing -> empty
		  Just pp -> pp )
	   )

pprBasicLit :: Literal -> SDoc
pprPrimKind :: PrimRep -> SDoc

pprBasicLit  lit = ppr lit
pprPrimKind  k   = ppr k
\end{code}


%************************************************************************
%*									*
\subsection[a2r-monad]{Monadery}
%*									*
%************************************************************************

We need some monadery to keep track of temps and externs we have already
printed.  This info must be threaded right through the Abstract~C, so
it's most convenient to hide it in this monad.

WDP 95/02: Switched from \tr{([Unique], [CLabel])} to
\tr{(UniqSet, CLabelSet)}.  Allegedly for efficiency.

\begin{code}
type CLabelSet = FiniteMap CLabel (){-any type will do-}
emptyCLabelSet = emptyFM
x `elementOfCLabelSet` labs
  = case (lookupFM labs x) of { Just _ -> True; Nothing -> False }

addToCLabelSet set x = addToFM set x ()

type TEenv = (UniqSet Unique, CLabelSet)

type TeM result =  TEenv -> (TEenv, result)

initTE :: TeM a -> a
initTE sa
  = case sa (emptyUniqSet, emptyCLabelSet) of { (_, result) ->
    result }

{-# INLINE thenTE #-}
{-# INLINE returnTE #-}

thenTE :: TeM a -> (a -> TeM b) -> TeM b
thenTE a b u
  = case a u	    of { (u_1, result_of_a) ->
    b result_of_a u_1 }

mapTE :: (a -> TeM b) -> [a] -> TeM [b]
mapTE f []     = returnTE []
mapTE f (x:xs)
  = f x		`thenTE` \ r  ->
    mapTE f xs	`thenTE` \ rs ->
    returnTE (r : rs)

returnTE :: a -> TeM a
returnTE result env = (env, result)

-- these next two check whether the thing is already
-- recorded, and THEN THEY RECORD IT
-- (subsequent calls will return False for the same uniq/label)

tempSeenTE :: Unique -> TeM Bool
tempSeenTE uniq env@(seen_uniqs, seen_labels)
  = if (uniq `elementOfUniqSet` seen_uniqs)
    then (env, True)
    else ((addOneToUniqSet seen_uniqs uniq,
	  seen_labels),
	  False)

labelSeenTE :: CLabel -> TeM Bool
labelSeenTE label env@(seen_uniqs, seen_labels)
  = if (label `elementOfCLabelSet` seen_labels)
    then (env, True)
    else ((seen_uniqs,
	  addToCLabelSet seen_labels label),
	  False)
\end{code}

\begin{code}
pprTempDecl :: Unique -> PrimRep -> SDoc
pprTempDecl uniq kind
  = hcat [ pprPrimKind kind, space, pprUnique uniq, ptext SLIT("_;") ]

pprExternDecl :: CLabel -> PrimRep -> SDoc

pprExternDecl clabel kind
  = if not (needsCDecl clabel) then
	empty -- do not print anything for "known external" things (e.g., < PreludeCore)
    else
	case (
	    case kind of
	      CodePtrRep -> ppLocalnessMacro True{-function-} clabel
	      _		 -> ppLocalnessMacro False{-data-}    clabel
	) of { pp_macro_str ->

	hcat [ pp_macro_str, lparen, pprCLabel clabel, pp_paren_semi ]
	}
\end{code}

\begin{code}
ppr_decls_AbsC :: AbstractC -> TeM (Maybe SDoc{-temps-}, Maybe SDoc{-externs-})

ppr_decls_AbsC AbsCNop		= returnTE (Nothing, Nothing)

ppr_decls_AbsC (AbsCStmts stmts_1 stmts_2)
  = ppr_decls_AbsC stmts_1  `thenTE` \ p1 ->
    ppr_decls_AbsC stmts_2  `thenTE` \ p2 ->
    returnTE (maybe_vcat [p1, p2])

ppr_decls_AbsC (CClosureUpdInfo info)
  = ppr_decls_AbsC info

ppr_decls_AbsC (CSplitMarker) = returnTE (Nothing, Nothing)

ppr_decls_AbsC (CAssign dest source)
  = ppr_decls_Amode dest    `thenTE` \ p1 ->
    ppr_decls_Amode source  `thenTE` \ p2 ->
    returnTE (maybe_vcat [p1, p2])

ppr_decls_AbsC (CJump target) = ppr_decls_Amode target

ppr_decls_AbsC (CFallThrough target) = ppr_decls_Amode target

ppr_decls_AbsC (CReturn target _) = ppr_decls_Amode target

ppr_decls_AbsC (CSwitch discrim alts deflt)
  = ppr_decls_Amode discrim	`thenTE` \ pdisc ->
    mapTE ppr_alt_stuff alts	`thenTE` \ palts  ->
    ppr_decls_AbsC deflt	`thenTE` \ pdeflt ->
    returnTE (maybe_vcat (pdisc:pdeflt:palts))
  where
    ppr_alt_stuff (_, absC) = ppr_decls_AbsC absC

ppr_decls_AbsC (CCodeBlock label absC)
  = ppr_decls_AbsC absC

ppr_decls_AbsC (CInitHdr cl_info reg_rel cost_centre inplace_upd)
	-- ToDo: strictly speaking, should chk "cost_centre" amode
  = labelSeenTE info_lbl     `thenTE` \  label_seen ->
    returnTE (Nothing,
	      if label_seen then
		  Nothing
	      else
		  Just (pprExternDecl info_lbl PtrRep))
  where
    info_lbl = infoTableLabelFromCI cl_info

ppr_decls_AbsC (COpStmt	results	_ args _ _) = ppr_decls_Amodes (results ++ args)
ppr_decls_AbsC (CSimultaneous abc)  	    = ppr_decls_AbsC abc

ppr_decls_AbsC (CMacroStmt	    _ amodes)	= ppr_decls_Amodes amodes

ppr_decls_AbsC (CCallProfCtrMacro   _ amodes)	= ppr_decls_Amodes [] -- *****!!!
  -- you get some nasty re-decls of stdio.h if you compile
  -- the prelude while looking inside those amodes;
  -- no real reason to, anyway.
ppr_decls_AbsC (CCallProfCCMacro    _ amodes)	= ppr_decls_Amodes amodes

ppr_decls_AbsC (CStaticClosure closure_lbl closure_info cost_centre amodes)
	-- ToDo: strictly speaking, should chk "cost_centre" amode
  = ppr_decls_Amodes amodes

ppr_decls_AbsC (CClosureInfoAndCode cl_info slow maybe_fast upd_lbl _ _)
  = ppr_decls_Amodes [entry_lbl, upd_lbl]	`thenTE` \ p1 ->
    ppr_decls_AbsC slow				`thenTE` \ p2 ->
    (case maybe_fast of
	Nothing   -> returnTE (Nothing, Nothing)
	Just fast -> ppr_decls_AbsC fast)	`thenTE` \ p3 ->
    returnTE (maybe_vcat [p1, p2, p3])
  where
    entry_lbl = CLbl slow_lbl CodePtrRep
    slow_lbl    = case (nonemptyAbsC slow) of
		    Nothing -> mkErrorStdEntryLabel
		    Just _  -> entryLabelFromCI cl_info

ppr_decls_AbsC (CRetVector label maybe_amodes absC)
  = ppr_decls_Amodes (catMaybes maybe_amodes)	`thenTE` \ p1 ->
    ppr_decls_AbsC   absC			`thenTE` \ p2 ->
    returnTE (maybe_vcat [p1, p2])

ppr_decls_AbsC (CRetUnVector   _ amode)  = ppr_decls_Amode amode
ppr_decls_AbsC (CFlatRetVector _ amodes) = ppr_decls_Amodes amodes
\end{code}

\begin{code}
ppr_decls_Amode :: CAddrMode -> TeM (Maybe SDoc, Maybe SDoc)
ppr_decls_Amode (CVal _ _)	= returnTE (Nothing, Nothing)
ppr_decls_Amode (CAddr _)	= returnTE (Nothing, Nothing)
ppr_decls_Amode (CReg _)	= returnTE (Nothing, Nothing)
ppr_decls_Amode (CString _)	= returnTE (Nothing, Nothing)
ppr_decls_Amode (CLit _)	= returnTE (Nothing, Nothing)
ppr_decls_Amode (CLitLit _ _) 	= returnTE (Nothing, Nothing)
ppr_decls_Amode (COffset _)	= returnTE (Nothing, Nothing)

-- CIntLike must be a literal -- no decls
ppr_decls_Amode (CIntLike int)	= returnTE (Nothing, Nothing)

-- CCharLike may have be arbitrary value -- may have decls
ppr_decls_Amode (CCharLike char)
  = ppr_decls_Amode char

-- now, the only place where we actually print temps/externs...
ppr_decls_Amode (CTemp uniq kind)
  = case kind of
      VoidRep -> returnTE (Nothing, Nothing)
      other ->
	tempSeenTE uniq `thenTE` \ temp_seen ->
	returnTE
	  (if temp_seen then Nothing else Just (pprTempDecl uniq kind), Nothing)

ppr_decls_Amode (CLbl label VoidRep)
  = returnTE (Nothing, Nothing)

ppr_decls_Amode (CLbl label kind)
  = labelSeenTE label `thenTE` \ label_seen ->
    returnTE (Nothing,
	      if label_seen then Nothing else Just (pprExternDecl label kind))

{- WRONG:
ppr_decls_Amode (CUnVecLbl direct vectored)
  = labelSeenTE direct   `thenTE` \ dlbl_seen ->
    labelSeenTE vectored `thenTE` \ vlbl_seen ->
    let
	ddcl = if dlbl_seen then empty else pprExternDecl direct CodePtrRep
	vdcl = if vlbl_seen then empty else pprExternDecl vectored DataPtrRep
    in
    returnTE (Nothing,
    	    	if (dlbl_seen || not (needsCDecl direct)) &&
    	    	   (vlbl_seen || not (needsCDecl vectored)) then Nothing
    	        else Just (hcat [ptext SLIT("UNVEC"),char '(', ddcl, comma, vdcl, rparen]))
-}

ppr_decls_Amode (CUnVecLbl direct vectored)
  = -- We don't mark either label as "seen", because
    -- we don't know which one will be used and which one tossed
    -- by the C macro...
    --labelSeenTE direct   `thenTE` \ dlbl_seen ->
    --labelSeenTE vectored `thenTE` \ vlbl_seen ->
    let
	ddcl = {-if dlbl_seen then empty else-} pprExternDecl direct CodePtrRep
	vdcl = {-if vlbl_seen then empty else-} pprExternDecl vectored DataPtrRep
    in
    returnTE (Nothing,
    	    	if ({-dlbl_seen ||-} not (needsCDecl direct)) &&
    	    	   ({-vlbl_seen ||-} not (needsCDecl vectored)) then Nothing
    	        else Just (hcat [ptext SLIT("UNVEC"),char '(', ddcl, comma, vdcl, rparen]))

ppr_decls_Amode (CTableEntry base index _)
  = ppr_decls_Amode base    `thenTE` \ p1 ->
    ppr_decls_Amode index   `thenTE` \ p2 ->
    returnTE (maybe_vcat [p1, p2])

ppr_decls_Amode (CMacroExpr _ _ amodes)
  = ppr_decls_Amodes amodes

ppr_decls_Amode other = returnTE (Nothing, Nothing)


maybe_vcat :: [(Maybe SDoc, Maybe SDoc)] -> (Maybe SDoc, Maybe SDoc)
maybe_vcat ps
  = case (unzip ps)	of { (ts, es) ->
    case (catMaybes ts)	of { real_ts  ->
    case (catMaybes es)	of { real_es  ->
    (if (null real_ts) then Nothing else Just (vcat real_ts),
     if (null real_es) then Nothing else Just (vcat real_es))
    } } }
\end{code}

\begin{code}
ppr_decls_Amodes :: [CAddrMode] -> TeM (Maybe SDoc, Maybe SDoc)
ppr_decls_Amodes amodes
  = mapTE ppr_decls_Amode amodes `thenTE` \ ps ->
    returnTE ( maybe_vcat ps )
\end{code}
