%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[GHC_Main]{Main driver for Glasgow Haskell compiler}

\begin{code}
module Main ( main ) where

#include "HsVersions.h"

import IO	( IOMode(..),
		  hGetContents, hPutStr, hClose, openFile,
		  stdin,stderr
		)
import HsSyn
import RdrHsSyn		( RdrName )
import BasicTypes	( NewOrData(..) )

import ReadPrefix	( rdModule )
import Rename		( renameModule )
import RnMonad		( ExportEnv )

import MkIface		-- several functions
import TcModule		( typecheckModule )
import Desugar		( deSugar, pprDsWarnings )
import SimplCore	( core2core )
import CoreToStg	( topCoreBindsToStg )
import StgSyn		( collectFinalStgBinders, pprStgBindings )
import SimplStg		( stg2stg )
import CodeGen		( codeGen )
#if ! OMIT_NATIVE_CODEGEN
import AsmCodeGen	( dumpRealAsm, writeRealAsm )
#endif

import AbsCSyn		( absCNop, AbstractC )
import AbsCUtils	( flattenAbsC )
import CoreUnfold	( Unfolding )
import Bag		( emptyBag, isEmptyBag )
import CmdLineOpts
import ErrUtils		( pprBagOfErrors, ghcExit, doIfSet, dumpIfSet )
import Maybes		( maybeToBool, MaybeErr(..) )
import StgSyn		( GenStgBinding )
import TcInstUtil	( InstInfo )
import TyCon		( isDataTyCon )
import Class		( classTyCon )
import UniqSupply	( mkSplitUniqSupply )

import PprAbsC		( dumpRealC, writeRealC )
import PprCore		( pprCoreBinding )
import FiniteMap	( emptyFM )
import Outputable
\end{code}

\begin{code}
main =
 _scc_ "main" 
 let
    cmd_line_info = classifyOpts
 in
 doIt cmd_line_info
\end{code}

\begin{code}
doIt :: ([CoreToDo], [StgToDo]) -> IO ()

doIt (core_cmds, stg_cmds)
  = doIfSet opt_Verbose 
	(hPutStr stderr ("Glasgow Haskell Compiler, version\ 
			 \ PROJECTVERSION\ 
			 \, for Haskell 1.4\n"))		>>

    -- ******* READER
    show_pass "Reader"	>>
    _scc_     "Reader"
    rdModule		>>= \ (mod_name, rdr_module) ->

    dumpIfSet opt_D_dump_rdr "Reader" (ppr rdr_module)		>>

    dumpIfSet opt_D_source_stats "Source Statistics"
	(ppSourceStats rdr_module)	 	>>

    -- UniqueSupplies for later use (these are the only lower case uniques)
--    _scc_     "spl-rn"
    mkSplitUniqSupply 'r'	>>= \ rn_uniqs 	-> -- renamer
--    _scc_     "spl-tc"
    mkSplitUniqSupply 'a'	>>= \ tc_uniqs 	-> -- typechecker
--    _scc_     "spl-ds"
    mkSplitUniqSupply 'd'	>>= \ ds_uniqs 	-> -- desugarer
--    _scc_     "spl-sm"
    mkSplitUniqSupply 's'	>>= \ sm_uniqs 	-> -- core-to-core simplifier
--    _scc_     "spl-c2s"
    mkSplitUniqSupply 'c'	>>= \ c2s_uniqs -> -- core-to-stg
--    _scc_     "spl-st"
    mkSplitUniqSupply 'g'	>>= \ st_uniqs  -> -- stg-to-stg passes
--    _scc_     "spl-absc"
    mkSplitUniqSupply 'f'	>>= \ fl_uniqs  -> -- absC flattener
--    _scc_     "spl-ncg"
    mkSplitUniqSupply 'n'	>>= \ ncg_uniqs -> -- native-code generator

    -- ******* RENAMER
    show_pass "Renamer" 			>>
    _scc_     "Renamer"

    renameModule rn_uniqs rdr_module		>>=
	\ maybe_rn_stuff ->
    case maybe_rn_stuff of {
	Nothing -> 	-- Hurrah!  Renamer reckons that there's no need to
			-- go any further
			return ();
	
	Just (rn_mod, iface_file_stuff, rn_name_supply, imported_modules) ->
			-- Oh well, we've got to recompile for real


    -- Safely past renaming: we can start the interface file:
    -- (the iface file is produced incrementally, as we have
    -- the information that we need...; we use "iface<blah>")
    -- "endIface" finishes the job.
    startIface mod_name					>>= \ if_handle ->
    ifaceMain if_handle iface_file_stuff		>>


    -- ******* TYPECHECKER
    show_pass "TypeCheck" 				>>
    _scc_     "TypeCheck"
    typecheckModule tc_uniqs rn_name_supply rn_mod	>>= \ maybe_tc_stuff ->
    case maybe_tc_stuff of {
	Nothing -> ghcExit 1;	-- Type checker failed

	Just (all_binds,
	      local_tycons, local_classes, inst_info, 
	      ddump_deriv) ->


    -- ******* DESUGARER
    show_pass "DeSugar" 			>>
    _scc_     "DeSugar"
    deSugar ds_uniqs mod_name all_binds		>>= \ desugared ->


    -- ******* CORE-TO-CORE SIMPLIFICATION
    show_pass "Core2Core" 			>>
    _scc_     "Core2Core"
    let
	local_data_tycons = filter isDataTyCon local_tycons
    in
    core2core core_cmds mod_name
	      sm_uniqs local_data_tycons desugared
						>>=
	 \ simplified ->


    -- ******* STG-TO-STG SIMPLIFICATION
    show_pass "Core2Stg" 			>>
    _scc_     "Core2Stg"
    let
	stg_binds   = topCoreBindsToStg c2s_uniqs simplified
    in

    show_pass "Stg2Stg" 			>>
    _scc_     "Stg2Stg"
    stg2stg stg_cmds mod_name st_uniqs stg_binds
						>>=
	\ (stg_binds2, cost_centre_info) ->

    dumpIfSet opt_D_dump_stg "STG syntax:" (pprStgBindings stg_binds2)	>>

	-- Dump instance decls and type signatures into the interface file
    let
	final_ids = collectFinalStgBinders stg_binds2
    in
    _scc_     "Interface"
    ifaceDecls if_handle local_tycons local_classes inst_info final_ids simplified	>>
    endIface if_handle						>>
    -- We are definitely done w/ interface-file stuff at this point:
    -- (See comments near call to "startIface".)
    

    -- ******* "ABSTRACT", THEN "FLAT", THEN *REAL* C!
    show_pass "CodeGen" 			>>
    _scc_     "CodeGen"
    let
	all_local_data_tycons = filter isDataTyCon (map classTyCon local_classes)
				++ local_data_tycons
					-- Generate info tables  for the data constrs arising
					-- from class decls as well

	all_tycon_specs       = emptyFM	-- Not specialising tycons any more

	abstractC      = codeGen mod_name		-- module name for CC labelling
				 cost_centre_info
				 imported_modules	-- import names for CC registering
				 all_local_data_tycons	-- type constructors generated locally
				 all_tycon_specs	-- tycon specialisations
				 stg_binds2

    	flat_abstractC = flattenAbsC fl_uniqs abstractC
    in
    dumpIfSet opt_D_dump_absC "Abstract C"
	(dumpRealC abstractC)		  	>>

    dumpIfSet opt_D_dump_flatC "Flat Abstract C"
	(dumpRealC flat_abstractC)		>>

    show_pass "CodeOutput" 			>>
    _scc_     "CodeOutput"
    -- You can have C (c_output) or assembly-language (ncg_output),
    -- but not both.  [Allowing for both gives a space leak on
    -- flat_abstractC.  WDP 94/10]
    let
	(flat_absC_c, flat_absC_ncg) =
	   case (maybeToBool opt_ProduceC || opt_D_dump_realC,
		 maybeToBool opt_ProduceS || opt_D_dump_asm) of
	     (True,  False) -> (flat_abstractC, absCNop)
	     (False, True)  -> (absCNop, flat_abstractC)
	     (False, False) -> (absCNop, absCNop)
	     (True,  True)  -> error "ERROR: Can't do both .hc and .s at the same time"

	c_output_d = dumpRealC flat_absC_c
	c_output_w = (\ f -> writeRealC f flat_absC_c)

#if OMIT_NATIVE_CODEGEN
	ncg_output_d = error "*** GHC not built with a native-code generator ***"
	ncg_output_w = ncg_output_d
#else
	ncg_output_d = dumpRealAsm flat_absC_ncg ncg_uniqs
	ncg_output_w = (\ f -> writeRealAsm f flat_absC_ncg ncg_uniqs)
#endif
    in

    dumpIfSet opt_D_dump_asm "Asm code" ncg_output_d 	>>
    doOutput opt_ProduceS ncg_output_w 			>>

    dumpIfSet opt_D_dump_realC "Real C" c_output_d 	>>
    doOutput opt_ProduceC c_output_w 			>>

    ghcExit 0
    } }
  where
    -------------------------------------------------------------
    -- ****** help functions:

    show_pass
      = if opt_D_show_passes
	then \ what -> hPutStr stderr ("*** "++what++":\n")
	else \ what -> return ()

    doOutput switch io_action
      = case switch of
	  Nothing -> return ()
	  Just fname ->
	    openFile fname WriteMode	>>= \ handle ->
	    io_action handle		>>
	    hClose handle


ppSourceStats (HsModule name version exports imports fixities decls src_loc)
 = vcat (map pp_val
	       [("ExportAll        ", export_all), -- 1 if no export list
		("ExportDecls      ", export_ds),
		("ExportModules    ", export_ms),
		("Imports          ", import_no),
		("  ImpQual        ", import_qual),
		("  ImpAs          ", import_as),
		("  ImpAll         ", import_all),
		("  ImpPartial     ", import_partial),
		("  ImpHiding      ", import_hiding),
		("FixityDecls      ", fixity_ds),
		("DefaultDecls     ", default_ds),
	      	("TypeDecls        ", type_ds),
	      	("DataDecls        ", data_ds),
	      	("NewTypeDecls     ", newt_ds),
	      	("DataConstrs      ", data_constrs),
		("DataDerivings    ", data_derivs),
	      	("ClassDecls       ", class_ds),
	      	("ClassMethods     ", class_method_ds),
	      	("DefaultMethods   ", default_method_ds),
	      	("InstDecls        ", inst_ds),
	      	("InstMethods      ", inst_method_ds),
	      	("TypeSigs         ", bind_tys),
	      	("ValBinds         ", val_bind_ds),
	      	("FunBinds         ", fn_bind_ds),
	      	("InlineMeths      ", method_inlines),
		("InlineBinds      ", bind_inlines),
--	      	("SpecialisedData  ", data_specs),
--	      	("SpecialisedInsts ", inst_specs),
	      	("SpecialisedMeths ", method_specs),
	      	("SpecialisedBinds ", bind_specs)
	       ])
  where
    pp_val (str, 0) = empty
    pp_val (str, n) = hcat [text str, int n]

    fixity_ds   = length fixities
    type_decls 	= [d | TyD d@(TySynonym _ _ _ _)    <- decls]
    data_decls 	= [d | TyD d@(TyData DataType _ _ _ _ _ _ _) <- decls]
    newt_decls 	= [d | TyD d@(TyData NewType  _ _ _ _ _ _ _) <- decls]
    type_ds	= length type_decls
    data_ds	= length data_decls
    newt_ds	= length newt_decls
    class_decls = [d | ClD d <- decls]
    class_ds    = length class_decls
    inst_decls  = [d | InstD d <- decls]
    inst_ds     = length inst_decls
    default_ds  = length [() | DefD _ <- decls]
    val_decls   = [d | ValD d <- decls]

    real_exports = case exports of { Nothing -> []; Just es -> es }
    n_exports  	 = length real_exports
    export_ms  	 = length [() | IEModuleContents _ <- real_exports]
    export_ds  	 = n_exports - export_ms
    export_all 	 = case exports of { Nothing -> 1; other -> 0 }

    (val_bind_ds, fn_bind_ds, bind_tys, bind_specs, bind_inlines)
	= count_binds (foldr ThenBinds EmptyBinds val_decls)

    (import_no, import_qual, import_as, import_all, import_partial, import_hiding)
	= foldr add6 (0,0,0,0,0,0) (map import_info imports)
    (data_constrs, data_derivs)
	= foldr add2 (0,0) (map data_info (newt_decls ++ data_decls))
    (class_method_ds, default_method_ds)
	= foldr add2 (0,0) (map class_info class_decls)
    (inst_method_ds, method_specs, method_inlines)
	= foldr add3 (0,0,0) (map inst_info inst_decls)


    count_binds EmptyBinds        = (0,0,0,0,0)
    count_binds (ThenBinds b1 b2) = count_binds b1 `add5` count_binds b2
    count_binds (MonoBind b sigs _) = case (count_monobinds b, count_sigs sigs) of
				        ((vs,fs),(ts,_,ss,is)) -> (vs,fs,ts,ss,is)

    count_monobinds EmptyMonoBinds	  = (0,0)
    count_monobinds (AndMonoBinds b1 b2)  = count_monobinds b1 `add2` count_monobinds b2
    count_monobinds (PatMonoBind (VarPatIn n) r _) = (1,0)
    count_monobinds (PatMonoBind p r _)   = (0,1)
    count_monobinds (FunMonoBind f _ m _) = (0,1)

    count_sigs sigs = foldr add4 (0,0,0,0) (map sig_info sigs)

    sig_info (Sig _ _ _)          = (1,0,0,0)
    sig_info (ClassOpSig _ _ _ _) = (0,1,0,0)
    sig_info (SpecSig _ _ _ _)    = (0,0,1,0)
    sig_info (InlineSig _ _)      = (0,0,0,1)
    sig_info _                    = (0,0,0,0)

    import_info (ImportDecl _ qual _ as spec _)
	= add6 (1, qual_info qual, as_info as, 0,0,0) (spec_info spec)
    qual_info False  = 0
    qual_info True   = 1
    as_info Nothing  = 0
    as_info (Just _) = 1
    spec_info Nothing 	        = (0,0,0,1,0,0)
    spec_info (Just (False, _)) = (0,0,0,0,1,0)
    spec_info (Just (True, _))  = (0,0,0,0,0,1)

    data_info (TyData _ _ _ _ constrs derivs _ _)
	= (length constrs, case derivs of {Nothing -> 0; Just ds -> length ds})

    class_info (ClassDecl _ _ _ meth_sigs def_meths _ _ _ _)
	= case count_sigs meth_sigs of
	    (_,classops,_,_) ->
	       (classops, addpr (count_monobinds def_meths))

    inst_info (InstDecl _ inst_meths inst_sigs _ _)
	= case count_sigs inst_sigs of
	    (_,_,ss,is) ->
	       (addpr (count_monobinds inst_meths), ss, is)

    addpr (x,y) = x+y
    add1 x1 y1  = x1+y1
    add2 (x1,x2) (y1,y2) = (x1+y1,x2+y2)
    add3 (x1,x2,x3) (y1,y2,y3) = (x1+y1,x2+y2,x3+y3)
    add4 (x1,x2,x3,x4) (y1,y2,y3,y4) = (x1+y1,x2+y2,x3+y3,x4+y4)
    add5 (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) = (x1+y1,x2+y2,x3+y3,x4+y4,x5+y5)
    add6 (x1,x2,x3,x4,x5,x6) (y1,y2,y3,y4,y5,y6) = (x1+y1,x2+y2,x3+y3,x4+y4,x5+y5,x6+y6)
\end{code}
