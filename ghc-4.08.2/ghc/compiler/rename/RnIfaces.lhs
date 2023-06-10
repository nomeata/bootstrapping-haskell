%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnIfaces]{Cacheing and Renaming of Interfaces}

\begin{code}
module RnIfaces (
	getInterfaceExports, 
	getImportedInstDecls, getImportedRules,
	lookupFixity, loadHomeInterface,
	importDecl, recordSlurp,
	getImportVersions, getSlurped,

	checkUpToDate,

	getDeclBinders, getDeclSysBinders,
	removeContext	 	-- removeContext probably belongs somewhere else
    ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_NoPruneDecls, opt_IgnoreIfacePragmas )
import HsSyn		( HsDecl(..), TyClDecl(..), InstDecl(..), IfaceSig(..), 
			  HsType(..), ConDecl(..), IE(..), ConDetails(..), Sig(..),
			  ForeignDecl(..), ForKind(..), isDynamicExtName,
			  FixitySig(..), RuleDecl(..),
			  isClassOpSig, Deprecation(..)
			)
import BasicTypes	( Version, NewOrData(..), defaultFixity )
import RdrHsSyn		( RdrNameHsDecl, RdrNameInstDecl, RdrNameTyClDecl, RdrNameRuleDecl,
			  extractHsTyRdrNames, RdrNameDeprecation
			)
import RnEnv		( mkImportedGlobalName, newImportedBinder, mkImportedGlobalFromRdrName,
			  lookupOccRn, lookupImplicitOccRn,
			  pprAvail,
			  availName, availNames, addAvailToNameSet, addSysAvails,
			  FreeVars, emptyFVs
			)
import RnMonad
import RnHsSyn          ( RenamedHsDecl, RenamedDeprecation )
import ParseIface	( parseIface, IfaceStuff(..) )

import FiniteMap	( FiniteMap, sizeFM, emptyFM, delFromFM, listToFM,
			  lookupFM, addToFM, addToFM_C, addListToFM, 
			  fmToList, elemFM, foldFM
			)
import Name		( Name {-instance NamedThing-},
			  nameModule, isLocallyDefined,
			  isWiredInName, nameUnique, NamedThing(..)
			 )
import Module		( Module, moduleString, pprModule,
			  mkVanillaModule, pprModuleName,
			  moduleUserString, moduleName, isLocalModule,
			  ModuleName, WhereFrom(..),
			)
import RdrName		( RdrName, rdrNameOcc )
import NameSet
import Var		( Id )
import SrcLoc		( mkSrcLoc, SrcLoc )
import PrelMods		( pREL_GHC )
import PrelInfo		( cCallishTyKeys )
import Bag
import Maybes		( MaybeErr(..), maybeToBool, orElse )
import ListSetOps	( unionLists )
import Outputable
import Unique		( Unique )
import StringBuffer     ( StringBuffer, hGetStringBuffer )
import FastString	( mkFastString )
import ErrUtils         ( Message )
import Lex
import Outputable

import IO	( isDoesNotExistError )
import List	( nub )
\end{code}


%*********************************************************
%*							*
\subsection{Loading a new interface file}
%*							*
%*********************************************************

\begin{code}
loadHomeInterface :: SDoc -> Name -> RnM d Ifaces
loadHomeInterface doc_str name
  = loadInterface doc_str (moduleName (nameModule name)) ImportBySystem

loadOrphanModules :: [ModuleName] -> RnM d ()
loadOrphanModules mods
  | null mods = returnRn ()
  | otherwise = traceRn (text "Loading orphan modules:" <+> 
			 fsep (map pprModuleName mods))		`thenRn_` 
		mapRn_ load mods				`thenRn_`
		returnRn ()
  where
    load mod   = loadInterface (mk_doc mod) mod ImportBySystem
    mk_doc mod = pprModuleName mod <+> ptext SLIT("is a orphan-instance module")
	   

loadInterface :: SDoc -> ModuleName -> WhereFrom -> RnM d Ifaces
loadInterface doc mod from 
  = tryLoadInterface doc mod from	`thenRn` \ (ifaces, maybe_err) ->
    case maybe_err of
	Nothing  -> returnRn ifaces
	Just err -> failWithRn ifaces err

tryLoadInterface :: SDoc -> ModuleName -> WhereFrom -> RnM d (Ifaces, Maybe Message)
	-- Returns (Just err) if an error happened
	-- Guarantees to return with iImpModInfo m --> (... Just cts)
	-- (If the load fails, we plug in a vanilla placeholder
tryLoadInterface doc_str mod_name from
 = getIfacesRn 			`thenRn` \ ifaces ->
   let
	mod_map  = iImpModInfo ifaces
	mod_info = lookupFM mod_map mod_name

	hi_boot_file = case from of {
		     	 ImportByUser       -> False ;		-- Not hi-boot
		     	 ImportByUserSource -> True ;		-- hi-boot
			 ImportBySystem     -> 
		       case mod_info of
			 Just (_, _, is_boot, _) -> is_boot

			 Nothing -> False
				-- We're importing a module we know absolutely
				-- nothing about, so we assume it's from
				-- another package, where we aren't doing 
				-- dependency tracking. So it won't be a hi-boot file.
		       }
	redundant_source_import 
	  = case (from, mod_info) of 
		(ImportByUserSource, Just (_,_,False,_)) -> True
		other					 -> False
   in
	-- CHECK WHETHER WE HAVE IT ALREADY
   case mod_info of {
	Just (_, _, _, Just _)
		-> 	-- We're read it already so don't re-read it
		    returnRn (ifaces, Nothing) ;

	_ ->

	-- Issue a warning for a redundant {- SOURCE -} import
	-- NB that we arrange to read all the ordinary imports before 
	-- any of the {- SOURCE -} imports
   warnCheckRn	(not redundant_source_import)
		(warnRedundantSourceImport mod_name)	`thenRn_`

	-- READ THE MODULE IN
   findAndReadIface doc_str mod_name hi_boot_file   `thenRn` \ read_result ->
   case read_result of {
	Left err -> 	-- Not found, so add an empty export env to the Ifaces map
			-- so that we don't look again
	   let
		mod         = mkVanillaModule mod_name
		new_mod_map = addToFM mod_map mod_name (0, False, False, Just (mod, from, []))
		new_ifaces  = ifaces { iImpModInfo = new_mod_map }
	   in
	   setIfacesRn new_ifaces		`thenRn_`
	   returnRn (new_ifaces, Just err) ;

	-- Found and parsed!
	Right iface ->

	-- LOAD IT INTO Ifaces

	-- NB: *first* we do loadDecl, so that the provenance of all the locally-defined
	---    names is done correctly (notably, whether this is an .hi file or .hi-boot file).
	--     If we do loadExport first the wrong info gets into the cache (unless we
	-- 	explicitly tag each export which seems a bit of a bore)

    getModuleRn 		`thenRn` \ this_mod_nm ->
    let
	rd_decls = pi_decls iface
	mod	 = pi_mod   iface
    in
	-- Sanity check.  If we're system-importing a module we know nothing at all
	-- about, it should be from a different package to this one
    WARN( not (maybeToBool mod_info) && 
	  case from of { ImportBySystem -> True; other -> False } &&
	  isLocalModule mod,
	  ppr mod )
    foldlRn (loadDecl mod)	      (iDecls ifaces) rd_decls 			`thenRn` \ new_decls ->
    foldlRn (loadInstDecl mod)	      (iInsts ifaces) (pi_insts iface)		`thenRn` \ new_insts ->
    (if opt_IgnoreIfacePragmas
	then returnRn emptyBag
	else foldlRn (loadRule mod)   (iRules ifaces) (pi_rules iface))		`thenRn` \ new_rules ->
    (if opt_IgnoreIfacePragmas
	then returnRn emptyNameEnv
	else foldlRn (loadDeprec mod) (iDeprecs ifaces) (pi_deprecs iface))	`thenRn` \ new_deprecs ->
    foldlRn (loadFixDecl mod_name)    (iFixes ifaces) rd_decls  		`thenRn` \ new_fixities ->
    mapRn   (loadExport this_mod_nm)  (pi_exports iface)			`thenRn` \ avails_s ->
    let
	-- For an explicit user import, add to mod_map info about
	-- the things the imported module depends on, extracted
	-- from its usage info.
	mod_map1 = case from of
			ImportByUser -> addModDeps mod (pi_usages iface) mod_map
			other        -> mod_map

	-- Now add info about this module
	mod_map2    = addToFM mod_map1 mod_name mod_details
	cts	    = (pi_mod iface, from, concat avails_s)
	mod_details = (pi_vers iface, pi_orphan iface, hi_boot_file, Just cts)

	new_ifaces = ifaces { iImpModInfo = mod_map2,
			      iDecls      = new_decls,
			      iFixes      = new_fixities,
			      iInsts      = new_insts,
			      iRules	  = new_rules,
			      iDeprecs	  = new_deprecs }
    in
    setIfacesRn new_ifaces		`thenRn_`
    returnRn (new_ifaces, Nothing)
    }}

addModDeps :: Module -> [ImportVersion a] 
	   -> ImportedModuleInfo -> ImportedModuleInfo
-- (addModDeps M ivs deps)
-- We are importing module M, and M.hi contains 'import' decls given by ivs
addModDeps mod new_deps mod_deps
  = foldr add mod_deps filtered_new_deps
  where
	-- Don't record dependencies when importing a module from another package
	-- Except for its descendents which contain orphans,
	-- and in that case, forget about the boot indicator
    filtered_new_deps
	| isLocalModule mod = [ (imp_mod, (version, has_orphans, is_boot, Nothing))
			      | (imp_mod, version, has_orphans, is_boot, _) <- new_deps 
			      ]			      
	| otherwise	    = [ (imp_mod, (version, True, False, Nothing))
			      | (imp_mod, version, has_orphans, _, _) <- new_deps, 
				has_orphans
			      ]
    add (imp_mod, dep) deps = addToFM_C combine deps imp_mod dep

    combine old@(_, _, old_is_boot, cts) new
	| maybeToBool cts || not old_is_boot = old	-- Keep the old info if it's already loaded
							-- or if it's a non-boot pending load
	| otherwise			     = new	-- Otherwise pick new info

loadExport :: ModuleName -> ExportItem -> RnM d [AvailInfo]
loadExport this_mod (mod, entities)
  | mod == this_mod = returnRn []
	-- If the module exports anything defined in this module, just ignore it.
	-- Reason: otherwise it looks as if there are two local definition sites
	-- for the thing, and an error gets reported.  Easiest thing is just to
	-- filter them out up front. This situation only arises if a module
	-- imports itself, or another module that imported it.  (Necessarily,
	-- this invoves a loop.)  Consequence: if you say
	--	module A where
	--	   import B( AType )
	--	   type AType = ...
	--
	--	module B( AType ) where
	--	   import {-# SOURCE #-} A( AType )
	--
	-- then you'll get a 'B does not export AType' message.  A bit bogus
	-- but it's a bogus thing to do!

  | otherwise
  = mapRn (load_entity mod) entities
  where
    new_name mod occ = mkImportedGlobalName mod occ

    load_entity mod (Avail occ)
      =	new_name mod occ	`thenRn` \ name ->
	returnRn (Avail name)
    load_entity mod (AvailTC occ occs)
      =	new_name mod occ	      `thenRn` \ name ->
        mapRn (new_name mod) occs     `thenRn` \ names ->
        returnRn (AvailTC name names)


loadFixDecl :: ModuleName -> FixityEnv
	    -> (Version, RdrNameHsDecl)
	    -> RnM d FixityEnv
loadFixDecl mod_name fixity_env (version, FixD sig@(FixitySig rdr_name fixity loc))
  = 	-- Ignore the version; when the fixity changes the version of
	-- its 'host' entity changes, so we don't need a separate version
	-- number for fixities
    mkImportedGlobalName mod_name (rdrNameOcc rdr_name) 	`thenRn` \ name ->
    let
	new_fixity_env = addToNameEnv fixity_env name (FixitySig name fixity loc)
    in
    returnRn new_fixity_env

	-- Ignore the other sorts of decl
loadFixDecl mod_name fixity_env other_decl = returnRn fixity_env

loadDecl :: Module 
	 -> DeclsMap
	 -> (Version, RdrNameHsDecl)
	 -> RnM d DeclsMap

loadDecl mod decls_map (version, decl)
  = getDeclBinders new_name decl	`thenRn` \ maybe_avail ->
    case maybe_avail of {
	Nothing -> returnRn decls_map;	-- No bindings
	Just avail ->

    getDeclSysBinders new_name decl	`thenRn` \ sys_bndrs ->
    let
	full_avail    = addSysAvails avail sys_bndrs
		-- Add the sys-binders to avail.  When we import the decl,
		-- it's full_avail that will get added to the 'already-slurped' set (iSlurp)
		-- If we miss out sys-binders, we'll read the decl multiple times!

	main_name     = availName avail
	new_decls_map = foldl add_decl decls_map
				       [ (name, (version, full_avail, name==main_name, (mod, decl'))) 
				       | name <- availNames full_avail]
	add_decl decls_map (name, stuff)
	  = WARN( name `elemNameEnv` decls_map, ppr name )
	    addToNameEnv decls_map name stuff
    in
    returnRn new_decls_map
    }
  where
	-- newImportedBinder puts into the cache the binder with the
	-- module information set correctly.  When the decl is later renamed,
	-- the binding site will thereby get the correct module.
    new_name rdr_name loc = newImportedBinder mod rdr_name

    {-
      If a signature decl is being loaded, and optIgnoreIfacePragmas is on,
      we toss away unfolding information.

      Also, if the signature is loaded from a module we're importing from source,
      we do the same. This is to avoid situations when compiling a pair of mutually
      recursive modules, peering at unfolding info in the interface file of the other, 
      e.g., you compile A, it looks at B's interface file and may as a result change
      its interface file. Hence, B is recompiled, maybe changing its interface file,
      which will the unfolding info used in A to become invalid. Simple way out is to
      just ignore unfolding info.

      [Jan 99: I junked the second test above.  If we're importing from an hi-boot
       file there isn't going to *be* any pragma info.  Maybe the above comment
       dates from a time where we picked up a .hi file first if it existed?]
    -}
    decl' = case decl of
	       SigD (IfaceSig name tp ls loc) | opt_IgnoreIfacePragmas
			 ->  SigD (IfaceSig name tp [] loc)
	       other	 -> decl

loadInstDecl :: Module
	     -> Bag GatedDecl
	     -> RdrNameInstDecl
	     -> RnM d (Bag GatedDecl)
loadInstDecl mod insts decl@(InstDecl inst_ty binds uprags dfun_name src_loc)
  = 
	-- Find out what type constructors and classes are "gates" for the
	-- instance declaration.  If all these "gates" are slurped in then
	-- we should slurp the instance decl too.
	-- 
	-- We *don't* want to count names in the context part as gates, though.
	-- For example:
	--		instance Foo a => Baz (T a) where ...
	--
	-- Here the gates are Baz and T, but *not* Foo.
    let 
	munged_inst_ty = removeContext inst_ty
	free_names     = extractHsTyRdrNames munged_inst_ty
    in
    setModuleRn (moduleName mod) $
    mapRn mkImportedGlobalFromRdrName free_names	`thenRn` \ gate_names ->
    returnRn ((mkNameSet gate_names, (mod, InstD decl)) `consBag` insts)


-- In interface files, the instance decls now look like
--	forall a. Foo a -> Baz (T a)
-- so we have to strip off function argument types as well
-- as the bit before the '=>' (which is always empty in interface files)
removeContext (HsForAllTy tvs cxt ty) = HsForAllTy tvs [] (removeFuns ty)
removeContext ty		      = removeFuns ty

removeFuns (MonoFunTy _ ty) = removeFuns ty
removeFuns ty		    = ty


loadRule :: Module -> Bag GatedDecl 
	 -> RdrNameRuleDecl -> RnM d (Bag GatedDecl)
-- "Gate" the rule simply by whether the rule variable is
-- needed.  We can refine this later.
loadRule mod rules decl@(IfaceRuleDecl var body src_loc)
  = setModuleRn (moduleName mod) $
    mkImportedGlobalFromRdrName var		`thenRn` \ var_name ->
    returnRn ((unitNameSet var_name, (mod, RuleD decl)) `consBag` rules)

-- SUP: TEMPORARY HACK, ignoring module deprecations for now
loadDeprec :: Module -> DeprecationEnv -> RdrNameDeprecation -> RnM d DeprecationEnv
loadDeprec mod deprec_env (Deprecation (IEModuleContents _) txt)
  = traceRn (text "module deprecation not yet implemented:" <+> ppr mod <> colon <+> ppr txt) `thenRn_`
    returnRn deprec_env
loadDeprec mod deprec_env (Deprecation ie txt)
  = setModuleRn (moduleName mod) $
    mapRn mkImportedGlobalFromRdrName (namesFromIE ie) `thenRn` \ names ->
    traceRn (text "loaded deprecation(s) for" <+> hcat (punctuate comma (map ppr names)) <> colon <+> ppr txt) `thenRn_`
    returnRn (extendNameEnv deprec_env (zip names (repeat txt)))

namesFromIE :: IE a -> [a]
namesFromIE (IEVar            n   ) = [n]
namesFromIE (IEThingAbs       n   ) = [n]
namesFromIE (IEThingAll       n   ) = [n]
namesFromIE (IEThingWith      n ns) = n:ns
namesFromIE (IEModuleContents _   ) = []
\end{code}


%********************************************************
%*							*
\subsection{Loading usage information}
%*							*
%********************************************************

\begin{code}
upToDate  = True
outOfDate = False

checkUpToDate :: ModuleName -> RnMG Bool	-- True <=> no need to recompile
	-- When this guy is called, we already know that the
	-- source code is unchanged from last time
checkUpToDate mod_name
  = getIfacesRn					`thenRn` \ ifaces ->
    findAndReadIface doc_str mod_name 
		     False {- Not hi-boot -}	`thenRn` \ read_result ->

	-- CHECK WHETHER WE HAVE IT ALREADY
    case read_result of
	Left err -> 	-- Old interface file not found, or garbled, so we'd better bail out
		    traceRn (vcat [ptext SLIT("No old iface") <+> pprModuleName mod_name,
				   err])			`thenRn_`
		    returnRn outOfDate

	Right iface
		-> 	-- Found it, so now check it
		    checkModUsage (pi_usages iface)
  where
	-- Only look in current directory, with suffix .hi
    doc_str = sep [ptext SLIT("need usage info from"), pprModuleName mod_name]

checkModUsage [] = returnRn upToDate		-- Yes!  Everything is up to date!

checkModUsage ((mod_name, old_mod_vers, _, _, Specifically []) : rest)
	-- If CurrentModule.hi contains 
	--	import Foo :: ;
	-- then that simply records that Foo lies below CurrentModule in the
	-- hierarchy, but CurrentModule doesn't depend in any way on Foo.
	-- In this case we don't even want to open Foo's interface.
  = traceRn (ptext SLIT("Nothing used from:") <+> ppr mod_name)	`thenRn_`
    checkModUsage rest	-- This one's ok, so check the rest

checkModUsage ((mod_name, old_mod_vers, _, _, whats_imported)  : rest)
  = tryLoadInterface doc_str mod_name ImportBySystem	`thenRn` \ (ifaces, maybe_err) ->
    case maybe_err of {
	Just err -> traceRn (sep [ptext SLIT("Can't find version number for module"), 
			     pprModuleName mod_name])		`thenRn_`
		     returnRn outOfDate ;
		-- Couldn't find or parse a module mentioned in the
		-- old interface file.  Don't complain -- it might just be that
		-- the current module doesn't need that import and it's been deleted
	Nothing -> 
    let
	new_mod_vers = case lookupFM (iImpModInfo ifaces) mod_name of
			   Just (version, _, _, _) -> version
    in
	-- If the module version hasn't changed, just move on
    if new_mod_vers == old_mod_vers then
	traceRn (sep [ptext SLIT("Module version unchanged:"), pprModuleName mod_name])
	`thenRn_` checkModUsage rest
    else
    traceRn (sep [ptext SLIT("Module version has changed:"), pprModuleName mod_name])
    `thenRn_`
	-- Module version changed, so check entities inside

	-- If the usage info wants to say "I imported everything from this module"
	--     it does so by making whats_imported equal to Everything
	-- In that case, we must recompile
    case whats_imported of {
      Everything -> traceRn (ptext SLIT("...and I needed the whole module"))	`thenRn_`
		    returnRn outOfDate;		   -- Bale out

      Specifically old_local_vers ->

	-- Non-empty usage list, so check item by item
    checkEntityUsage mod_name (iDecls ifaces) old_local_vers	`thenRn` \ up_to_date ->
    if up_to_date then
	traceRn (ptext SLIT("...but the bits I use haven't."))	`thenRn_`
	checkModUsage rest	-- This one's ok, so check the rest
    else
	returnRn outOfDate		-- This one failed, so just bail out now
    }}
  where
    doc_str = sep [ptext SLIT("need version info for"), pprModuleName mod_name]


checkEntityUsage mod decls [] 
  = returnRn upToDate	-- Yes!  All up to date!

checkEntityUsage mod decls ((occ_name,old_vers) : rest)
  = mkImportedGlobalName mod occ_name 	`thenRn` \ name ->
    case lookupNameEnv decls name of

	Nothing       -> 	-- We used it before, but it ain't there now
			  traceRn (sep [ptext SLIT("No longer exported:"), ppr name])
			  `thenRn_` returnRn outOfDate

	Just (new_vers,_,_,_) 	-- It's there, but is it up to date?
		| new_vers == old_vers
			-- Up to date, so check the rest
		-> checkEntityUsage mod decls rest

		| otherwise
			-- Out of date, so bale out
		-> traceRn (sep [ptext SLIT("Out of date:"), ppr name])  `thenRn_`
		   returnRn outOfDate
\end{code}


%*********************************************************
%*							*
\subsection{Getting in a declaration}
%*							*
%*********************************************************

\begin{code}
importDecl :: Name -> RnMG (Maybe (Module, RdrNameHsDecl))
	-- Returns Nothing for 
	--	(a) wired in name
	--	(b) local decl
	--	(c) already slurped

importDecl name
  | isWiredInName name
  = returnRn Nothing
  | otherwise
  = getSlurped 				`thenRn` \ already_slurped ->
    if name `elemNameSet` already_slurped then
	returnRn Nothing	-- Already dealt with
    else
	if isLocallyDefined name then	-- Don't bring in decls from
					-- the renamed module's own interface file
		  addWarnRn (importDeclWarn name) `thenRn_`
		  returnRn Nothing
	else
	getNonWiredInDecl name
\end{code}

\begin{code}
getNonWiredInDecl :: Name -> RnMG (Maybe (Module, RdrNameHsDecl))
getNonWiredInDecl needed_name 
  = traceRn doc_str				`thenRn_`
    loadHomeInterface doc_str needed_name	`thenRn` \ ifaces ->
    case lookupNameEnv (iDecls ifaces) needed_name of

      Just (version,avail,_,decl)
	-> recordSlurp (Just version) avail	`thenRn_`
	   returnRn (Just decl)

      Nothing 	 	-- Can happen legitimately for "Optional" occurrences
	-> addErrRn (getDeclErr needed_name)	`thenRn_` 
	   returnRn Nothing
  where
     doc_str = ptext SLIT("need decl for") <+> ppr needed_name
\end{code}

@getWiredInDecl@ maps a wired-in @Name@ to what it makes available.
It behaves exactly as if the wired in decl were actually in an interface file.
Specifically,
\begin{itemize}
\item	if the wired-in name is a data type constructor or a data constructor, 
	it brings in the type constructor and all the data constructors; and
	marks as ``occurrences'' any free vars of the data con.

\item 	similarly for synonum type constructor

\item 	if the wired-in name is another wired-in Id, it marks as ``occurrences''
	the free vars of the Id's type.

\item	it loads the interface file for the wired-in thing for the
	sole purpose of making sure that its instance declarations are available
\end{itemize}
All this is necessary so that we know all types that are ``in play'', so
that we know just what instances to bring into scope.
	


    
%*********************************************************
%*							*
\subsection{Getting what a module exports}
%*							*
%*********************************************************

@getInterfaceExports@ is called only for directly-imported modules.

\begin{code}
getInterfaceExports :: ModuleName -> WhereFrom -> RnMG (Module, Avails)
getInterfaceExports mod_name from
  = loadInterface doc_str mod_name from	`thenRn` \ ifaces ->
    case lookupFM (iImpModInfo ifaces) mod_name of
	Just (_, _, _, Just (mod, _, avails)) -> returnRn (mod, avails)
	-- loadInterface always puts something in the map
	-- even if it's a fake
  where
    doc_str = sep [pprModuleName mod_name, ptext SLIT("is directly imported")]
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations are handled specially}
%*							*
%*********************************************************

\begin{code}
getImportedInstDecls :: NameSet -> RnMG [(Module,RdrNameHsDecl)]
getImportedInstDecls gates
  =    	-- First, load any orphan-instance modules that aren't aready loaded
	-- Orphan-instance modules are recorded in the module dependecnies
    getIfacesRn 					`thenRn` \ ifaces ->
    let
	orphan_mods =
	  [mod | (mod, (_, True, _, Nothing)) <- fmToList (iImpModInfo ifaces)]
    in
    loadOrphanModules orphan_mods			`thenRn_` 

	-- Now we're ready to grab the instance declarations
	-- Find the un-gated ones and return them, 
	-- removing them from the bag kept in Ifaces
    getIfacesRn 					`thenRn` \ ifaces ->
    let
	(decls, new_insts) = selectGated gates (iInsts ifaces)
    in
    setIfacesRn (ifaces { iInsts = new_insts })		`thenRn_`

    traceRn (sep [text "getImportedInstDecls:", 
		  nest 4 (fsep (map ppr gate_list)),
		  text "Slurped" <+> int (length decls) <+> text "instance declarations",
		  nest 4 (vcat (map ppr_brief_inst_decl decls))])	`thenRn_`
    returnRn decls
  where
    gate_list      = nameSetToList gates

ppr_brief_inst_decl (mod, InstD (InstDecl inst_ty _ _ _ _))
  = case inst_ty of
	HsForAllTy _ _ tau -> ppr tau
	other		   -> ppr inst_ty

getImportedRules :: RnMG [(Module,RdrNameHsDecl)]
getImportedRules 
  | opt_IgnoreIfacePragmas = returnRn []
  | otherwise
  = getIfacesRn 	`thenRn` \ ifaces ->
    let
	gates		   = iSlurp ifaces	-- Anything at all that's been slurped
	(decls, new_rules) = selectGated gates (iRules ifaces)
    in
    setIfacesRn (ifaces { iRules = new_rules })		`thenRn_`
    traceRn (sep [text "getImportedRules:", 
		  text "Slurped" <+> int (length decls) <+> text "rules"])	`thenRn_`
    returnRn decls

selectGated gates decl_bag
	-- Select only those decls whose gates are *all* in 'gates'
#ifdef DEBUG
  | opt_NoPruneDecls	-- Just to try the effect of not gating at all
  = (foldrBag (\ (_,d) ds -> d:ds) [] decl_bag, emptyBag)	-- Grab them all

  | otherwise
#endif
  = foldrBag select ([], emptyBag) decl_bag
  where
    select (reqd, decl) (yes, no)
	| isEmptyNameSet (reqd `minusNameSet` gates) = (decl:yes, no)
	| otherwise				     = (yes,      (reqd,decl) `consBag` no)

lookupFixity :: Name -> RnMS Fixity
lookupFixity name
  | isLocallyDefined name
  = getFixityEnv			`thenRn` \ local_fix_env ->
    case lookupNameEnv local_fix_env name of 
	Just (FixitySig _ fix _) -> returnRn fix
	Nothing		  	 -> returnRn defaultFixity

  | otherwise	-- Imported
      -- For imported names, we have to get their fixities by doing a loadHomeInterface,
      -- and consulting the Ifaces that comes back from that, because the interface
      -- file for the Name might not have been loaded yet.  Why not?  Suppose you import module A,
      -- which exports a function 'f', which is defined in module B.  Then B isn't loaded
      -- right away (after all, it's possible that nothing from B will be used).
      -- When we come across a use of 'f', we need to know its fixity, and it's then,
      -- and only then, that we load B.hi.  That is what's happening here.
  = loadHomeInterface doc name		`thenRn` \ ifaces ->
    case lookupNameEnv (iFixes ifaces) name of
	Just (FixitySig _ fix _) -> returnRn fix 
	Nothing 		 -> returnRn defaultFixity
  where
    doc = ptext SLIT("Checking fixity for") <+> ppr name
\end{code}


%*********************************************************
%*							*
\subsection{Keeping track of what we've slurped, and version numbers}
%*							*
%*********************************************************

getImportVersions figures out what the ``usage information'' for this
moudule is; that is, what it must record in its interface file as the
things it uses.  It records:

\begin{itemize}
\item	(a) anything reachable from its body code
\item	(b) any module exported with a @module Foo@
\item   (c) anything reachable from an exported item
\end{itemize}

Why (b)?  Because if @Foo@ changes then this module's export list
will change, so we must recompile this module at least as far as
making a new interface file --- but in practice that means complete
recompilation.

Why (c)?  Consider this:
\begin{verbatim}
	module A( f, g ) where	|	module B( f ) where
	  import B( f )		|	  f = h 3
	  g = ...		|	  h = ...
\end{verbatim}

Here, @B.f@ isn't used in A.  Should we nevertheless record @B.f@ in
@A@'s usages?  Our idea is that we aren't going to touch A.hi if it is
*identical* to what it was before.  If anything about @B.f@ changes
than anyone who imports @A@ should be recompiled in case they use
@B.f@ (they'll get an early exit if they don't).  So, if anything
about @B.f@ changes we'd better make sure that something in A.hi
changes, and the convenient way to do that is to record the version
number @B.f@ in A.hi in the usage list.  If B.f changes that'll force a
complete recompiation of A, which is overkill but it's the only way to 
write a new, slightly different, A.hi.

But the example is tricker.  Even if @B.f@ doesn't change at all,
@B.h@ may do so, and this change may not be reflected in @f@'s version
number.  But with -O, a module that imports A must be recompiled if
@B.h@ changes!  So A must record a dependency on @B.h@.  So we treat
the occurrence of @B.f@ in the export list *just as if* it were in the
code of A, and thereby haul in all the stuff reachable from it.

[NB: If B was compiled with -O, but A isn't, we should really *still*
haul in all the unfoldings for B, in case the module that imports A *is*
compiled with -O.  I think this is the case.]

Even if B is used at all we get a usage line for B
	import B <n> :: ... ;
in A.hi, to record the fact that A does import B.  This is used to decide
to look to look for B.hi rather than B.hi-boot when compiling a module that
imports A.  This line says that A imports B, but uses nothing in it.
So we'll get an early bale-out when compiling A if B's version changes.

\begin{code}
getImportVersions :: ModuleName			-- Name of this module
		  -> ExportEnv			-- Info about exports 
		  -> RnMG (VersionInfo Name)	-- Version info for these names

getImportVersions this_mod (ExportEnv _ _ export_all_mods)
  = getIfacesRn					`thenRn` \ ifaces ->
    let
	mod_map   = iImpModInfo ifaces
	imp_names = iVSlurp     ifaces

	-- mv_map groups together all the things imported from a particular module.
	mv_map :: FiniteMap ModuleName [(Name,Version)]
	mv_map = foldr add_mv emptyFM imp_names

	-- Build the result list by adding info for each module.
	-- For (a) a library module, we don't record it at all unless it contains orphans
	-- 	   (We must never lose track of orphans.)
	-- 
	--     (b) a source-imported module, don't record the dependency at all
	--	
	-- (b) may seem a bit strange.  The idea is that the usages in a .hi file records
	-- *all* the module's dependencies other than the loop-breakers.  We use
	-- this info in findAndReadInterface to decide whether to look for a .hi file or
	-- a .hi-boot file.  
	--
	-- This means we won't track version changes, or orphans, from .hi-boot files.
	-- The former is potentially rather bad news.  It could be fixed by recording
	-- whether something is a boot file along with the usage info for it, but 
	-- I can't be bothered just now.

	mk_version_info mod_name (version, has_orphans, is_boot, contents) so_far
	   | mod_name == this_mod	-- Check if M appears in the set of modules 'below' M
					-- This seems like a convenient place to check
	   = WARN( not is_boot, ptext SLIT("Wierd:") <+> ppr this_mod <+> 
			        ptext SLIT("imports itself (perhaps indirectly)") )
	     so_far
 
	   | otherwise
	   = let
		go_for_it exports = (mod_name, version, has_orphans, is_boot, exports) 
                                    : so_far
	     in 
	     case contents of
		Nothing -> 	-- We didn't even open the interface
			-- This happens when a module, Foo, that we explicitly imported has 
			-- 'import Baz' in its interface file, recording that Baz is below
			-- Foo in the module dependency hierarchy.  We want to propagate this
			-- information.  The Nothing says that we didn't even open the interface
			-- file but we must still propagate the dependeny info.
			-- The module in question must be a local module (in the same package)
		   go_for_it (Specifically [])

		Just (mod, how_imported, _)
		   |  is_sys_import && is_lib_module && not has_orphans
		   -> so_far		
	   
		   |  is_lib_module 			-- Record the module but not detailed
		   || mod_name `elem` export_all_mods	-- version information for the imports
		   -> go_for_it Everything

		   |  otherwise
		   -> case lookupFM mv_map mod_name of
			Just whats_imported -> go_for_it (Specifically whats_imported)
			Nothing		    -> go_for_it (Specifically [])
						-- This happens if you have
						--	import Foo
						-- but don't actually *use* anything from Foo
					 	-- In which case record an empty dependency list
		   where
		     is_lib_module = not (isLocalModule mod)
		     is_sys_import = case how_imported of
					ImportBySystem -> True
					other	       -> False
	     
    in

    returnRn (foldFM mk_version_info [] mod_map)
  where
     add_mv v@(name, version) mv_map
      = addToFM_C add_item mv_map mod [v] 
      where
	 mod = moduleName (nameModule name)
         add_item vs _ = (v:vs)
\end{code}

\begin{code}
getSlurped
  = getIfacesRn 	`thenRn` \ ifaces ->
    returnRn (iSlurp ifaces)

recordSlurp maybe_version avail
-- Nothing	for locally defined names
-- Just version for imported names
  = getIfacesRn 	`thenRn` \ ifaces@(Ifaces { iSlurp  = slurped_names,
					            iVSlurp = imp_names }) ->
    let
	new_slurped_names = addAvailToNameSet slurped_names avail

	new_imp_names = case maybe_version of
			   Just version	-> (availName avail, version) : imp_names
			   Nothing      -> imp_names
    in
    setIfacesRn (ifaces { iSlurp  = new_slurped_names,
			  iVSlurp = new_imp_names })
\end{code}


%*********************************************************
%*							*
\subsection{Getting binders out of a declaration}
%*							*
%*********************************************************

@getDeclBinders@ returns the names for a @RdrNameHsDecl@.
It's used for both source code (from @availsFromDecl@) and interface files
(from @loadDecl@).

It doesn't deal with source-code specific things: @ValD@, @DefD@.  They
are handled by the sourc-code specific stuff in @RnNames@.

\begin{code}
getDeclBinders :: (RdrName -> SrcLoc -> RnM d Name)	-- New-name function
		-> RdrNameHsDecl
		-> RnM d (Maybe AvailInfo)

getDeclBinders new_name (TyClD (TyData _ _ tycon _ condecls _ _ src_loc))
  = new_name tycon src_loc			`thenRn` \ tycon_name ->
    getConFieldNames new_name condecls		`thenRn` \ sub_names ->
    returnRn (Just (AvailTC tycon_name (tycon_name : nub sub_names)))
	-- The "nub" is because getConFieldNames can legitimately return duplicates,
	-- when a record declaration has the same field in multiple constructors

getDeclBinders new_name (TyClD (TySynonym tycon _ _ src_loc))
  = new_name tycon src_loc		`thenRn` \ tycon_name ->
    returnRn (Just (AvailTC tycon_name [tycon_name]))

getDeclBinders new_name (TyClD (ClassDecl _ cname _ _ sigs _ _ _ _ _ _ src_loc))
  = new_name cname src_loc			`thenRn` \ class_name ->

	-- Record the names for the class ops
    let
	-- just want class-op sigs
	op_sigs = filter isClassOpSig sigs
    in
    mapRn (getClassOpNames new_name) op_sigs	`thenRn` \ sub_names ->

    returnRn (Just (AvailTC class_name (class_name : sub_names)))

getDeclBinders new_name (SigD (IfaceSig var ty prags src_loc))
  = new_name var src_loc			`thenRn` \ var_name ->
    returnRn (Just (Avail var_name))

getDeclBinders new_name (FixD _)  = returnRn Nothing

    -- foreign declarations
getDeclBinders new_name (ForD (ForeignDecl nm kind _ dyn _ loc))
  | binds_haskell_name kind dyn
  = new_name nm loc		    `thenRn` \ name ->
    returnRn (Just (Avail name))

  | otherwise -- a foreign export
  = lookupImplicitOccRn nm `thenRn_` 
    returnRn Nothing

getDeclBinders new_name (DefD _)  = returnRn Nothing
getDeclBinders new_name (InstD _) = returnRn Nothing
getDeclBinders new_name (RuleD _) = returnRn Nothing

binds_haskell_name (FoImport _) _   = True
binds_haskell_name FoLabel      _   = True
binds_haskell_name FoExport  ext_nm = isDynamicExtName ext_nm

----------------
getConFieldNames new_name (ConDecl con _ _ _ (RecCon fielddecls) src_loc : rest)
  = mapRn (\n -> new_name n src_loc) (con:fields)	`thenRn` \ cfs ->
    getConFieldNames new_name rest			`thenRn` \ ns  -> 
    returnRn (cfs ++ ns)
  where
    fields = concat (map fst fielddecls)

getConFieldNames new_name (ConDecl con _ _ _ condecl src_loc : rest)
  = new_name con src_loc		`thenRn` \ n ->
    (case condecl of
      NewCon _ (Just f) -> 
        new_name f src_loc `thenRn` \ new_f ->
	returnRn [n,new_f]
      _ -> returnRn [n])		`thenRn` \ nn ->
    getConFieldNames new_name rest	`thenRn` \ ns -> 
    returnRn (nn ++ ns)

getConFieldNames new_name [] = returnRn []

getClassOpNames new_name (ClassOpSig op _ _ _ src_loc) = new_name op src_loc
\end{code}

@getDeclSysBinders@ gets the implicit binders introduced by a decl.
A the moment that's just the tycon and datacon that come with a class decl.
They aren't returned by @getDeclBinders@ because they aren't in scope;
but they {\em should} be put into the @DeclsMap@ of this module.

Note that this excludes the default-method names of a class decl,
and the dict fun of an instance decl, because both of these have 
bindings of their own elsewhere.

\begin{code}
getDeclSysBinders new_name (TyClD (ClassDecl _ cname _ _ sigs _ _ tname dname dwname snames src_loc))
  = sequenceRn [new_name n src_loc | n <- (tname : dname : dwname : snames)]

getDeclSysBinders new_name (TyClD (TyData _ _ _ _ cons _ _ _))
  = sequenceRn [new_name wkr_name src_loc | ConDecl _ wkr_name _ _ _ src_loc <- cons]

getDeclSysBinders new_name other_decl
  = returnRn []
\end{code}

%*********************************************************
%*							*
\subsection{Reading an interface file}
%*							*
%*********************************************************

\begin{code}
findAndReadIface :: SDoc -> ModuleName 
		 -> IsBootInterface	-- True  <=> Look for a .hi-boot file
					-- False <=> Look for .hi file
		 -> RnM d (Either Message ParsedIface)
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 

findAndReadIface doc_str mod_name hi_boot_file
  = traceRn trace_msg			`thenRn_`
      -- we keep two maps for interface files,
      -- one for 'normal' ones, the other for .hi-boot files,
      -- hence the need to signal which kind we're interested.

    getHiMaps			`thenRn` \ (search_path, hi_map, hiboot_map) ->
    let
	relevant_map | hi_boot_file = hiboot_map
		     | otherwise    = hi_map
    in	
    case lookupFM relevant_map mod_name of
	-- Found the file
      Just fpath -> traceRn (ptext SLIT("...reading from") <+> text fpath)	`thenRn_`
		    readIface mod_name fpath
	
	-- Can't find it
      Nothing    -> traceRn (ptext SLIT("...not found"))	`thenRn_`
		    returnRn (Left (noIfaceErr mod_name hi_boot_file search_path))

  where
    trace_msg = sep [hsep [ptext SLIT("Reading"), 
			   if hi_boot_file then ptext SLIT("[boot]") else empty,
			   ptext SLIT("interface for"), 
			   pprModuleName mod_name <> semi],
		     nest 4 (ptext SLIT("reason:") <+> doc_str)]
\end{code}

@readIface@ tries just the one file.

\begin{code}
readIface :: ModuleName -> String -> RnM d (Either Message ParsedIface)
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 
readIface wanted_mod file_path
  = ioToRnM (hGetStringBuffer False file_path)       `thenRn` \ read_result ->
    case read_result of
	Right contents	  -> 
             case parseIface contents
			PState{ bol = 0#, atbol = 1#,
				context = [],
				glasgow_exts = 1#,
				loc = mkSrcLoc (mkFastString file_path) 1 } of
		  POk _  (PIface iface) ->
		      warnCheckRn (read_mod == wanted_mod)
		    		  (hiModuleNameMismatchWarn wanted_mod read_mod) `thenRn_`
		      returnRn (Right iface)
		    where
		      read_mod = moduleName (pi_mod iface)

		  PFailed err   -> bale_out err
	          parse_result 	-> bale_out empty
		 	-- This last case can happen if the interface file is (say) empty
			-- in which case the parser thinks it looks like an IdInfo or
			-- something like that.  Just an artefact of the fact that the
			-- parser is used for several purposes at once.

        Left io_err -> bale_out (text (show io_err))
  where
    bale_out err = returnRn (Left (badIfaceFile file_path err))
\end{code}

%*********************************************************
%*						 	 *
\subsection{Errors}
%*							 *
%*********************************************************

\begin{code}
noIfaceErr mod_name boot_file search_path
  = vcat [ptext SLIT("Could not find interface file for") <+> quotes (pprModuleName mod_name),
	  ptext SLIT("in the directories") <+> 
			-- \& to avoid cpp interpreting this string as a
			-- comment starter with a pre-4.06 mkdependHS --SDM
		vcat [ text dir <> text "/\&*" <> pp_suffix suffix 
		     | (dir,suffix) <- search_path]
	]
  where
    pp_suffix suffix | boot_file = ptext SLIT(".hi-boot")
		     | otherwise = text suffix

badIfaceFile file err
  = vcat [ptext SLIT("Bad interface file:") <+> text file, 
	  nest 4 err]

getDeclErr name
  = vcat [ptext SLIT("Failed to find interface decl for") <+> quotes (ppr name),
	  ptext SLIT("from module") <+> quotes (ppr (nameModule name))
	 ]

getDeclWarn name loc
  = sep [ptext SLIT("Failed to find (optional) interface decl for") <+> quotes (ppr name),
	 ptext SLIT("desired at") <+> ppr loc]

importDeclWarn name
  = sep [ptext SLIT(
    "Compiler tried to import decl from interface file with same name as module."), 
	 ptext SLIT(
    "(possible cause: module name clashes with interface file already in scope.)")
	] $$
    hsep [ptext SLIT("name:"), quotes (ppr name)]

warnRedundantSourceImport mod_name
  = ptext SLIT("Unnecessary {- SOURCE -} in the import of module")
          <+> quotes (pprModuleName mod_name)

hiModuleNameMismatchWarn :: ModuleName -> ModuleName  -> Message
hiModuleNameMismatchWarn requested_mod read_mod = 
    hsep [ ptext SLIT("Something is amiss; requested module name")
	 , pprModuleName requested_mod
	 , ptext SLIT("differs from name found in the interface file")
   	 , pprModuleName read_mod
  	 ]

\end{code}
