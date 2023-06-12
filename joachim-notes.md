Joachim's notes from Zurihac 2023
---------------------------------

It was relatively easy to find a command line for hugs that would start loading
some files from the GHC source, see <./joachims-experiments.sh>.

I then played around with patching Hugs. In particular:

## Magic hash

Adding '#" to IDAFTER in src/char.cs
(5471c5a65da8a8dec75e8f1a925e6640e7af33a1)

That helped some, but other uses of `#`, in particular in export lists,
still failed. This hack made progresss, but would have to be fixed properly

    diff --git a/hugs98-Sep2006/src/parser.y b/hugs98-Sep2006/src/parser.y
    index 206c8fe..c3bba42 100644
    --- a/hugs98-Sep2006/src/parser.y
    +++ b/hugs98-Sep2006/src/parser.y
    @@ -187,6 +187,8 @@ exports       : exports ',' export          {$$ = gc3(cons($3,$1));}
      * Relaxing the rule lets us explicitly export (:) from the Prelude.
      */
     export   : qvar                        {$$ = $1;}
    +         // This is a horrible hack to continue parsing here, needs to be fixed
    +          | qvar '#'                   {$$ = $1;}
              | qcon                        {$$ = $1;}
              | qconid '(' UPTO ')'         {$$ = gc4(pair($1,DOTDOT));}
              | qconid '(' qnames ')'       {$$ = gc4(pair($1,$3));}

## Recursive modules

Next I tried to make Hugs understand `.hs-boot` files. See the example files in 

`./tests/`

I skipped the parsing part first, and instead hard-coded the import of
`RecB` in `RecA` to go via a boot file:


    diff --git a/hugs98-Sep2006/src/script.c b/hugs98-Sep2006/src/script.c
    index 8496005..b1b8e91 100644
    --- a/hugs98-Sep2006/src/script.c
    +++ b/hugs98-Sep2006/src/script.c
    @@ -198,6 +198,9 @@ Long   len; {                           /* length of script file   */
     	forgetAScript(numScripts);
     	errFail();
         }
    +    if (!quiet) {
    +	Printf("Needs imports: %b\n",needsImports);  FlushStdout();
    +    }
         if (needsImports) return FALSE;
         checkDefns();
         typeCheckDefns();
    @@ -230,6 +233,11 @@ List imps; {
     	    EEND;
     	}
     
    +	fprintf(stderr, "-> %s -> %s <-\n", scriptTable[origPos].fileName, iname);
    +	if (!strcmp(scriptTable[origPos].fileName, "../tests/RecA.hs")) {
    +	   iname = "../tests/RecB.hs-boot";
    +	};
    +
     	rname = RealPath(iname);
     	for (; i<namesUpto; i++)
     	    if (filenamecmp(scriptTable[i].realName,rname)==0)
    @@ -244,7 +252,7 @@ List imps; {
     		addScriptName(iname,FALSE);
     		if (inOrigDir)
     		    scriptTable[i].directory = strCopy(origDir);
    -	    } else if (scriptTable[i].postponed) {/* imported by itself? */
    +	    } else if (0 && scriptTable[i].postponed) {/* imported by itself? */
     		ERRMSG(0)
     		  "Recursive import dependency between \"%s\" and \"%s\"",
     		  scriptTable[origPos].fileName, iname
    diff --git a/hugs98-Sep2006/src/static.c b/hugs98-Sep2006/src/static.c
    index aabb3d1..e6bf680 100644
    --- a/hugs98-Sep2006/src/static.c
    +++ b/hugs98-Sep2006/src/static.c
    @@ -342,7 +342,7 @@ Cell nm; {
     	if ( moduleUserPrelude == 0 && t == textUserPrelude ) {
     	  moduleUserPrelude = m;
     	}
    -    } else if (!isPreludeScript()) {
    +    } else if (0 && !isPreludeScript()) {
     	/* You're allowed to break the rules in the Prelude! */
     #if HSCRIPT
     	reloadModule = textToStr(t);
    @@ -408,7 +408,7 @@ Cell what; {				/* SYNONYM/DATATYPE/etc...	   */
         Text t = textOf(getHead(lhs));
         Tycon tc = findTycon(t);
     
    -    if ( nonNull(tc) ) {
    +    if ( 0 && nonNull(tc) ) {
     	ERRMSG(line) "Multiple declarations of type constructor \"%s\"",
     		     textToStr(t)
     	EEND;
    diff --git a/hugs98-Sep2006/src/storage.c b/hugs98-Sep2006/src/storage.c
    index d2f2a18..36a97c4 100644
    --- a/hugs98-Sep2006/src/storage.c
    +++ b/hugs98-Sep2006/src/storage.c
    @@ -1556,6 +1556,7 @@ Module m; {
     	  module(currentModule).modImports = NIL;
     	}
     	currentModule = m; /* This is the only assignment to currentModule */
    +	fprintf(stderr, "setCurrModule(%s)\n", textToStr(module(m).text));
     	for (i=0; i<TYCONHSZ; ++i)
     	    tyconHash[i] = NIL;
     	mapProc(hashTycon,module(m).tycons);


Run like this:

    HUGSDIR=hugsdir src/hugs  -q  ../tests/RecB.hs

Unfortunately, that crashes:

    $ HUGSDIR=hugsdir src/hugs >
    __   __ __  __  ____   ___      _________________________________________
    ||   || ||  || ||  || ||__      Hugs 98: Based on the Haskell 98 standard
    ||___|| ||__|| ||__||  __||     Copyright (c) 1994-2005
    ||---||         ___||           World Wide Web: http://haskell.org/hugs
    ||   ||                         Bugs: http://hackage.haskell.org/trac/hugs
    ||   || Version: September 2006 _________________________________________
    
    Haskell 98 mode: Restart with command line option -98 to enable extensions
    
    setCurrModule(Hugs.Prelude)
    Reading file "hugsdir/packages/hugsbase/Hugs/Prelude.hs":
    Needs imports: 0
    Reading file "hugsdir/packages/base/Prelude.hs":
    ParsingsetCurrModule(Prelude)
    -> hugsdir/packages/base/Prelude.hs -> hugsdir/packages/hugsbase/Hugs/Prelude.hs <-
    Needs imports: 0
    Reading file "hugsdir/packages/hugsbase/Hugs.hs":
    ParsingsetCurrModule(Hugs)
    Needs imports: 0
    Reading file "../tests/RecB.hs":
    ParsingsetCurrModule(RecB)
    -> ../tests/RecB.hs -> ../tests/RecA.hs <-
    Needs imports: 1
    Reading file "../tests/RecA.hs":
    ParsingsetCurrModule(RecA)
    -> ../tests/RecA.hs -> ../tests/RecB.hs <-
    Needs imports: 1
    Reading file "../tests/RecB.hs-boot":
    ParsingsetCurrModule(RecB)
    Needs imports: 0
    Reading file "../tests/RecA.hs":
    ParsingsetCurrModule(RecA)
    -> ../tests/RecA.hs -> ../tests/RecB.hs <-
    Needs imports: 0
    Reading file "../tests/RecB.hs":
    ParsingsetCurrModule(RecB)
    -> ../tests/RecB.hs -> ../tests/RecA.hs <-
    Needs imports: 0
    setCurrModule(RecA)
    INTERNAL ERROR: findModid

This can be loaded in gdb (`HUGSDIR=hugsdir gdb -args src/hugs  -q
../tests/RecB.hs`), `break internal` is useful.
