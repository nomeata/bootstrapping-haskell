
/* --------------------------------------------------------------------------
 * Load symbols required from the Prelude
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: link.c,v $
 * $Revision: 1.60 $
 * $Date: 2000/04/27 16:35:29 $
 * ------------------------------------------------------------------------*/

#include "hugsbasictypes.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "Rts.h"                        /* to make Prelude.h palatable     */
#include "Assembler.h"                  /* for asmPrimOps and AsmReps      */
#include "Prelude.h"                    /* for fixupRTStoPreludeRefs       */


Type typeArrow;                         /* Function spaces                 */

Type typeChar;
Type typeInt;
Type typeInteger;
Type typeWord;
Type typeAddr;
Type typePrimArray;            
Type typePrimByteArray;
Type typeRef;                  
Type typePrimMutableArray;     
Type typePrimMutableByteArray; 
Type typeFloat;
Type typeDouble;
Type typeStable;
Type typeThreadId;
Type typeMVar;
#ifdef PROVIDE_WEAK
Type typeWeak;
#endif
#ifdef PROVIDE_FOREIGN
Type typeForeign;
#endif

Type typeList;
Type typeUnit;
Type typeString;
Type typeBool;
Type typeST;
Type typeIO;
Type typeException;

Class classEq;                          /* `standard' classes              */
Class classOrd;
Class classShow;
Class classRead;
Class classIx;
Class classEnum;
Class classBounded;

Class classReal;                        /* `numeric' classes               */
Class classIntegral;
Class classRealFrac;
Class classRealFloat;
Class classFractional;
Class classFloating;
Class classNum;
Class classMonad;                       /* Monads and monads with a zero   */

List stdDefaults;                       /* standard default values         */

Name nameTrue;    
Name nameFalse;            /* primitive boolean constructors  */
Name nameNil;     
Name nameCons;             /* primitive list constructors     */
Name nameUnit;                          /* primitive Unit type constructor */

Name nameEq;    
Name nameFromInt;
Name nameFromDouble;       /* coercion of numerics            */
Name nameFromInteger;
Name nameReturn;  
Name nameBind;             /* for translating monad comps     */
Name nameZero;                          /* for monads with a zero          */

Name nameId;
Name nameShow;
Name namePutStr;
Name nameRunIO_toplevel;
Name namePrint;

Name nameOtherwise;
Name nameUndefined;                     /* generic undefined value         */
Name namePmSub; 
Name namePMFail;
Name nameEqChar;
Name namePmInt;
Name namePmInteger;
Name namePmDouble;
Name namePmLe;
Name namePmSubtract;
Name namePmFromInteger;
Name nameMkIO;
Name nameUnpackString;
Name nameError;
Name nameInd;
Name nameCreateAdjThunk;

Name nameAnd;
Name nameCompAux;
Name nameRangeSize;
Name nameComp;
Name nameShowField;
Name nameApp;
Name nameShowParen;
Name nameReadParen;
Name nameLex;
Name nameReadField;
Name nameFlip;

Name namePrimSeq;
Name namePrimCatch;
Name namePrimRaise;
Name namePrimTakeMVar;

Name nameFromTo;
Name nameFromThen;
Name nameFrom;
Name nameFromThenTo;
Name nameNegate;

Name nameAssert;
Name nameAssertError;
Name nameTangleMessage;
Name nameIrrefutPatError;
Name nameNoMethodBindingError;
Name nameNonExhaustiveGuardsError;
Name namePatError;
Name nameRecSelError;
Name nameRecConError;
Name nameRecUpdError;

/* these names are required before we've had a chance to do the right thing */
Name nameSel;
Name nameUnsafeUnpackCString;

/* constructors used during translation and codegen */
Name nameMkC;                           /* Char#        -> Char           */
Name nameMkI;                           /* Int#         -> Int            */
Name nameMkInteger;                     /* Integer#     -> Integer        */
Name nameMkW;                           /* Word#        -> Word           */
Name nameMkA;                           /* Addr#        -> Addr            */
Name nameMkF;                           /* Float#       -> Float           */
Name nameMkD;                           /* Double#      -> Double          */
Name nameMkPrimArray;            
Name nameMkPrimByteArray;
Name nameMkRef;                  
Name nameMkPrimMutableArray;     
Name nameMkPrimMutableByteArray; 
Name nameMkStable;                      /* StablePtr# a -> StablePtr a     */
Name nameMkThreadId;                    /* ThreadId#    -> ThreadId        */
Name nameMkPrimMVar;                    /* MVar# a      -> MVar a          */
#ifdef PROVIDE_WEAK
Name nameMkWeak;                        /* Weak# a      -> Weak a          */
#endif
#ifdef PROVIDE_FOREIGN
Name nameMkForeign;                     /* ForeignObj#  -> ForeignObj      */
#endif



Name nameMinBnd;
Name nameMaxBnd;
Name nameCompare;
Name nameShowsPrec;
Name nameIndex;
Name nameReadsPrec; 
Name nameRange;
Name nameEQ;
Name nameInRange;
Name nameGt;
Name nameLe;
Name namePlus;
Name nameMult;
Name nameMFail;
Type typeOrdering;
Module modulePrelPrim;
Module modulePrelude;
Name nameMap;
Name nameMinus;

/* --------------------------------------------------------------------------
 * Frequently used type skeletons:
 * ------------------------------------------------------------------------*/

Type  arrow;                     /* mkOffset(0) -> mkOffset(1)      */
Type  boundPair;                 /* (mkOffset(0),mkOffset(0))       */
Type  listof;                    /* [ mkOffset(0) ]                 */
Type  typeVarToVar;              /* mkOffset(0) -> mkOffset(0)      */

Cell  predNum;                   /* Num (mkOffset(0))               */
Cell  predFractional;            /* Fractional (mkOffset(0))        */
Cell  predIntegral;              /* Integral (mkOffset(0))          */
Kind  starToStar;                /* Type -> Type                    */
Cell  predMonad;                 /* Monad (mkOffset(0))             */
Type  typeProgIO;                /* IO a                            */


/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

static Tycon linkTycon ( String s );
static Tycon linkClass ( String s );
static Name  linkName  ( String s );
static Name  predefinePrim ( String s );


static Tycon linkTycon( String s )
{
    Tycon tc = findTycon(findText(s));
    if (nonNull(tc)) return tc;
    if (combined) {
       tc = findTyconInAnyModule(findText(s));
       if (nonNull(tc)) return tc;
    }
FPrintf(stderr, "frambozenvla!  unknown tycon %s\n", s );
return NIL;
    ERRMSG(0) "Prelude does not define standard type \"%s\"", s
    EEND;
}

static Class linkClass( String s )
{
    Class cc = findClass(findText(s));
    if (nonNull(cc)) return cc;
    if (combined) {
       cc = findClassInAnyModule(findText(s));
       if (nonNull(cc)) return cc;
    }   
FPrintf(stderr, "frambozenvla!  unknown class %s\n", s );
return NIL;
    ERRMSG(0) "Prelude does not define standard class \"%s\"", s
    EEND;
}

static Name linkName( String s )
{
    Name n = findName(findText(s));
    if (nonNull(n)) return n;
    if (combined) {
       n = findNameInAnyModule(findText(s));
       if (nonNull(n)) return n;
    }   
FPrintf(stderr, "frambozenvla!  unknown  name %s\n", s );
return NIL;
    ERRMSG(0) "Prelude does not define standard name \"%s\"", s
    EEND;
}

static Name predefinePrim ( String s )
{
    Name nm;
    Text t = findText(s);
    nm = findName(t);
    if (nonNull(nm)) {
       //fprintf(stderr, "predefinePrim: %s already exists\n", s );
    } else {
       nm = newName(t,NIL);
       name(nm).defn=PREDEFINED;
    }
    return nm;
}


/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

/* In standalone mode, linkPreludeTC, linkPreludeCM and linkPrimNames
   are called, in that order, during static analysis of Prelude.hs.
   In combined mode such an analysis does not happen.  Instead these
   calls will be made as a result of a call link(POSTPREL).

   linkPreludeTC, linkPreludeCM and linkPreludeNames are needed in both
   standalone and combined modes.
*/


Void linkPreludeTC(void) {              /* Hook to tycons and classes in   */
    static Bool initialised = FALSE;    /* prelude when first loaded       */
    if (!initialised) {
        Int i;
        initialised = TRUE;
	if (combined) {
	  setCurrModule(modulePrelude);
	} else {
	  setCurrModule(modulePrelPrim);
	}

        typeChar                 = linkTycon("Char");
        typeInt                  = linkTycon("Int");
        typeInteger              = linkTycon("Integer");
        typeWord                 = linkTycon("Word");
        typeAddr                 = linkTycon("Addr");
        typePrimArray            = linkTycon("PrimArray");
        typePrimByteArray        = linkTycon("PrimByteArray");
        typeRef                  = linkTycon("STRef");
        typePrimMutableArray     = linkTycon("PrimMutableArray");
        typePrimMutableByteArray = linkTycon("PrimMutableByteArray");
        typeFloat                = linkTycon("Float");
        typeDouble               = linkTycon("Double");
        typeStable               = linkTycon("StablePtr");
#       ifdef PROVIDE_WEAK
        typeWeak                 = linkTycon("Weak");
#       endif
#       ifdef PROVIDE_FOREIGN
        typeForeign              = linkTycon("ForeignObj");
#       endif
        typeThreadId             = linkTycon("ThreadId");
        typeMVar                 = linkTycon("MVar");
        typeBool                 = linkTycon("Bool");
        typeST                   = linkTycon("ST");
        typeIO                   = linkTycon("IO");
        typeException            = linkTycon("Exception");
        typeString               = linkTycon("String");
        typeOrdering             = linkTycon("Ordering");

        classEq                  = linkClass("Eq");
        classOrd                 = linkClass("Ord");
        classIx                  = linkClass("Ix");
        classEnum                = linkClass("Enum");
        classShow                = linkClass("Show");
        classRead                = linkClass("Read");
        classBounded             = linkClass("Bounded");
        classReal                = linkClass("Real");
        classIntegral            = linkClass("Integral");
        classRealFrac            = linkClass("RealFrac");
        classRealFloat           = linkClass("RealFloat");
        classFractional          = linkClass("Fractional");
        classFloating            = linkClass("Floating");
        classNum                 = linkClass("Num");
        classMonad               = linkClass("Monad");

        stdDefaults              = NIL;
        stdDefaults              = cons(typeDouble,stdDefaults);
        stdDefaults              = cons(typeInteger,stdDefaults);

        predNum                  = ap(classNum,aVar);
        predFractional           = ap(classFractional,aVar);
        predIntegral             = ap(classIntegral,aVar);
        predMonad                = ap(classMonad,aVar);
	typeProgIO               = ap(typeIO,aVar);

        nameMkC                  = addPrimCfunREP(findText("C#"),1,0,CHAR_REP);
        nameMkI                  = addPrimCfunREP(findText("I#"),1,0,INT_REP);
        nameMkW                  = addPrimCfunREP(findText("W#"),1,0,WORD_REP);
        nameMkA                  = addPrimCfunREP(findText("A#"),1,0,ADDR_REP);
        nameMkF                  = addPrimCfunREP(findText("F#"),1,0,FLOAT_REP);
        nameMkD                  = addPrimCfunREP(findText("D#"),1,0,DOUBLE_REP);
        nameMkStable             = addPrimCfunREP(findText("Stable#"),1,0,STABLE_REP);
        nameMkThreadId           = addPrimCfunREP(findText("ThreadId#"),1,0,THREADID_REP);

#       ifdef PROVIDE_FOREIGN
        nameMkForeign            = addPrimCfunREP(findText("Foreign#"),1,0,0);
#       endif
#       ifdef PROVIDE_WEAK
        nameMkWeak               = addPrimCfunREP(findText("Weak#"),1,0,0);
#       endif
        nameMkPrimArray          = addPrimCfunREP(findText("PrimArray#"),1,0,0);
        nameMkPrimByteArray      = addPrimCfunREP(findText("PrimByteArray#"),1,0,0);
        nameMkRef                = addPrimCfunREP(findText("STRef#"),1,0,0);
        nameMkPrimMutableArray   = addPrimCfunREP(findText("PrimMutableArray#"),1,0,0);
        nameMkPrimMutableByteArray = addPrimCfunREP(findText("PrimMutableByteArray#"),1,0,0);
        nameMkPrimMVar           = addPrimCfunREP(findText("MVar#"),1,0,0);
        nameMkInteger            = addPrimCfunREP(findText("Integer#"),1,0,0);

        if (!combined) {
           name(namePrimSeq).type   = primType(MONAD_Id, "ab", "b");
           name(namePrimCatch).type = primType(MONAD_Id, "aH", "a");
           name(namePrimRaise).type = primType(MONAD_Id, "E", "a");

           /* This is a lie.  For a more accurate type of primTakeMVar
              see ghc/interpreter/lib/Prelude.hs.
	   */
           name(namePrimTakeMVar).type = primType(MONAD_Id, "rbc", "d");
        }

        if (!combined) {
           for (i=2; i<=NUM_DTUPLES; i++) {/* Add derived instances of tuples */
               addTupInst(classEq,i);
               addTupInst(classOrd,i);
               addTupInst(classIx,i);
               addTupInst(classShow,i);
               addTupInst(classRead,i);
               addTupInst(classBounded,i);
           }
        }
    }
}

Void linkPreludeCM(void) {              /* Hook to cfuns and mfuns in      */
    static Bool initialised = FALSE;    /* prelude when first loaded       */
    if (!initialised) {
        Int i;
        initialised = TRUE;

	if (combined) {
	  setCurrModule(modulePrelude);
	} else {
	  setCurrModule(modulePrelPrim);
	}

        /* constructors */
        nameFalse        = linkName("False");
        nameTrue         = linkName("True");

        /* members */
        nameEq           = linkName("==");
        nameFromInt      = linkName("fromInt");
        nameFromInteger  = linkName("fromInteger");
        nameReturn       = linkName("return");
        nameBind         = linkName(">>=");
	nameMFail        = linkName("fail");
        nameLe           = linkName("<=");
        nameGt           = linkName(">");
        nameShowsPrec    = linkName("showsPrec");
        nameReadsPrec    = linkName("readsPrec");
        nameEQ           = linkName("EQ");
        nameCompare      = linkName("compare");
        nameMinBnd       = linkName("minBound");
        nameMaxBnd       = linkName("maxBound");
        nameRange        = linkName("range");
        nameIndex        = linkName("index");
        namePlus         = linkName("+");
        nameMult         = linkName("*");
        nameRangeSize    = linkName("rangeSize");
        nameInRange      = linkName("inRange");
        nameMinus        = linkName("-");
        /* These come before calls to implementPrim */
        if (!combined) {
           for(i=0; i<NUM_TUPLES; ++i) {
               if (i != 1) implementTuple(i);
           }
        }
    }
}

Void linkPrimNames ( void ) {        /* Hook to names defined in Prelude */
    static Bool initialised = FALSE;

    if (!initialised) {
        initialised = TRUE;

	if (combined) {
	  setCurrModule(modulePrelude);
	} else {
	  setCurrModule(modulePrelPrim);
	}

        /* primops */
        nameMkIO           = linkName("hugsprimMkIO");

        if (!combined) {
	  Int i;
	  for (i=0; asmPrimOps[i].name; ++i) {
	    Text t = findText(asmPrimOps[i].name);
	    Name n = findName(t);
	    if (isNull(n)) {
	      n = newName(t,NIL);
	      name(n).line   = 0;
	      name(n).defn   = NIL;
	      name(n).type   = primType(asmPrimOps[i].monad,
					asmPrimOps[i].args,
					asmPrimOps[i].results);
	      name(n).arity  = strlen(asmPrimOps[i].args);
	      name(n).primop = &(asmPrimOps[i]);
	      implementPrim(n);
	    } else {
	      ERRMSG(0) "Link Error in Prelude, surplus definition of \"%s\"", 
				asmPrimOps[i].name
              EEND;	      
	      // Name already defined!
	    }
	  }
        }

        /* static(tidyInfix)                        */
        nameNegate         = linkName("negate");
        /* user interface                           */
        nameRunIO_toplevel = linkName("hugsprimRunIO_toplevel");
        nameShow           = linkName("show");
        namePutStr         = linkName("putStr");
        namePrint          = linkName("print");
        /* desugar                                  */
        nameOtherwise      = linkName("otherwise");
        nameUndefined      = linkName("undefined");
        /* pmc                                      */
        namePmSub          = linkName("hugsprimPmSub");
        /* translator                               */
        nameEqChar         = linkName("hugsprimEqChar");
        nameCreateAdjThunk = linkName("hugsprimCreateAdjThunk");
        namePmInt          = linkName("hugsprimPmInt");
        namePmInteger      = linkName("hugsprimPmInteger");
        namePmDouble       = linkName("hugsprimPmDouble");

        nameFromDouble     = linkName("fromDouble");
        namePmFromInteger = linkName("hugsprimPmFromInteger");

        namePmSubtract    = linkName("hugsprimPmSubtract");
        namePmLe          = linkName("hugsprimPmLe");

        if (!combined) {
           implementCfun ( nameCons, NIL );
           implementCfun ( nameNil, NIL );
           implementCfun ( nameUnit, NIL );
        }
    }
}


/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

/* ToDo: fix pFun (or eliminate its use) */
#define pFun(n,s) n = predefinePrim(s)

Void linkControl(what)
Int what; {
    Int i;
    switch (what) {
      //case EXIT : fooble();break;
        case RESET   :
        case MARK    : 
                       break;

        case POSTPREL: {
           Name nm;
           Module modulePrelBase = findModule(findText("PrelBase"));
           assert(nonNull(modulePrelBase));
	   /* fprintf(stderr, "linkControl(POSTPREL)\n"); */
	   setCurrModule(modulePrelude);
           linkPreludeTC();
           linkPreludeCM();
           linkPrimNames();
           fixupRTStoPreludeRefs ( lookupObjName );

           nameUnpackString = linkName("hugsprimUnpackString");
           namePMFail       = linkName("hugsprimPmFail");
assert(nonNull(namePMFail));
#define xyzzy(aaa,bbb) aaa = linkName(bbb)


               /* pmc                                   */
               pFun(nameSel,            "_SEL");

               /* strict constructors                   */
               xyzzy(nameFlip,           "flip"     );

               /* parser                                */
               xyzzy(nameFromTo,         "enumFromTo");
               xyzzy(nameFromThenTo,     "enumFromThenTo");
               xyzzy(nameFrom,           "enumFrom");
               xyzzy(nameFromThen,       "enumFromThen");

               /* deriving                              */
               xyzzy(nameApp,            "++");
               xyzzy(nameReadField,      "hugsprimReadField");
               xyzzy(nameReadParen,      "readParen");
               xyzzy(nameShowField,      "hugsprimShowField");
               xyzzy(nameShowParen,      "showParen");
               xyzzy(nameLex,            "lex");
               xyzzy(nameComp,           ".");
               xyzzy(nameAnd,            "&&");
               xyzzy(nameCompAux,        "hugsprimCompAux");
               xyzzy(nameMap,            "map");

               /* implementTagToCon                     */
               xyzzy(nameError,          "hugsprimError");


           typeStable = linkTycon("Stable");
           typeRef    = linkTycon("IORef");
           // {Prim,PrimByte,PrimMutable,PrimMutableByte}Array ?

           ifLinkConstrItbl ( nameFalse );
           ifLinkConstrItbl ( nameTrue );
           ifLinkConstrItbl ( nameNil );
           ifLinkConstrItbl ( nameCons );

           /* PrelErr.hi doesn't give a type for error, alas.  
              So error never appears in any symbol table.
              So we fake it by copying the table entry for
              hugsprimError -- which is just a call to error.
              Although we put it on the Prelude export list, we
              have to claim internally that it lives in PrelErr, 
              so that the correct symbol (PrelErr_error_closure)
              is referred to.
              Big Big Sigh.
           */
           nm            = newName ( findText("error"), NIL );
           name(nm)      = name(nameError);
           name(nm).mod  = findModule(findText("PrelErr"));
           name(nm).text = findText("error");
           setCurrModule(modulePrelude);
           module(modulePrelude).exports
              = cons ( nm, module(modulePrelude).exports );

           /* The GHC prelude doesn't seem to export Addr.  Add it to the
              export list for the sake of compatibility with standalone mode.
	   */
           module(modulePrelude).exports
              = cons ( pair(typeAddr,DOTDOT), 
                       module(modulePrelude).exports );
           addTycon(typeAddr);

           /* Make nameListMonad be the builder fn for instance Monad [].
              Standalone hugs does this with a disgusting hack in 
              checkInstDefn() in static.c.  We have a slightly different
              disgusting hack for the combined case.
           */
           {
           Class cm;   /* :: Class   */
           List  is;   /* :: [Inst]  */
           cm = findClassInAnyModule(findText("Monad"));
           assert(nonNull(cm));
           is = cclass(cm).instances;
           assert(nonNull(is));
           while (nonNull(is) && snd(inst(hd(is)).head) != typeList)
              is = tl(is);
           assert(nonNull(is));
           nameListMonad = inst(hd(is)).builder;
           assert(nonNull(nameListMonad));
           }

           break;
        }
        case PREPREL : 

           if (combined) {
               Module modulePrelBase;

               modulePrelude = findFakeModule(textPrelude);

               nameMkC = addWiredInBoxingTycon("PrelBase", "Char",  "C#",
                                               CHAR_REP,   STAR );
               nameMkI = addWiredInBoxingTycon("PrelBase", "Int",   "I#",
                                               INT_REP,    STAR );
               nameMkW = addWiredInBoxingTycon("PrelAddr", "Word",  "W#",
                                               WORD_REP,   STAR );
               nameMkA = addWiredInBoxingTycon("PrelAddr", "Addr",  "A#",
                                               ADDR_REP,   STAR );
               nameMkF = addWiredInBoxingTycon("PrelFloat","Float", "F#",
                                               FLOAT_REP,  STAR );
               nameMkD = addWiredInBoxingTycon("PrelFloat","Double","D#",
                                               DOUBLE_REP, STAR );
               nameMkInteger            
                       = addWiredInBoxingTycon("PrelNum","Integer","Integer#",
                                               0 ,STAR );
               nameMkPrimByteArray      
                       = addWiredInBoxingTycon("PrelGHC","ByteArray",
                                               "PrimByteArray#",0 ,STAR );

               for (i=0; i<NUM_TUPLES; ++i) {
                   if (i != 1) addTupleTycon(i);
               }
	       addWiredInEnumTycon("PrelBase","Bool",
                                   doubleton(findText("False"),
                                             findText("True")));

               //nameMkThreadId
               //   = addWiredInBoxingTycon("PrelConc","ThreadId","ThreadId#"
               //                           ,1,0,THREADID_REP);

               setCurrModule(modulePrelude);

               typeArrow = addPrimTycon(findText("(->)"),
                                        pair(STAR,pair(STAR,STAR)),
                                        2,DATATYPE,NIL);

               /* desugaring                            */
               pFun(nameInd,            "_indirect");
               name(nameInd).number = DFUNNAME;

               /* newtype and USE_NEWTYPE_FOR_DICTS     */
               /* make a name entry for PrelBase.id _before_ loading Prelude
                  since ifSetClassDefaultsAndDCon() may need to refer to
                  nameId. 
               */
               modulePrelBase = findModule(findText("PrelBase"));
               module(modulePrelBase).objectExtraNames 
                  = singleton(findText("libHSstd_cbits"));

               setCurrModule(modulePrelBase);
               pFun(nameId,             "id");
               setCurrModule(modulePrelude);

           } else {
               fixupRTStoPreludeRefs(NULL);

               modulePrelPrim = findFakeModule(textPrelPrim);
               modulePrelude = findFakeModule(textPrelude);
               setCurrModule(modulePrelPrim);
        
               for (i=0; i<NUM_TUPLES; ++i) {
                   if (i != 1) addTupleTycon(i);
               }
               setCurrModule(modulePrelPrim);

               typeArrow = addPrimTycon(findText("(->)"),
                                        pair(STAR,pair(STAR,STAR)),
                                        2,DATATYPE,NIL);

               /* newtype and USE_NEWTYPE_FOR_DICTS     */
               pFun(nameId,             "id");

               /* desugaring                            */
               pFun(nameInd,            "_indirect");
               name(nameInd).number = DFUNNAME;

               /* pmc                                   */
               pFun(nameSel,            "_SEL");

               /* strict constructors                   */
               pFun(nameFlip,           "flip"     );

               /* parser                                */
               pFun(nameFromTo,         "enumFromTo");
               pFun(nameFromThenTo,     "enumFromThenTo");
               pFun(nameFrom,           "enumFrom");
               pFun(nameFromThen,       "enumFromThen");

               /* deriving                              */
               pFun(nameApp,            "++");
               pFun(nameReadField,      "hugsprimReadField");
               pFun(nameReadParen,      "readParen");
               pFun(nameShowField,      "hugsprimShowField");
               pFun(nameShowParen,      "showParen");
               pFun(nameLex,            "lex");
               pFun(nameComp,           ".");
               pFun(nameAnd,            "&&");
               pFun(nameCompAux,        "hugsprimCompAux");
               pFun(nameMap,            "map");

               /* implementTagToCon                     */
               pFun(namePMFail,         "hugsprimPmFail");
               pFun(nameError,          "error");
               pFun(nameUnpackString,   "hugsprimUnpackString");

	       /* assertion and exception issues */
	       pFun(nameAssert,	        "assert");
	       pFun(nameAssertError,	"assertError");
	       pFun(nameTangleMessage,	"tangleMessager");
	       pFun(nameIrrefutPatError,	
		                        "irrefutPatError");
	       pFun(nameNoMethodBindingError,
		                        "noMethodBindingError");
	       pFun(nameNonExhaustiveGuardsError,
		                        "nonExhaustiveGuardsError");
	       pFun(namePatError,	"patError");
	       pFun(nameRecSelError,	"recSelError");
	       pFun(nameRecConError,	"recConError");
	       pFun(nameRecUpdError,	"recUpdError");

               /* hooks for handwritten bytecode */
               pFun(namePrimSeq,        "primSeq");
               pFun(namePrimCatch,      "primCatch");
               pFun(namePrimRaise,      "primRaise");
               pFun(namePrimTakeMVar,   "primTakeMVar");
               {
                  Name n          = namePrimSeq;
                  name(n).line    = 0;
                  name(n).arity   = 1;
                  name(n).type    = NIL;
                  name(n).closure = mkCPtr ( asm_BCO_seq() );
                  addToCodeList ( modulePrelPrim, n );
               }
               {
                  Name n          = namePrimCatch;
                  name(n).line    = 0;
                  name(n).arity   = 2;
                  name(n).type    = NIL;
                  name(n).closure = mkCPtr ( asm_BCO_catch() );
                  addToCodeList ( modulePrelPrim, n );
               }
               {
                  Name n          = namePrimRaise;
                  name(n).line    = 0;
                  name(n).arity   = 1;
                  name(n).type    = NIL;
                  name(n).closure = mkCPtr ( asm_BCO_raise() );
                  addToCodeList ( modulePrelPrim, n );
               }
               {
                  Name n          = namePrimTakeMVar;
                  name(n).line    = 0;
                  name(n).arity   = 2;
                  name(n).type    = NIL;
                  name(n).closure = mkCPtr ( asm_BCO_takeMVar() );
                  addToCodeList ( modulePrelPrim, n );
               }
	   }
           break;
    }
}
#undef pFun

/*-------------------------------------------------------------------------*/
