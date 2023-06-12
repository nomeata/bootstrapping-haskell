/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 17 "parser.y"

#ifndef lint
#define lint
#endif
#define defTycon(n,l,lhs,rhs,w)	 tyconDefn(intOf(l),lhs,rhs,w); sp-=n
#define sigdecl(l,vs,t)		 ap(SIGDECL,triple(l,vs,t))
#define fixdecl(l,ops,a,p)	 ap(FIXDECL,\
				    triple(l,ops,mkInt(mkSyntax(a,intOf(p)))))
#define grded(gs)		 ap(GUARDED,gs)
#define bang(t)			 ap(BANG,t)
#define only(t)			 ap(ONLY,t)
#define letrec(bs,e)		 (nonNull(bs) ? ap(LETREC,pair(bs,e)) : e)
#define qualify(ps,t)		 (nonNull(ps) ? ap(QUAL,pair(ps,t)) : t)
#define exportSelf()		 singleton(ap(MODULEENT,mkCon(module(currentModule).text)))
#define yyerror(s)		 /* errors handled elsewhere */
#define YYSTYPE			 Cell

#ifdef YYBISON
# if !defined(__GNUC__) || __GNUC__ <= 1
static void __yy_memcpy Args((char*,char*, unsigned int));
# endif
#endif

#ifdef _MANAGED
static void yymemcpy (char *yyto, const char *yyfrom, size_t yycount);
#endif

static Cell   local gcShadow	 Args((Int,Cell));
static Void   local syntaxError	 Args((String));
static String local unexpected	 Args((Void));
static Cell   local checkPrec	 Args((Cell));
static Cell   local buildTuple	 Args((List));
static List   local checkCtxt	 Args((List));
static Cell   local checkPred	 Args((Cell));
static Pair   local checkDo	 Args((List));
static Cell   local checkTyLhs	 Args((Cell));
static Cell   local checkConstr	 Args((Cell));

#if MUDO
static Pair   local checkMDo	 Args((List));
#endif

#if !TREX
static Void   local noTREX	 Args((String));
#endif
#if !IPARAM
static Void   local noIP	 Args((String));
#endif
#if !MUDO
static Void   local noMDo	 Args((String));
#endif

/* For the purposes of reasonably portable garbage collection, it is
 * necessary to simulate the YACC stack on the Hugs stack to keep
 * track of all intermediate constructs.  The lexical analyser
 * pushes a token onto the stack for each token that is found, with
 * these elements being removed as reduce actions are performed,
 * taking account of look-ahead tokens as described by gcShadow()
 * below.
 *
 * Of the non-terminals used below, only start, topDecl & begin
 * do not leave any values on the Hugs stack.  The same is true for the
 * terminals EXPR and SCRIPT.  At the end of a successful parse, there
 * should only be one element left on the stack, containing the result
 * of the parse.
 */

#define gc0(e)			gcShadow(0,e)
#define gc1(e)			gcShadow(1,e)
#define gc2(e)			gcShadow(2,e)
#define gc3(e)			gcShadow(3,e)
#define gc4(e)			gcShadow(4,e)
#define gc5(e)			gcShadow(5,e)
#define gc6(e)			gcShadow(6,e)
#define gc7(e)			gcShadow(7,e)


#line 149 "y.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    EXPR = 258,                    /* EXPR  */
    CTXT = 259,                    /* CTXT  */
    SCRIPT = 260,                  /* SCRIPT  */
    CASEXP = 261,                  /* CASEXP  */
    OF = 262,                      /* OF  */
    DATA = 263,                    /* DATA  */
    TYPE = 264,                    /* TYPE  */
    IF = 265,                      /* IF  */
    THEN = 266,                    /* THEN  */
    ELSE = 267,                    /* ELSE  */
    WHERE = 268,                   /* WHERE  */
    LET = 269,                     /* LET  */
    IN = 270,                      /* IN  */
    INFIXN = 271,                  /* INFIXN  */
    INFIXL = 272,                  /* INFIXL  */
    INFIXR = 273,                  /* INFIXR  */
    PRIMITIVE = 274,               /* PRIMITIVE  */
    TNEWTYPE = 275,                /* TNEWTYPE  */
    DEFAULT = 276,                 /* DEFAULT  */
    DERIVING = 277,                /* DERIVING  */
    DO = 278,                      /* DO  */
    TCLASS = 279,                  /* TCLASS  */
    TINSTANCE = 280,               /* TINSTANCE  */
    MDO = 281,                     /* MDO  */
    REPEAT = 282,                  /* REPEAT  */
    ALL = 283,                     /* ALL  */
    NUMLIT = 284,                  /* NUMLIT  */
    CHARLIT = 285,                 /* CHARLIT  */
    STRINGLIT = 286,               /* STRINGLIT  */
    VAROP = 287,                   /* VAROP  */
    VARID = 288,                   /* VARID  */
    CONOP = 289,                   /* CONOP  */
    CONID = 290,                   /* CONID  */
    QVAROP = 291,                  /* QVAROP  */
    QVARID = 292,                  /* QVARID  */
    QCONOP = 293,                  /* QCONOP  */
    QCONID = 294,                  /* QCONID  */
    RECSELID = 295,                /* RECSELID  */
    IPVARID = 296,                 /* IPVARID  */
    COCO = 297,                    /* COCO  */
    UPTO = 298,                    /* UPTO  */
    FROM = 299,                    /* FROM  */
    ARROW = 300,                   /* ARROW  */
    IMPLIES = 301,                 /* IMPLIES  */
    TMODULE = 302,                 /* TMODULE  */
    IMPORT = 303,                  /* IMPORT  */
    HIDING = 304,                  /* HIDING  */
    QUALIFIED = 305,               /* QUALIFIED  */
    ASMOD = 306,                   /* ASMOD  */
    NEEDPRIMS = 307,               /* NEEDPRIMS  */
    FOREIGN = 308                  /* FOREIGN  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define EXPR 258
#define CTXT 259
#define SCRIPT 260
#define CASEXP 261
#define OF 262
#define DATA 263
#define TYPE 264
#define IF 265
#define THEN 266
#define ELSE 267
#define WHERE 268
#define LET 269
#define IN 270
#define INFIXN 271
#define INFIXL 272
#define INFIXR 273
#define PRIMITIVE 274
#define TNEWTYPE 275
#define DEFAULT 276
#define DERIVING 277
#define DO 278
#define TCLASS 279
#define TINSTANCE 280
#define MDO 281
#define REPEAT 282
#define ALL 283
#define NUMLIT 284
#define CHARLIT 285
#define STRINGLIT 286
#define VAROP 287
#define VARID 288
#define CONOP 289
#define CONID 290
#define QVAROP 291
#define QVARID 292
#define QCONOP 293
#define QCONID 294
#define RECSELID 295
#define IPVARID 296
#define COCO 297
#define UPTO 298
#define FROM 299
#define ARROW 300
#define IMPLIES 301
#define TMODULE 302
#define IMPORT 303
#define HIDING 304
#define QUALIFIED 305
#define ASMOD 306
#define NEEDPRIMS 307
#define FOREIGN 308

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);



/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_EXPR = 3,                       /* EXPR  */
  YYSYMBOL_CTXT = 4,                       /* CTXT  */
  YYSYMBOL_SCRIPT = 5,                     /* SCRIPT  */
  YYSYMBOL_CASEXP = 6,                     /* CASEXP  */
  YYSYMBOL_OF = 7,                         /* OF  */
  YYSYMBOL_DATA = 8,                       /* DATA  */
  YYSYMBOL_TYPE = 9,                       /* TYPE  */
  YYSYMBOL_IF = 10,                        /* IF  */
  YYSYMBOL_THEN = 11,                      /* THEN  */
  YYSYMBOL_ELSE = 12,                      /* ELSE  */
  YYSYMBOL_WHERE = 13,                     /* WHERE  */
  YYSYMBOL_LET = 14,                       /* LET  */
  YYSYMBOL_IN = 15,                        /* IN  */
  YYSYMBOL_INFIXN = 16,                    /* INFIXN  */
  YYSYMBOL_INFIXL = 17,                    /* INFIXL  */
  YYSYMBOL_INFIXR = 18,                    /* INFIXR  */
  YYSYMBOL_PRIMITIVE = 19,                 /* PRIMITIVE  */
  YYSYMBOL_TNEWTYPE = 20,                  /* TNEWTYPE  */
  YYSYMBOL_DEFAULT = 21,                   /* DEFAULT  */
  YYSYMBOL_DERIVING = 22,                  /* DERIVING  */
  YYSYMBOL_DO = 23,                        /* DO  */
  YYSYMBOL_TCLASS = 24,                    /* TCLASS  */
  YYSYMBOL_TINSTANCE = 25,                 /* TINSTANCE  */
  YYSYMBOL_MDO = 26,                       /* MDO  */
  YYSYMBOL_REPEAT = 27,                    /* REPEAT  */
  YYSYMBOL_ALL = 28,                       /* ALL  */
  YYSYMBOL_NUMLIT = 29,                    /* NUMLIT  */
  YYSYMBOL_CHARLIT = 30,                   /* CHARLIT  */
  YYSYMBOL_STRINGLIT = 31,                 /* STRINGLIT  */
  YYSYMBOL_VAROP = 32,                     /* VAROP  */
  YYSYMBOL_VARID = 33,                     /* VARID  */
  YYSYMBOL_CONOP = 34,                     /* CONOP  */
  YYSYMBOL_CONID = 35,                     /* CONID  */
  YYSYMBOL_QVAROP = 36,                    /* QVAROP  */
  YYSYMBOL_QVARID = 37,                    /* QVARID  */
  YYSYMBOL_QCONOP = 38,                    /* QCONOP  */
  YYSYMBOL_QCONID = 39,                    /* QCONID  */
  YYSYMBOL_RECSELID = 40,                  /* RECSELID  */
  YYSYMBOL_IPVARID = 41,                   /* IPVARID  */
  YYSYMBOL_COCO = 42,                      /* COCO  */
  YYSYMBOL_43_ = 43,                       /* '='  */
  YYSYMBOL_UPTO = 44,                      /* UPTO  */
  YYSYMBOL_45_ = 45,                       /* '@'  */
  YYSYMBOL_46_ = 46,                       /* '\\'  */
  YYSYMBOL_47_ = 47,                       /* '|'  */
  YYSYMBOL_48_ = 48,                       /* '-'  */
  YYSYMBOL_FROM = 49,                      /* FROM  */
  YYSYMBOL_ARROW = 50,                     /* ARROW  */
  YYSYMBOL_51_ = 51,                       /* '~'  */
  YYSYMBOL_52_ = 52,                       /* '!'  */
  YYSYMBOL_IMPLIES = 53,                   /* IMPLIES  */
  YYSYMBOL_54_ = 54,                       /* '('  */
  YYSYMBOL_55_ = 55,                       /* ','  */
  YYSYMBOL_56_ = 56,                       /* ')'  */
  YYSYMBOL_57_ = 57,                       /* '['  */
  YYSYMBOL_58_ = 58,                       /* ';'  */
  YYSYMBOL_59_ = 59,                       /* ']'  */
  YYSYMBOL_60_ = 60,                       /* '`'  */
  YYSYMBOL_61_ = 61,                       /* '.'  */
  YYSYMBOL_TMODULE = 62,                   /* TMODULE  */
  YYSYMBOL_IMPORT = 63,                    /* IMPORT  */
  YYSYMBOL_HIDING = 64,                    /* HIDING  */
  YYSYMBOL_QUALIFIED = 65,                 /* QUALIFIED  */
  YYSYMBOL_ASMOD = 66,                     /* ASMOD  */
  YYSYMBOL_NEEDPRIMS = 67,                 /* NEEDPRIMS  */
  YYSYMBOL_FOREIGN = 68,                   /* FOREIGN  */
  YYSYMBOL_69_ = 69,                       /* '{'  */
  YYSYMBOL_70_ = 70,                       /* '}'  */
  YYSYMBOL_71_ = 71,                       /* '#'  */
  YYSYMBOL_72___ = 72,                     /* '_'  */
  YYSYMBOL_73_ = 73,                       /* '+'  */
  YYSYMBOL_YYACCEPT = 74,                  /* $accept  */
  YYSYMBOL_start = 75,                     /* start  */
  YYSYMBOL_topModule = 76,                 /* topModule  */
  YYSYMBOL_startMain = 77,                 /* startMain  */
  YYSYMBOL_modname = 78,                   /* modname  */
  YYSYMBOL_modid = 79,                     /* modid  */
  YYSYMBOL_modBody = 80,                   /* modBody  */
  YYSYMBOL_expspec = 81,                   /* expspec  */
  YYSYMBOL_exports = 82,                   /* exports  */
  YYSYMBOL_export = 83,                    /* export  */
  YYSYMBOL_qnames = 84,                    /* qnames  */
  YYSYMBOL_qnames1 = 85,                   /* qnames1  */
  YYSYMBOL_qname = 86,                     /* qname  */
  YYSYMBOL_impDecls = 87,                  /* impDecls  */
  YYSYMBOL_chase = 88,                     /* chase  */
  YYSYMBOL_impDecl = 89,                   /* impDecl  */
  YYSYMBOL_impspec = 90,                   /* impspec  */
  YYSYMBOL_imports = 91,                   /* imports  */
  YYSYMBOL_imports1 = 92,                  /* imports1  */
  YYSYMBOL_import = 93,                    /* import  */
  YYSYMBOL_names = 94,                     /* names  */
  YYSYMBOL_names1 = 95,                    /* names1  */
  YYSYMBOL_name = 96,                      /* name  */
  YYSYMBOL_topDecls = 97,                  /* topDecls  */
  YYSYMBOL_topDecl = 98,                   /* topDecl  */
  YYSYMBOL_tyLhs = 99,                     /* tyLhs  */
  YYSYMBOL_invars = 100,                   /* invars  */
  YYSYMBOL_invar = 101,                    /* invar  */
  YYSYMBOL_constrs = 102,                  /* constrs  */
  YYSYMBOL_pconstr = 103,                  /* pconstr  */
  YYSYMBOL_qconstr = 104,                  /* qconstr  */
  YYSYMBOL_constr = 105,                   /* constr  */
  YYSYMBOL_btype3 = 106,                   /* btype3  */
  YYSYMBOL_bbtype = 107,                   /* bbtype  */
  YYSYMBOL_nconstr = 108,                  /* nconstr  */
  YYSYMBOL_fieldspecs = 109,               /* fieldspecs  */
  YYSYMBOL_fieldspec = 110,                /* fieldspec  */
  YYSYMBOL_deriving = 111,                 /* deriving  */
  YYSYMBOL_derivs0 = 112,                  /* derivs0  */
  YYSYMBOL_derivs = 113,                   /* derivs  */
  YYSYMBOL_prims = 114,                    /* prims  */
  YYSYMBOL_prim = 115,                     /* prim  */
  YYSYMBOL_crule = 116,                    /* crule  */
  YYSYMBOL_irule = 117,                    /* irule  */
  YYSYMBOL_dtypes = 118,                   /* dtypes  */
  YYSYMBOL_dtypes1 = 119,                  /* dtypes1  */
  YYSYMBOL_fds = 120,                      /* fds  */
  YYSYMBOL_fds1 = 121,                     /* fds1  */
  YYSYMBOL_fd = 122,                       /* fd  */
  YYSYMBOL_varids0 = 123,                  /* varids0  */
  YYSYMBOL_topType = 124,                  /* topType  */
  YYSYMBOL_topType0 = 125,                 /* topType0  */
  YYSYMBOL_topType1 = 126,                 /* topType1  */
  YYSYMBOL_polyType = 127,                 /* polyType  */
  YYSYMBOL_bpolyType = 128,                /* bpolyType  */
  YYSYMBOL_varids = 129,                   /* varids  */
  YYSYMBOL_sigType = 130,                  /* sigType  */
  YYSYMBOL_context = 131,                  /* context  */
  YYSYMBOL_lcontext = 132,                 /* lcontext  */
  YYSYMBOL_lacks = 133,                    /* lacks  */
  YYSYMBOL_lacks1 = 134,                   /* lacks1  */
  YYSYMBOL_type = 135,                     /* type  */
  YYSYMBOL_type1 = 136,                    /* type1  */
  YYSYMBOL_btype = 137,                    /* btype  */
  YYSYMBOL_btype1 = 138,                   /* btype1  */
  YYSYMBOL_btype2 = 139,                   /* btype2  */
  YYSYMBOL_atype = 140,                    /* atype  */
  YYSYMBOL_atype1 = 141,                   /* atype1  */
  YYSYMBOL_btypes2 = 142,                  /* btypes2  */
  YYSYMBOL_typeTuple = 143,                /* typeTuple  */
  YYSYMBOL_tfields = 144,                  /* tfields  */
  YYSYMBOL_tfield = 145,                   /* tfield  */
  YYSYMBOL_gendecl = 146,                  /* gendecl  */
  YYSYMBOL_optDigit = 147,                 /* optDigit  */
  YYSYMBOL_ops = 148,                      /* ops  */
  YYSYMBOL_vars = 149,                     /* vars  */
  YYSYMBOL_decls = 150,                    /* decls  */
  YYSYMBOL_decls0 = 151,                   /* decls0  */
  YYSYMBOL_decls1 = 152,                   /* decls1  */
  YYSYMBOL_decl = 153,                     /* decl  */
  YYSYMBOL_funlhs = 154,                   /* funlhs  */
  YYSYMBOL_funlhs0 = 155,                  /* funlhs0  */
  YYSYMBOL_funlhs1 = 156,                  /* funlhs1  */
  YYSYMBOL_rhs = 157,                      /* rhs  */
  YYSYMBOL_rhs1 = 158,                     /* rhs1  */
  YYSYMBOL_gdrhs = 159,                    /* gdrhs  */
  YYSYMBOL_gddef = 160,                    /* gddef  */
  YYSYMBOL_wherePart = 161,                /* wherePart  */
  YYSYMBOL_lwherePart = 162,               /* lwherePart  */
  YYSYMBOL_ldecls = 163,                   /* ldecls  */
  YYSYMBOL_ldecls0 = 164,                  /* ldecls0  */
  YYSYMBOL_ldecls1 = 165,                  /* ldecls1  */
  YYSYMBOL_ldecl = 166,                    /* ldecl  */
  YYSYMBOL_pat = 167,                      /* pat  */
  YYSYMBOL_pat_npk = 168,                  /* pat_npk  */
  YYSYMBOL_npk = 169,                      /* npk  */
  YYSYMBOL_pat0 = 170,                     /* pat0  */
  YYSYMBOL_pat0_INT = 171,                 /* pat0_INT  */
  YYSYMBOL_pat0_vI = 172,                  /* pat0_vI  */
  YYSYMBOL_infixPat = 173,                 /* infixPat  */
  YYSYMBOL_pat10 = 174,                    /* pat10  */
  YYSYMBOL_pat10_vI = 175,                 /* pat10_vI  */
  YYSYMBOL_fpat = 176,                     /* fpat  */
  YYSYMBOL_apat = 177,                     /* apat  */
  YYSYMBOL_apat_vI = 178,                  /* apat_vI  */
  YYSYMBOL_pats2 = 179,                    /* pats2  */
  YYSYMBOL_pats1 = 180,                    /* pats1  */
  YYSYMBOL_patbinds = 181,                 /* patbinds  */
  YYSYMBOL_patbinds1 = 182,                /* patbinds1  */
  YYSYMBOL_patbind = 183,                  /* patbind  */
  YYSYMBOL_patfields = 184,                /* patfields  */
  YYSYMBOL_patfield = 185,                 /* patfield  */
  YYSYMBOL_exp = 186,                      /* exp  */
  YYSYMBOL_exp_err = 187,                  /* exp_err  */
  YYSYMBOL_exp0 = 188,                     /* exp0  */
  YYSYMBOL_exp0a = 189,                    /* exp0a  */
  YYSYMBOL_exp0b = 190,                    /* exp0b  */
  YYSYMBOL_infixExpa = 191,                /* infixExpa  */
  YYSYMBOL_infixExpb = 192,                /* infixExpb  */
  YYSYMBOL_exp10a = 193,                   /* exp10a  */
  YYSYMBOL_exp10b = 194,                   /* exp10b  */
  YYSYMBOL_then_exp = 195,                 /* then_exp  */
  YYSYMBOL_else_exp = 196,                 /* else_exp  */
  YYSYMBOL_pats = 197,                     /* pats  */
  YYSYMBOL_appExp = 198,                   /* appExp  */
  YYSYMBOL_aexp = 199,                     /* aexp  */
  YYSYMBOL_exps2 = 200,                    /* exps2  */
  YYSYMBOL_vfields = 201,                  /* vfields  */
  YYSYMBOL_vfield = 202,                   /* vfield  */
  YYSYMBOL_alts = 203,                     /* alts  */
  YYSYMBOL_alts1 = 204,                    /* alts1  */
  YYSYMBOL_alt = 205,                      /* alt  */
  YYSYMBOL_altRhs = 206,                   /* altRhs  */
  YYSYMBOL_guardAlts = 207,                /* guardAlts  */
  YYSYMBOL_guardAlt = 208,                 /* guardAlt  */
  YYSYMBOL_stmts = 209,                    /* stmts  */
  YYSYMBOL_stmts1 = 210,                   /* stmts1  */
  YYSYMBOL_stmt = 211,                     /* stmt  */
  YYSYMBOL_fbinds = 212,                   /* fbinds  */
  YYSYMBOL_fbinds1 = 213,                  /* fbinds1  */
  YYSYMBOL_fbind = 214,                    /* fbind  */
  YYSYMBOL_list = 215,                     /* list  */
  YYSYMBOL_zipquals = 216,                 /* zipquals  */
  YYSYMBOL_quals = 217,                    /* quals  */
  YYSYMBOL_qual = 218,                     /* qual  */
  YYSYMBOL_gcon = 219,                     /* gcon  */
  YYSYMBOL_tupCommas = 220,                /* tupCommas  */
  YYSYMBOL_varid = 221,                    /* varid  */
  YYSYMBOL_qconid = 222,                   /* qconid  */
  YYSYMBOL_var = 223,                      /* var  */
  YYSYMBOL_qvar = 224,                     /* qvar  */
  YYSYMBOL_con = 225,                      /* con  */
  YYSYMBOL_qcon = 226,                     /* qcon  */
  YYSYMBOL_varop = 227,                    /* varop  */
  YYSYMBOL_varop_mi = 228,                 /* varop_mi  */
  YYSYMBOL_varop_pl = 229,                 /* varop_pl  */
  YYSYMBOL_varop_mipl = 230,               /* varop_mipl  */
  YYSYMBOL_qvarop = 231,                   /* qvarop  */
  YYSYMBOL_qvarop_mi = 232,                /* qvarop_mi  */
  YYSYMBOL_conop = 233,                    /* conop  */
  YYSYMBOL_qconop = 234,                   /* qconop  */
  YYSYMBOL_op = 235,                       /* op  */
  YYSYMBOL_qop = 236,                      /* qop  */
  YYSYMBOL_begin = 237,                    /* begin  */
  YYSYMBOL_end = 238                       /* end  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  60
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   4123

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  74
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  165
/* YYNRULES -- Number of rules.  */
#define YYNRULES  502
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  892

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   308


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    52,     2,    71,     2,     2,     2,     2,
      54,    56,     2,    73,    55,    48,    61,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    58,
       2,    43,     2,     2,    45,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    57,    46,    59,     2,    72,    60,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    69,    47,    70,    51,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    44,    49,
      50,    53,    62,    63,    64,    65,    66,    67,    68
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   120,   120,   121,   122,   123,   136,   140,   144,   146,
     148,   154,   157,   159,   160,   168,   169,   170,   171,   172,
     177,   178,   179,   180,   181,   183,   184,   189,   191,   192,
     193,   194,   195,   197,   198,   199,   200,   202,   203,   205,
     206,   211,   212,   213,   215,   226,   228,   231,   234,   237,
     239,   240,   241,   243,   244,   245,   246,   248,   249,   251,
     252,   253,   254,   256,   257,   258,   259,   261,   262,   264,
     265,   270,   271,   272,   273,   274,   279,   280,   283,   284,
     287,   291,   293,   296,   297,   300,   304,   305,   311,   313,
     314,   315,   317,   318,   320,   322,   324,   325,   327,   329,
     331,   332,   334,   335,   336,   337,   338,   339,   340,   341,
     342,   344,   345,   346,   347,   348,   349,   351,   352,   353,
     355,   357,   358,   360,   361,   362,   364,   365,   366,   368,
     369,   371,   372,   377,   379,   380,   381,   383,   384,   389,
     391,   393,   395,   397,   403,   404,   405,   406,   407,   408,
     410,   411,   413,   414,   416,   417,   419,   420,   422,   423,
     426,   427,   429,   430,   432,   433,   438,   440,   442,   443,
     445,   446,   447,   448,   450,   452,   454,   455,   457,   458,
     460,   461,   463,   464,   465,   466,   467,   468,   470,   471,
     473,   480,   488,   489,   490,   491,   492,   495,   496,   498,
     499,   500,   501,   502,   504,   505,   507,   508,   510,   511,
     513,   514,   516,   517,   518,   519,   520,   521,   522,   523,
     524,   531,   538,   539,   540,   543,   544,   546,   547,   548,
     549,   552,   553,   555,   562,   563,   564,   565,   566,   567,
     568,   569,   571,   572,   574,   575,   577,   578,   580,   581,
     583,   584,   585,   587,   589,   590,   591,   594,   596,   597,
     598,   600,   601,   602,   603,   604,   606,   607,   608,   609,
     610,   612,   613,   615,   616,   618,   619,   621,   623,   624,
     629,   630,   633,   634,   637,   638,   639,   642,   644,   651,
     652,   657,   658,   660,   661,   663,   665,   666,   667,   669,
     670,   672,   673,   675,   676,   677,   678,   679,   680,   681,
     682,   683,   684,   686,   687,   689,   690,   692,   693,   695,
     696,   697,   699,   700,   701,   702,   703,   704,   705,   706,
     707,   708,   709,   711,   718,   721,   722,   724,   725,   727,
     728,   730,   731,   733,   734,   737,   738,   740,   752,   753,
     755,   756,   758,   759,   761,   762,   764,   765,   767,   768,
     769,   770,   772,   774,   775,   776,   777,   779,   781,   782,
     783,   790,   792,   795,   796,   801,   802,   804,   805,   808,
     809,   811,   812,   814,   815,   816,   817,   818,   819,   820,
     821,   823,   824,   825,   826,   827,   828,   830,   837,   838,
     840,   841,   842,   843,   845,   846,   849,   850,   852,   861,
     862,   864,   865,   866,   868,   870,   871,   872,   874,   875,
     877,   880,   881,   883,   884,   885,   888,   889,   891,   893,
     894,   896,   897,   899,   900,   905,   906,   907,   923,   924,
     925,   926,   929,   930,   932,   933,   935,   936,   937,   942,
     943,   944,   945,   947,   948,   950,   951,   952,   953,   955,
     956,   958,   959,   960,   961,   962,   963,   965,   966,   967,
     969,   970,   972,   973,   974,   976,   977,   978,   980,   981,
     983,   984,   986,   987,   988,   989,   991,   992,   994,   995,
     996,   999,  1000,  1002,  1003,  1004,  1006,  1007,  1009,  1010,
    1015,  1018,  1019
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "EXPR", "CTXT",
  "SCRIPT", "CASEXP", "OF", "DATA", "TYPE", "IF", "THEN", "ELSE", "WHERE",
  "LET", "IN", "INFIXN", "INFIXL", "INFIXR", "PRIMITIVE", "TNEWTYPE",
  "DEFAULT", "DERIVING", "DO", "TCLASS", "TINSTANCE", "MDO", "REPEAT",
  "ALL", "NUMLIT", "CHARLIT", "STRINGLIT", "VAROP", "VARID", "CONOP",
  "CONID", "QVAROP", "QVARID", "QCONOP", "QCONID", "RECSELID", "IPVARID",
  "COCO", "'='", "UPTO", "'@'", "'\\\\'", "'|'", "'-'", "FROM", "ARROW",
  "'~'", "'!'", "IMPLIES", "'('", "','", "')'", "'['", "';'", "']'", "'`'",
  "'.'", "TMODULE", "IMPORT", "HIDING", "QUALIFIED", "ASMOD", "NEEDPRIMS",
  "FOREIGN", "'{'", "'}'", "'#'", "'_'", "'+'", "$accept", "start",
  "topModule", "startMain", "modname", "modid", "modBody", "expspec",
  "exports", "export", "qnames", "qnames1", "qname", "impDecls", "chase",
  "impDecl", "impspec", "imports", "imports1", "import", "names", "names1",
  "name", "topDecls", "topDecl", "tyLhs", "invars", "invar", "constrs",
  "pconstr", "qconstr", "constr", "btype3", "bbtype", "nconstr",
  "fieldspecs", "fieldspec", "deriving", "derivs0", "derivs", "prims",
  "prim", "crule", "irule", "dtypes", "dtypes1", "fds", "fds1", "fd",
  "varids0", "topType", "topType0", "topType1", "polyType", "bpolyType",
  "varids", "sigType", "context", "lcontext", "lacks", "lacks1", "type",
  "type1", "btype", "btype1", "btype2", "atype", "atype1", "btypes2",
  "typeTuple", "tfields", "tfield", "gendecl", "optDigit", "ops", "vars",
  "decls", "decls0", "decls1", "decl", "funlhs", "funlhs0", "funlhs1",
  "rhs", "rhs1", "gdrhs", "gddef", "wherePart", "lwherePart", "ldecls",
  "ldecls0", "ldecls1", "ldecl", "pat", "pat_npk", "npk", "pat0",
  "pat0_INT", "pat0_vI", "infixPat", "pat10", "pat10_vI", "fpat", "apat",
  "apat_vI", "pats2", "pats1", "patbinds", "patbinds1", "patbind",
  "patfields", "patfield", "exp", "exp_err", "exp0", "exp0a", "exp0b",
  "infixExpa", "infixExpb", "exp10a", "exp10b", "then_exp", "else_exp",
  "pats", "appExp", "aexp", "exps2", "vfields", "vfield", "alts", "alts1",
  "alt", "altRhs", "guardAlts", "guardAlt", "stmts", "stmts1", "stmt",
  "fbinds", "fbinds1", "fbind", "list", "zipquals", "quals", "qual",
  "gcon", "tupCommas", "varid", "qconid", "var", "qvar", "con", "qcon",
  "varop", "varop_mi", "varop_pl", "varop_mipl", "qvarop", "qvarop_mi",
  "conop", "qconop", "op", "qop", "begin", "end", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-744)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-472)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     431,  -744,  2382,   776,    13,    91,  -744,  2382,  2382,    44,
      90,    95,  -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,
    -744,  -744,   588,  2915,  3079,  2116,  2197,  -744,  -744,  -744,
    -744,   144,  -744,  -744,   131,  -744,   451,  -744,   451,  -744,
    3079,   102,  -744,  -744,  -744,   153,  -744,   107,  -744,  -744,
     164,  1181,  -744,  -744,  4021,   188,  -744,   190,  -744,   170,
    -744,   235,    24,  -744,   229,  2544,  2544,  -744,  -744,  -744,
     588,  3168,  3310,  -744,  -744,  -744,  3386,  -744,   234,   219,
    -744,  -744,   102,   294,   299,   308,   332,  2650,   346,  -744,
    -744,   467,   349,   358,   161,   451,   257,   254,  -744,   264,
     380,  -744,  -744,  2703,  -744,  2703,  -744,   152,   387,   398,
      44,  -744,  1695,  -744,  -744,  -744,  -744,  -744,  -744,  -744,
    -744,  -744,  -744,  -744,  2756,  2809,   102,   769,  3079,   769,
    2487,  -744,  -744,   329,  3758,   345,   951,  1775,  -744,  -744,
    -744,  -744,  -744,   909,  -744,   422,  -744,  2974,  2974,   417,
    2382,   477,    58,  1081,    77,  2382,    44,  2544,   442,    54,
     436,  -744,    54,  -744,   226,   294,   299,   332,   820,   346,
     349,   358,   452,   461,   463,   488,  -744,   226,   226,   588,
    -744,   374,   277,  -744,   588,   491,   175,  1636,  -744,  -744,
    -744,    38,  2382,  -744,   588,   769,  -744,  -744,  -744,  -744,
    -744,  -744,   483,   508,   514,   516,  -744,  -744,  2382,  -744,
    2597,  2382,  -744,  2382,   909,  -744,  -744,  -744,  2382,   485,
    -744,   530,  2264,  2439,  2382,   544,  -744,  -744,  -744,  1462,
     542,  -744,   540,  -744,  -744,  3866,  3786,  -744,   188,  2915,
    -744,  -744,  2915,  -744,  -744,   310,   525,   541,  -744,   555,
     556,   102,   532,  2474,  -744,  3871,  1386,  -744,  1386,  -744,
    1386,  -744,   547,  -744,   395,  1970,   399,   407,   342,  -744,
     424,   558,  -744,   545,  -744,  4057,   595,  1341,    71,   521,
     577,   640,   129,  1498,    64,  1556,  2278,   835,  3213,  2974,
     265,    72,  1402,   543,   552,  -744,   554,  -744,  -744,   194,
    -744,   176,  -744,   588,  -744,   109,   835,   835,  3123,    54,
    3348,  -744,  2382,  2382,   602,  -744,  -744,   122,  -744,  -744,
    -744,  -744,  -744,  -744,  -744,  -744,   229,  -744,  2382,  -744,
    2862,  -744,   296,  3424,  -744,  -744,   588,  -744,  3462,  -744,
    -744,  2487,  3500,  3538,  -744,  3462,  -744,  3462,   909,  -744,
    -744,  3462,   586,  3576,  3462,  -744,  -744,  -744,   546,   565,
    -744,   555,   579,  -744,  -744,  -744,  -744,  -744,  -744,  -744,
     568,  -744,   380,  -744,  -744,  -744,  -744,    44,   581,   571,
    -744,   587,  2439,   909,  2474,   580,   578,   542,   596,   598,
    3703,   454,   315,  2487,  2487,  2487,  -744,  2487,  -744,  -744,
    -744,  -744,   308,   584,  -744,   769,  2382,  -744,  -744,  -744,
    4021,  -744,  4021,  -744,  4021,  -744,  2487,  -744,  2487,  -744,
    2487,  -744,  2487,  -744,  2487,   909,  -744,  -744,  2487,  -744,
     222,   594,   997,   601,  -744,   443,   484,  -744,   607,   592,
    -744,    59,  -744,   611,  3802,    78,  -744,   304,  -744,  -744,
    1110,  -744,  1110,  -744,  1110,  -744,   347,   212,  -744,   636,
    -744,   615,  3906,  -744,  1957,  -744,   626,   622,  3911,  -744,
     666,   623,  3911,  -744,   946,  -744,  3462,  -744,   628,  3614,
     629,  3258,  -744,  -744,  -744,   443,   204,  -744,  -744,  -744,
     481,   481,  -744,    42,  -744,  3035,   905,   481,  -744,  2487,
    2382,  2703,  -744,   666,   635,  -744,  -744,  -744,  3462,  3462,
    -744,  3652,  -744,  3462,  -744,  -744,  3348,    70,    54,   644,
    -744,  -744,  -744,  2382,  -744,  2382,  -744,  -744,   588,  -744,
    -744,  -744,   588,  -744,   588,  -744,  -744,   630,  -744,   491,
    -744,  -744,   588,  -744,  -744,  -744,   769,  3462,  -744,   229,
    2382,  2439,  2325,   571,   660,  -744,   493,  3716,   529,  -744,
    2487,  1709,   641,  1709,   650,  -744,  -744,  -744,  -744,  -744,
    -744,  -744,  -744,  3871,  -744,  3871,  -744,   649,  -744,   558,
    -744,  -744,  -744,  1476,  -744,  1252,  -744,  -744,  2974,    86,
    1196,  2487,  -744,  1060,   652,  -744,  -744,  -744,   652,   652,
    3272,   481,  -744,    86,  1196,   662,   664,  -744,   181,   666,
     343,   651,  -744,   343,  -744,   227,   588,   588,   588,   285,
    1592,   668,   443,  -744,   373,   692,  3035,  -744,  -744,  -744,
    -744,   909,  -744,  -744,  -744,   680,   678,  -744,  3934,  3830,
    -744,   109,  -744,   690,  -744,  -744,  -744,  -744,   226,  -744,
    -744,   227,  -744,  -744,  -744,  2703,  2382,   666,   687,  -744,
    -744,  3462,  -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,
    -744,  -744,  -744,  -744,  1695,  -744,  -744,   685,  -744,  -744,
    -744,   686,  -744,   688,   694,  -744,  -744,  -744,    54,  -744,
     663,  -744,   909,  4021,  1818,    61,  -744,  -744,  3957,   115,
    3843,  3744,   674,   732,  1110,  -744,  -744,  1007,  -744,   729,
    -744,  2487,  -744,   700,  -744,   719,  -744,  4021,  -744,  -744,
    4021,  -744,  -744,  -744,   443,  -744,   704,  -744,   703,   706,
    -744,  -744,  1592,   116,   481,   883,   481,   554,   837,  4044,
    4044,  4044,  4044,  -744,  2382,   713,  -744,  -744,  -744,  -744,
    -744,  -744,  -744,  -744,  1298,  -744,  1196,  1108,   115,  4021,
    4021,   708,   189,  1196,  -744,  4021,  -744,  -744,   736,  3980,
    3980,  4021,  -744,  3980,   645,   481,  -744,  1196,  -744,  -744,
     181,  -744,  -744,  1383,    88,   116,  3657,  -744,  1515,   716,
    -744,   731,   481,  3272,   734,   737,  3985,  -744,  4008,  -744,
    -744,  -744,  -744,  2382,  -744,    61,  1238,  3980,   709,   343,
    -744,  -744,  -744,  4021,  -744,  -744,  -744,  -744,  -744,  -744,
    -744,   160,  -744,   221,  -744,   733,  -744,   750,   729,  -744,
     909,  -744,  -744,  -744,  -744,  -744,  -744,   740,  1044,  -744,
     742,   746,  -744,  -744,  -744,  -744,  -744,  3272,   766,  -744,
    3272,  3272,  -744,  -744,  -744,  1911,  -744,  -744,   759,  3680,
    -744,   760,   763,  -744,  -744,   481,  -744,  1321,   481,  3272,
    -744,  -744,  -744,  1653,  -744,  3272,  -744,  -744,  2036,  -744,
     343,  -744,  2487,  -744,  -744,  -744,  -744,  -744,  -744,  -744,
    -744,  -744
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     5,     0,     0,    11,     0,   349,     0,     0,     0,
       0,     0,   394,   391,   392,   393,   455,   470,   467,   472,
     399,   386,     0,     0,     0,     0,     0,   456,   457,   458,
     387,   280,   348,   351,   352,   353,   354,   356,   355,   357,
     371,   382,   388,   461,   469,   383,   474,   449,   460,   459,
       0,     0,     3,   186,   183,     0,   209,     0,     4,   500,
       1,     0,     0,   284,     0,     0,     0,   319,   325,   326,
       0,     0,     0,   327,   380,   321,     0,   323,   320,   449,
     360,   365,   385,   482,   491,   488,   493,     0,   484,   454,
     450,     0,   485,   478,     0,   355,     0,     0,   407,     0,
     461,   490,   479,     0,   495,     0,   451,   435,   436,     0,
       0,     2,     0,   482,   491,   488,   493,   486,   484,   485,
     478,   498,   487,   499,     0,     0,   381,   429,     0,   429,
       0,   182,   196,     0,     0,     0,     0,     0,   224,   208,
     210,   212,   211,     0,    10,    20,    12,    15,    15,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   428,     0,
     421,   425,     0,   332,   297,     0,     0,     0,     0,     0,
       0,     0,     0,   292,   291,   294,   298,   302,   301,   315,
     316,     0,     0,   346,   323,   461,   296,     0,   338,   292,
     291,     0,     0,   379,     0,   339,   462,   471,   468,   473,
     464,   465,     0,     0,     0,     0,   466,   463,     0,   395,
       0,     0,   396,     0,     0,   397,   453,   452,     0,     0,
     352,     0,     0,     0,     0,   437,   400,   281,   203,     0,
       0,   350,     0,   181,   197,   199,   198,   207,   212,     0,
     359,   364,     0,   362,   367,     0,     0,   430,   432,   433,
       0,   384,     0,     0,   191,   198,     0,   187,     0,   184,
       0,   185,     0,   213,     0,     0,     0,     0,     0,   232,
       0,   212,   223,     0,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   297,     0,    15,
       0,     0,     0,     0,    44,    43,    17,    74,   254,     0,
      75,     0,   258,   259,   260,     0,   302,   301,   296,     0,
       0,   376,     0,     0,     0,   374,   502,     0,   285,   501,
     290,   287,   282,   286,   283,   373,   427,   422,     0,   369,
     424,   370,     0,     0,   304,   303,   313,   314,     0,   328,
     329,     0,     0,     0,   317,     0,   330,     0,     0,   333,
     318,     0,     0,     0,     0,   331,   372,   322,     0,   340,
     342,   344,     0,   492,   489,   494,   483,   405,   401,   404,
       0,   406,     0,   408,   402,   403,   438,     0,   447,   443,
     445,   405,     0,     0,     0,   213,     0,   175,     0,   196,
       0,     0,   212,     0,     0,     0,   206,     0,   358,   363,
     361,   366,     0,     0,   390,     0,     0,   389,   188,   194,
     193,   195,   226,   192,   225,   214,     0,   215,     0,   216,
       0,   218,     0,   219,     0,     0,   220,   217,     0,   222,
     460,   472,     0,     0,    21,     0,     0,    26,     0,    27,
      29,     0,    83,     0,    81,    91,    90,     0,   235,   242,
       0,   237,     0,   239,     0,   136,     0,     0,   135,   138,
      86,     0,   183,   149,     0,   147,   158,     0,   151,   148,
     278,     0,   153,   476,     0,   475,     0,   477,     0,     0,
     291,   296,    16,    49,    14,     0,    50,    13,    88,    87,
       0,     0,     7,    44,    18,    71,     0,     0,   272,     0,
       0,     0,   255,   278,   274,   276,   270,   257,     0,     0,
     480,     0,   269,     0,   481,     6,     0,     0,     0,   409,
     413,   375,   378,     0,   289,     0,   426,   423,     0,   307,
     336,   293,     0,   311,     0,   309,   335,     0,   345,     0,
     347,   295,     0,   305,   337,   324,     0,     0,   398,   448,
       0,     0,     0,   442,     0,   179,     0,     0,     0,   176,
       0,     0,   216,     0,   218,   200,   180,   201,   202,   431,
     434,   227,   228,   226,   229,   225,   230,     0,   231,     0,
     233,    22,    32,     0,    23,    33,    28,     9,    15,     0,
       0,     0,    89,     0,   234,   496,   497,   245,   236,   238,
       0,     0,   137,     0,     0,     0,   155,   157,     0,   278,
       0,     0,   145,     0,   263,   296,     0,     0,   329,    50,
      53,     0,     0,    45,     0,     0,     0,    41,    72,    73,
     241,     0,   240,   167,   169,     0,     0,   173,   204,   205,
     246,     0,   273,     0,   271,   275,   262,   261,   295,   265,
     300,   299,   264,   410,   417,     0,     0,   278,   415,   419,
     368,   412,   377,   288,   308,   312,   310,   334,   306,   341,
     343,   446,   444,   441,     0,   178,   189,     0,   221,    24,
      25,     0,    34,     0,    35,    38,    39,    40,     0,    91,
      82,   110,     0,     0,     0,   126,    97,    99,   107,     0,
       0,   106,     0,    76,     0,   133,   134,     0,   120,   126,
     146,     0,   163,   159,   161,     0,   144,   150,   250,   279,
     152,   266,   267,   268,     0,    48,    60,    54,     0,    55,
      58,    59,    53,    50,     0,     0,     0,    19,     0,     0,
       0,     0,     0,   256,     0,     0,   416,   414,   418,   411,
     174,   177,    30,    31,    36,     8,     0,     0,     0,   204,
     205,     0,     0,     0,    79,     0,   115,   113,     0,     0,
       0,     0,   112,     0,     0,     0,   244,     0,    84,   156,
       0,   164,   165,     0,     0,    50,    63,    52,    56,     0,
      46,     0,     0,     0,     0,     0,     0,   170,   205,   168,
     171,   172,   277,     0,    37,   126,     0,     0,   116,   129,
     127,    96,   114,     0,   105,   119,   118,   103,   111,   104,
     109,     0,   122,     0,   247,    77,    93,    95,   126,   160,
     162,   251,   253,   248,   252,   249,    47,     0,     0,    64,
       0,    65,    68,    69,    70,    57,    51,     0,     0,   140,
       0,     0,   166,   420,    80,     0,    98,   101,     0,   106,
     102,     0,   130,   132,   117,     0,   108,     0,     0,     0,
      85,    61,    62,    66,   139,     0,   142,   143,     0,   128,
       0,   121,     0,   123,   124,    92,    94,    67,   141,   100,
     131,   125
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -744,  -744,  -744,  -744,  -744,  -422,  -139,  -744,  -744,   236,
    -744,  -744,    66,  -744,   331,   333,  -596,    93,  -744,    40,
    -744,  -744,   -44,   205,   337,  -504,  -744,   -32,    82,  -589,
    -744,  -739,  -744,  -696,    68,  -744,   -22,  -673,  -744,  -744,
    -744,   245,  -744,  -744,  -744,  -744,  -744,  -744,    74,    81,
    -553,    69,  -213,     1,  1201,  -547,   186,     7,  -744,   -48,
     504,   -68,  -115,  -644,  1029,   187,  -182,   510,   -50,  -744,
    -744,   465,  -744,   256,  -362,  -743,  -744,  -744,  -744,  -151,
    -744,   603,   605,  -300,  -744,  -744,   405,  -465,  -744,  -102,
    -744,  -744,  -744,   130,   -47,   -67,  -136,  -744,   388,  -128,
      33,  -121,    10,   486,   860,  -744,  -744,  -744,  -744,   365,
    -744,   564,  1159,   -59,   -89,   -75,  -744,  -744,  -744,    17,
      79,  -744,  -744,  -744,  -744,    26,   887,  -744,   701,   401,
    -744,   258,  -744,  -744,   260,   -14,  -744,   591,   794,  -744,
     526,  -744,  -744,   550,   383,  1578,    -7,    -3,   853,   710,
     -88,  -353,  1481,  -211,  -744,  -744,    20,  -744,   910,  -393,
       8,   237,    56,  -744,  -125
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     5,    58,    59,   145,   486,   293,   276,   436,   437,
     683,   684,   685,   294,   494,   295,   623,   728,   729,   730,
     840,   841,   842,   296,   297,   447,   825,   826,   695,   696,
     856,   697,   698,   814,   709,   821,   822,   764,   861,   862,
     457,   458,   466,   470,   605,   606,   609,   713,   714,   715,
     632,   633,   634,   386,   230,   554,   231,   636,   388,    53,
     133,   233,   234,   637,   235,   255,   139,   237,   266,   267,
     268,   269,   298,   450,   594,   299,   719,   783,   784,   300,
     301,   302,   303,   502,   503,   504,   505,   612,   111,    64,
     153,   154,   321,   517,   189,   190,   175,   649,   176,   177,
     335,   178,   179,   337,   180,   181,   191,   358,   359,   360,
     182,   183,   378,    32,    33,    34,    35,    36,    37,    38,
      39,   152,   315,    76,    40,    41,    96,    97,    98,   518,
     519,   520,   657,   658,   659,   159,   160,   161,   246,   247,
     248,   109,   225,   379,   380,    42,   270,    43,    56,    44,
      45,    46,    79,   595,   101,   513,   477,   121,   122,   104,
     353,   597,   124,   148,   322
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      55,   135,   320,   132,   174,   507,   158,   158,   227,   309,
      52,   305,   305,   582,   219,   708,   221,   305,    99,   306,
     306,   264,   100,   725,   173,   306,   307,   307,   220,   324,
     220,   823,   307,   105,   329,   150,   778,   331,   644,   250,
      80,   250,    95,   -42,   123,   102,   123,   705,    55,   758,
      82,   141,   162,   396,   326,   316,   102,   596,   102,   596,
     587,   596,   254,   619,    99,   463,   126,   857,   185,   273,
     313,   654,   445,   488,   817,    57,   476,   819,   316,   -78,
     304,   304,   151,   762,   738,   690,   304,   689,   205,   316,
     598,    60,   599,   354,   125,   508,   509,   355,   158,   707,
     -42,   489,    81,   123,    80,   290,   446,   362,   763,   238,
     498,   860,   -42,    63,   264,   102,   314,   655,   464,   232,
     656,   446,   823,   524,   319,   816,   816,   141,   588,   816,
     455,   141,   854,   271,   141,   323,   -78,   790,   264,   889,
     274,   240,   243,   327,   716,   757,   834,   319,   -78,   114,
     482,   210,   500,   305,   251,   870,   501,   110,   319,    65,
     306,   306,    16,   816,    66,   525,    81,   307,   307,   864,
     620,   127,   333,   112,   811,   768,   129,   498,   336,   391,
     621,   389,   712,   456,   515,   342,   343,   439,   708,   836,
      54,   144,   747,    27,    28,    29,   222,   336,   128,   223,
     733,   172,   188,   241,   244,   408,   130,   224,   409,   114,
     411,   372,   413,   116,  -164,   865,   208,   209,   499,   500,
     194,   480,   304,   501,    48,    48,   392,   243,    49,    49,
     866,  -164,   141,   141,   143,   332,   496,   702,   134,   147,
     849,   173,   149,   809,   155,  -164,  -164,  -164,   352,   497,
     392,   702,   141,    55,   600,    55,   398,    55,   620,   400,
     114,   114,   141,   867,   116,   116,   483,   601,   621,   264,
     622,   158,   194,   531,    55,   549,   497,  -470,  -470,   194,
      55,    99,    55,    55,   443,   185,   332,   332,   195,   244,
     461,  -470,   467,   471,   874,   333,   484,   876,   877,   236,
      48,   213,   785,   572,    49,   574,   769,   770,   773,   214,
     215,   596,   211,   212,   342,   343,   886,   250,   399,   216,
     217,   401,   888,   265,   347,   565,   566,   567,   514,   568,
     485,   202,   348,   349,   558,   204,   389,    16,   141,   620,
     614,   743,   165,   336,   629,   539,   402,   591,   571,   621,
     196,   724,   336,   336,   576,   197,   577,   428,   403,   305,
     580,   143,   169,   336,   198,   807,   529,   306,    27,    28,
      29,   170,   646,   647,   307,   533,   535,   652,    48,   165,
     555,   392,    49,   171,   256,   257,   543,   141,   199,   424,
     141,   141,   141,   660,   141,   403,   607,   425,   426,   169,
     260,   261,   201,   702,   734,   206,    16,   141,   170,   141,
     702,   141,   643,   141,   207,   141,   390,   141,   172,   141,
     171,   141,   579,   218,   702,   141,   220,   456,   304,   345,
     346,   641,     1,   844,     2,     3,     4,    27,    28,    29,
     265,   141,   211,   410,   592,   412,   572,   414,   574,   688,
     416,   417,   305,   702,   420,   421,   396,   226,   362,   141,
     306,   141,   422,   423,   444,   141,   773,   307,   530,   141,
     462,   205,   468,   472,   484,   536,   275,   537,    48,   216,
     427,   540,    49,   113,   544,   114,   310,   115,   312,   116,
     305,   328,   677,   238,   330,   439,   141,   686,   306,   117,
      16,   514,   202,   118,   203,   307,   204,   338,    74,   563,
     564,    91,   119,   411,    16,   413,   767,   339,   396,   340,
     844,   304,   448,   703,   120,   702,   797,   799,   800,   801,
     341,    27,    28,    29,   351,   456,   452,   454,   336,   583,
     584,   374,   336,   363,   336,    27,    28,    29,   256,   676,
     449,   675,   336,  -243,   141,  -243,   163,   141,   238,   304,
     238,   664,   193,   755,   140,   665,   745,   666,   364,  -243,
     141,   557,   141,  -243,   365,   668,   366,   396,   451,   264,
     220,  -243,  -243,   812,   563,   421,   375,   141,   141,   818,
     205,   382,   393,   394,  -243,   404,   405,   238,  -469,   406,
     428,   141,   407,   415,   429,   573,   449,   575,   441,  -243,
     493,  -243,   495,   492,   523,   541,   545,    67,    68,    69,
     546,    16,   547,    17,   548,  -243,   551,    19,   555,  -243,
     550,   552,   832,  -182,   559,   141,   141,  -243,  -243,    70,
     200,   453,    71,   779,   140,    72,   408,   305,  -459,   560,
    -243,  -188,    27,    28,    29,   306,   333,   581,   833,   835,
      73,   585,   307,   586,   589,   344,   686,   602,   603,   449,
     350,   238,  -243,   608,  -243,   610,   613,   670,    16,   611,
     357,   232,   501,   639,   616,   618,   667,   592,  -243,   555,
     141,   392,  -243,    16,  -184,   141,    16,   141,   141,   456,
    -243,  -243,   661,  -185,   592,   678,   756,   704,   141,    27,
      28,    29,   782,  -243,   141,   820,   304,   141,   710,   711,
     718,   674,   732,   736,    27,    28,    29,    27,    28,    29,
     739,   740,    78,   744,   655,   675,   141,   141,   141,   141,
     264,   751,   752,   774,   753,   140,   140,   775,   573,   754,
     575,   762,    16,   141,   675,   780,   141,   141,   786,   787,
     141,   788,   141,   803,   808,   140,   141,   141,   141,   781,
     141,   202,   846,   847,   141,   140,   850,   701,  -471,   851,
      78,   186,   186,    27,    28,    29,    78,   639,   868,   506,
     238,   701,   869,   238,   512,   141,   871,   717,   872,   884,
     720,   873,    16,   238,   141,   391,    18,   389,   875,    16,
     141,    48,   878,   858,   891,    49,   879,    50,   880,   680,
     804,   334,   344,   245,   626,   789,   627,   782,   845,   887,
      51,   737,   628,    27,    28,    29,   885,   249,   805,   249,
      27,    28,    29,   881,   238,   828,   706,   238,   238,    67,
      68,    69,   392,    16,   829,    17,   141,   308,   308,    19,
     750,   236,   830,   308,   141,   852,   238,   113,   883,   114,
      16,    70,   238,   116,    71,   141,   200,    72,    78,   141,
     760,   265,    75,   473,    27,    28,    29,   118,   556,    78,
     578,   478,    73,   479,    78,   474,   119,    78,   796,   650,
     140,    27,    28,    29,    78,   361,   630,   142,   475,   645,
     146,   669,   538,   108,   792,   371,    16,   653,   748,   749,
     140,   527,   140,   252,   140,   793,   798,   798,   798,   798,
      75,   569,   553,   631,   672,   103,    75,   456,    16,     0,
      48,   776,    16,   701,    49,     0,    50,    27,    28,    29,
     701,     0,   228,     0,   140,     0,   760,   760,     0,   229,
     760,     0,   137,     0,   701,   506,     0,   512,     0,    27,
      28,    29,   140,    27,    28,    29,     0,   138,   140,    16,
     639,   202,   140,   639,    16,   204,    48,   142,     0,     0,
      49,     0,   459,   859,   760,     0,     0,     0,   481,   308,
     760,   262,   491,     0,     0,   253,    89,   263,   137,     0,
      27,    28,    29,    78,     0,    27,    28,    29,    78,     0,
     186,     0,     0,   138,     0,     0,     0,     0,    75,   165,
       0,   166,     0,   402,   639,   167,     0,   639,   639,    75,
      16,     0,   390,    78,    75,   403,    78,    75,   186,   169,
     777,     0,    78,    78,    75,   186,   639,   186,   170,     0,
       0,   186,   639,    78,   186,   701,     0,   140,     0,     0,
     171,    27,    28,    29,     0,     0,   165,     0,   166,     0,
       0,     0,   316,   140,     0,   140,     0,     0,   142,   142,
       0,     0,   403,    16,     0,   202,   169,   279,   280,   281,
       0,     0,   721,   722,   723,   170,     0,     0,   142,     0,
     287,    68,    69,     0,    16,   249,    17,   171,   142,     0,
      19,     0,   317,     0,    27,    28,    29,     0,   438,   187,
       0,     0,    70,     0,     0,   288,     0,     0,    72,   318,
       0,    16,   113,   487,   114,    27,    28,    29,   140,   140,
       0,   319,     0,    73,     0,     0,     0,     0,   473,     0,
       0,    31,   118,    75,     0,     0,    61,    62,    75,   806,
     593,   119,    27,    28,    29,     0,     0,     0,     0,     0,
       0,     0,     0,   475,    94,   107,   615,     0,     0,    78,
       0,    78,     0,    75,     0,     0,    75,   691,     0,     0,
     624,   625,    75,    75,     0,   308,     0,   640,   140,     0,
     140,   140,     0,    75,    16,     0,    48,     0,   615,   615,
      49,   651,    50,   615,   692,     0,   186,   140,     0,    16,
     140,   430,     0,     0,     0,    49,     0,   131,    78,   691,
       0,     0,    78,   142,    78,    27,    28,    29,   693,     0,
     694,     0,    78,   137,     0,     0,   361,   186,     0,     0,
      27,    28,    29,   142,     0,   142,     0,   142,   138,   140,
     140,    16,     0,   430,     0,   140,     0,    49,     0,    50,
       0,   140,     0,     0,     0,    16,     0,    17,   487,    18,
     693,    19,   855,     0,     0,   137,   681,   142,   308,     0,
       0,     0,    27,    28,    29,     0,   432,   682,   140,   311,
     138,   459,     0,     0,   325,   142,    27,    28,    29,     0,
       0,   142,   228,     0,     0,   142,    78,    78,    78,     0,
     731,    16,     0,    17,   735,    18,   308,    19,   487,    75,
       0,    75,   442,     0,     0,     0,     0,     0,     0,   383,
       0,   356,   432,     0,    16,     0,    48,     0,     0,     0,
      49,     0,    27,    28,    29,     0,     0,   367,     0,   140,
     369,   186,   370,   882,    16,   253,    48,   373,   137,     0,
      49,   376,    50,   381,   316,    27,    28,    29,    75,     0,
       0,     0,    75,   138,    75,    51,     0,     0,     0,   279,
     280,   281,    75,     0,     0,    27,    28,    29,     0,     0,
     142,     0,   287,    68,    69,     0,    16,     0,    17,    16,
       0,    48,    19,     0,     0,    49,   142,    50,   142,     0,
     387,   187,     0,     0,    70,    16,   438,   288,     0,     0,
      72,   831,   731,     0,   791,   794,   795,    27,    28,    29,
      27,    28,    29,   319,   387,    73,   456,     0,     0,     0,
       0,     0,     0,   228,     0,   490,    27,    28,    29,     0,
       0,   521,   522,     0,     0,   487,    75,    75,    75,     0,
       0,     0,     0,    47,   824,   827,     0,   526,    47,    47,
     383,   142,   142,   308,     0,    16,   843,    48,   731,   460,
       0,    49,   848,    50,    47,    47,    47,    47,     0,    16,
       0,   430,   262,    18,     0,   431,   384,    89,   385,   137,
       0,    47,     0,     0,     0,   638,    27,    28,    29,     0,
     432,    16,   679,    48,   138,     0,     0,    49,   435,    50,
      27,    28,    29,     0,     0,     0,    47,    47,    16,     0,
     726,   142,    51,   142,   142,     0,     0,   465,     0,     0,
       0,     0,    27,    28,    29,   570,     0,     0,    47,   456,
     142,     0,     0,   142,     0,   824,     0,   487,   827,    27,
      28,    29,     0,   843,    47,   387,    47,     0,     0,    16,
       0,    48,     0,     0,     0,    49,     0,    50,     0,     0,
      77,     0,     0,     0,     0,    47,    47,     0,     0,    47,
      51,     0,   142,   142,     0,   810,     0,     0,   142,   700,
      27,    28,    29,     0,   142,    16,     0,   726,     0,   638,
       0,    47,     0,   700,     0,     0,    47,   334,    47,     0,
       0,     0,     0,     0,     0,     0,   456,   727,    77,   184,
     184,   142,     0,     0,    77,     0,    27,    28,    29,   642,
       0,     0,   863,     0,     0,    67,    68,    69,     0,    16,
       0,    17,     0,    47,     0,    19,     0,     0,     0,     0,
       0,     0,   662,     0,   663,     0,    16,    70,    17,    47,
      71,    47,    47,    72,    47,     0,   228,   635,     0,    47,
      27,    28,    29,    47,    47,    47,     0,   838,    73,   671,
     228,   673,   142,     0,     0,     0,     0,    27,    28,    29,
      47,     0,   759,    47,     0,   184,   184,     0,    16,     0,
      48,   184,     0,   890,    49,     0,    50,     0,     0,     0,
       0,     0,    16,     0,    48,     0,   184,     0,    49,   229,
      50,     0,   137,     0,     0,     0,   440,    77,     0,    27,
      28,    29,    77,   253,     0,   184,   137,   138,   638,   638,
     638,   638,    77,    27,    28,    29,   228,     0,     0,     0,
       0,   138,     0,     0,     0,   700,     0,     0,     0,     0,
       0,   699,   700,    47,    47,     0,     0,     0,   759,   759,
       0,   635,   759,     0,     0,   699,   700,     0,    16,    47,
      48,    47,     0,     0,    49,   746,     0,     0,     0,   228,
       0,     0,   638,     0,     0,   638,     0,     0,     0,   253,
       0,     0,   137,     0,   272,   700,   759,     0,     0,    27,
      28,    29,   759,     0,     0,     0,   383,   138,     0,     0,
       0,    16,   761,    48,     0,     0,     0,    49,     0,    50,
       0,     0,     0,    47,     0,     0,   184,   184,   262,     0,
       0,     0,   384,    89,   263,   137,   638,     0,     0,   638,
     638,    77,    27,    28,    29,     0,    77,    47,   184,     0,
     138,     0,     0,     0,     0,   387,     0,     0,   638,   766,
       0,     0,   772,   802,   638,     0,     0,   700,     0,     0,
       0,   184,   228,     0,    77,     0,   184,     0,     0,     0,
     184,   184,     0,   184,     0,   184,     0,     0,     0,   184,
       0,   184,   184,     0,     0,     0,     0,     0,     0,   383,
     635,   635,   635,   635,    16,   761,    48,     0,     0,     0,
      49,     0,    50,     0,     0,     0,     0,   699,   228,     0,
       0,   262,   853,     0,   699,   384,    89,   385,   137,     0,
     815,   815,     0,     0,   815,    27,    28,    29,   699,     0,
       0,    47,    47,   138,     0,     0,     0,     0,     0,     0,
      16,     0,    48,     0,   635,     0,    49,   635,     0,     0,
       0,     0,     0,    16,    47,    48,    47,   699,   815,    49,
       0,   253,     0,  -154,   137,     0,     0,     0,     0,     0,
     397,    27,    28,    29,   136,   418,   419,   137,     0,   138,
       0,    47,    47,    47,    27,    28,    29,   691,     0,     0,
       0,     0,   138,     0,     0,     0,     0,     0,   635,     0,
       0,   635,   635,     0,   184,     0,   387,    77,     0,    77,
     772,     0,     0,     0,   440,     0,   687,     0,   387,    16,
     635,   430,     0,   184,     0,    49,   635,     0,     0,   699,
       0,     0,     0,     0,     0,     0,   184,   184,   693,   184,
     694,   184,     0,   137,   184,     0,     0,     0,     0,     0,
      27,    28,    29,     0,     0,     0,   184,     0,   138,     0,
     184,     0,   184,     0,     0,     0,     0,     6,     0,     0,
     184,     0,     7,     0,     0,   184,     8,     0,     0,     0,
       9,     0,     0,     0,     0,     0,    47,    47,     0,    10,
       0,     0,    11,    12,     0,    13,    14,    15,    83,    16,
      84,    17,    85,    18,    86,    19,    20,    21,     0,     0,
       0,     0,    22,     0,    87,     0,   184,    24,    88,     0,
      25,    89,    90,    26,     0,     0,    91,    92,     0,     0,
      27,    28,    29,     0,     0,     0,     0,     0,    30,    93,
       0,     0,     0,     0,    77,    77,    77,     0,     6,     0,
       0,     0,     0,     7,   184,     0,     0,     8,     0,     0,
       0,     9,     0,     0,     0,     0,     0,     0,     0,     0,
      10,     0,     0,    11,    12,    47,    13,    14,    15,     0,
      16,     0,    17,     0,    18,   687,    19,    20,    21,   184,
       0,     0,     0,    22,     0,    23,     0,     0,    24,     0,
       0,    25,     0,     0,    26,     0,   106,     0,     0,     0,
       0,    27,    28,    29,     0,     6,     0,     0,     0,    30,
       7,     0,     0,     0,     8,     0,     0,     0,     9,   469,
       0,     0,     0,     0,    47,     0,     0,    10,     0,     0,
      11,    12,     0,    13,    14,    15,     0,    16,     0,    17,
       0,    18,     0,    19,    20,    21,     0,     0,     0,     0,
      22,    16,    23,    48,     0,    24,     0,    49,    25,    50,
       0,    26,     0,  -440,     0,     0,     6,     0,    27,    28,
      29,     7,    51,     0,     0,     8,    30,     0,     0,     9,
       0,     0,    27,    28,    29,     0,     0,     0,    10,     0,
       0,    11,    12,     0,    13,    14,    15,     0,    16,     0,
      17,   184,    18,     0,    19,    20,    21,     0,     0,     0,
       0,    22,     0,    23,     0,     0,    24,     0,     0,    25,
       0,     0,    26,     6,  -439,     0,     0,     0,     7,    27,
      28,    29,     8,     0,     0,     0,     9,    30,     0,     0,
       0,     0,     0,     0,     0,    10,     0,     0,    11,    12,
       0,    13,    14,    15,     0,    16,     0,    17,     0,    18,
       0,    19,    20,    21,     0,     0,     0,     0,    22,     0,
      23,     0,     0,    24,     0,     0,    25,     0,     0,    26,
       6,     0,     0,     0,     0,     7,    27,    28,    29,     8,
       0,     0,     0,   377,    30,     0,     0,     0,     0,     0,
       0,     0,    10,     0,     0,    11,    12,     0,    13,    14,
      15,     0,    16,     0,    17,   228,    18,     0,    19,    20,
      21,     0,     0,     0,     0,    22,     0,    23,   228,     0,
      24,     0,     0,    25,     0,     0,    26,     0,     0,     0,
       0,     0,   383,    27,    28,    29,     0,    16,     0,    48,
       0,    30,     0,    49,     0,    50,     0,     0,     0,     0,
      16,     0,    48,     0,   262,     0,    49,     0,   384,    89,
     263,   137,     0,     0,     0,     0,     0,     0,    27,    28,
      29,   253,     0,     0,   137,     0,   138,     0,     0,     0,
       7,    27,    28,    29,     8,     0,     0,     0,   156,   138,
       0,     0,     0,     0,     0,     0,     0,    10,     0,     0,
      11,    12,     0,    13,    14,    15,     0,    16,     0,    17,
       0,    18,     0,    19,    20,    21,     0,     0,     0,     0,
      22,     0,    23,     0,     0,    24,     0,     0,    25,     0,
       0,    26,   157,     7,     0,     0,     0,     8,    27,    28,
      29,     9,     0,     0,     0,     0,    30,     0,     0,     0,
      10,     0,     0,    11,    12,     0,    13,    14,    15,     0,
      16,     0,    17,     0,    18,     0,    19,    20,    21,     0,
       0,     0,     0,    22,     0,   242,     0,     0,    24,     0,
       0,    25,     0,   368,    26,     0,     7,     0,     0,     0,
       8,    27,    28,    29,     9,     0,     0,     0,     0,    30,
       0,     0,     0,    10,     0,     0,    11,    12,     0,    13,
      14,    15,     0,    16,     0,    17,     0,    18,     0,    19,
      20,    21,     0,     0,     0,     0,    22,     0,     0,     0,
       0,    24,     0,     0,    25,     0,   200,    26,     0,     7,
       0,     0,     0,     8,    27,    28,    29,     9,     0,     0,
       0,     0,    30,     0,     0,     0,    10,     0,     0,    11,
      12,     0,    13,    14,    15,     0,    16,     0,    17,     0,
      18,     0,    19,    20,    21,     0,     0,     0,     0,    22,
       0,    23,     0,     0,    24,     0,     0,    25,     0,     0,
      26,     0,     7,     0,     0,     0,     8,    27,    28,    29,
       9,     0,     0,     0,     0,    30,     0,     0,     0,    10,
       0,     0,    11,    12,     0,    13,    14,    15,     0,    16,
       0,    17,     0,    18,     0,    19,    20,    21,     0,     0,
       0,     0,    22,     0,   239,     0,     0,    24,     0,     0,
      25,     0,     0,    26,     0,     7,     0,     0,     0,     8,
      27,    28,    29,     9,     0,     0,     0,     0,    30,     0,
       0,     0,    10,     0,     0,    11,    12,     0,    13,    14,
      15,     0,    16,     0,    17,     0,    18,     0,    19,    20,
      21,     0,     0,     0,     0,    22,     0,   242,     0,     0,
      24,     0,     0,    25,     0,     0,    26,     0,     7,     0,
       0,     0,     8,    27,    28,    29,   156,     0,     0,     0,
       0,    30,     0,     0,     0,    10,     0,     0,    11,    12,
       0,    13,    14,    15,     0,    16,     0,    17,     0,    18,
       0,    19,    20,    21,     0,     0,     0,     0,    22,     0,
      23,     0,     0,    24,     0,     0,    25,     0,     0,    26,
       0,     7,     0,     0,     0,     8,    27,    28,    29,     9,
       0,     0,     0,     0,    30,     0,     0,     0,    10,     0,
       0,    11,    12,     0,    13,    14,    15,     0,    16,     0,
      17,     0,    18,     0,    19,    20,    21,     0,     0,     0,
       0,    22,     0,     0,     0,     0,    24,     0,     0,    25,
       0,     0,    26,     0,     0,     0,     0,     0,     0,    27,
      28,    29,   277,   278,     0,     0,     0,    30,     0,     0,
     279,   280,   281,   282,   283,   284,     0,     0,   285,   286,
       0,     0,     0,   287,    68,    69,     0,    16,     0,    17,
       0,     0,     0,    19,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,    70,     0,     0,   288,     0,
       0,    72,   289,     0,     0,     0,     0,   290,    27,    28,
      29,   291,   292,   277,   278,     0,    73,     0,     0,     0,
       0,   279,   280,   281,   282,   283,   284,     0,     0,   285,
     286,     0,     0,     0,   287,    68,    69,     0,    16,     0,
      17,     0,     0,     0,    19,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,    70,     0,     0,   288,
       0,     0,    72,     0,     0,     0,     0,     0,     0,    27,
      28,    29,   291,   292,     0,     0,    12,    73,    13,    14,
      15,     0,    16,     0,    17,     0,    18,     0,    19,    20,
      21,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      24,     0,     0,    25,     0,     0,    26,     0,     0,     0,
       0,     0,     0,    27,    28,    29,     0,     0,     0,     0,
       0,    30,    67,    68,    69,   113,    16,   114,    17,     0,
       0,   116,    19,     0,     0,  -247,     0,     0,   194,     0,
       0,   510,     0,     0,    70,   118,     0,    71,  -247,     0,
      72,     0,     0,   474,   119,     0,     0,    27,    28,    29,
       0,     0,     0,     0,     0,    73,   511,   164,    68,    69,
     165,    16,   166,    17,     0,     0,   167,    19,     0,     0,
       0,     0,     0,     0,     0,     0,   168,     0,     0,    70,
     169,     0,    71,    89,    90,    72,     0,     0,     0,   170,
       0,     0,    27,    28,    29,     0,     0,     0,     0,     0,
      73,   171,   287,    68,    69,   165,    16,   166,    17,     0,
       0,   167,    19,     0,     0,     0,     0,     0,     0,     0,
       0,   168,     0,     0,    70,   169,     0,   288,    89,    90,
      72,     0,     0,     0,   170,     0,     0,    27,    28,    29,
       0,     0,     0,     0,     0,    73,   171,    67,    68,    69,
     113,    16,   114,    17,     0,     0,   116,    19,     0,     0,
     631,     0,     0,   194,     0,    16,   510,    48,     0,    70,
     118,    49,    71,    50,     0,    72,     0,     0,   474,   119,
       0,     0,    27,    28,    29,     0,   229,     0,     0,   137,
      73,   511,     0,     0,     0,     0,    27,    28,    29,   164,
      68,    69,     0,    16,   138,    17,     0,     0,     0,    19,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,    70,     0,     0,    71,     0,     0,    72,     0,   106,
       0,     0,     0,     0,    27,    28,    29,   164,    68,    69,
       0,    16,    73,    17,     0,     0,     0,    19,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,    70,
       0,     0,    71,     0,     0,    72,   516,     0,     0,     0,
       0,     0,    27,    28,    29,    67,    68,    69,     0,    16,
      73,    17,     0,     0,     0,    19,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   192,    70,     0,     0,
      71,     0,     0,    72,     0,     0,     0,     0,     0,     0,
      27,    28,    29,    67,    68,    69,     0,    16,    73,    17,
       0,     0,     0,    19,     0,     0,     0,     0,     0,     0,
       0,     0,   528,     0,     0,    70,     0,     0,    71,     0,
       0,    72,     0,     0,     0,     0,     0,     0,    27,    28,
      29,   164,    68,    69,     0,    16,    73,    17,     0,     0,
       0,    19,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,    70,     0,     0,    71,     0,     0,    72,
       0,     0,     0,     0,     0,     0,    27,    28,    29,    67,
      68,    69,     0,    16,    73,    17,     0,     0,     0,    19,
       0,     0,     0,     0,     0,     0,     0,     0,   532,     0,
       0,    70,     0,     0,    71,     0,     0,    72,     0,     0,
       0,     0,     0,     0,    27,    28,    29,    67,    68,    69,
       0,    16,    73,    17,     0,     0,     0,    19,     0,     0,
       0,     0,     0,     0,     0,     0,   534,     0,     0,    70,
       0,     0,    71,     0,     0,    72,     0,     0,     0,     0,
       0,     0,    27,    28,    29,    67,    68,    69,     0,    16,
      73,    17,     0,     0,     0,    19,     0,     0,     0,     0,
       0,     0,     0,     0,   542,     0,     0,    70,     0,     0,
      71,     0,     0,    72,     0,     0,     0,     0,     0,     0,
      27,    28,    29,    67,    68,    69,     0,    16,    73,    17,
       0,     0,     0,    19,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    70,     0,     0,    71,     0,
     617,    72,     0,     0,     0,     0,     0,     0,    27,    28,
      29,   648,    68,    69,     0,    16,    73,    17,     0,     0,
      16,    19,    17,     0,     0,     0,     0,     0,     0,     0,
     187,   837,     0,    70,     0,     0,    71,     0,     0,    72,
       0,   838,   839,    16,   114,    48,    27,    28,    29,    49,
       0,    27,    28,    29,    73,     0,     0,     0,     0,     0,
       0,     0,   771,  -183,   253,     0,    16,   137,    48,     0,
     768,     0,    49,     0,    27,    28,    29,     0,     0,    16,
       0,    48,   138,   397,     0,    49,     0,   136,   561,   562,
     137,     0,     0,     0,     0,     0,   397,    27,    28,    29,
     136,   561,   419,   137,     0,   138,     0,    16,   114,    48,
      27,    28,    29,    49,     0,     0,     0,     0,   138,     0,
       0,    16,     0,    48,     0,     0,   771,    49,   253,     0,
       0,   137,     0,     0,   768,     0,     0,     0,    27,    28,
      29,     0,   136,   258,   259,   137,   138,     0,     0,    16,
       0,    48,    27,    28,    29,    49,     0,     0,     0,     0,
     138,     0,     0,     0,     0,    16,   397,    48,     0,  -183,
     136,    49,     0,   137,     0,   590,     0,     0,     0,     0,
      27,    28,    29,     0,     0,  -183,   136,     0,   138,   137,
       0,     0,     0,    16,     0,    48,    27,    28,    29,    49,
       0,     0,     0,     0,   138,     0,    16,   114,    48,     0,
     742,     0,    49,  -183,   136,     0,     0,   137,     0,     0,
       0,     0,     0,     0,    27,    28,    29,   136,     0,    16,
     137,    48,   138,   768,    16,    49,    48,    27,    28,    29,
      49,     0,     0,     0,     0,   138,   395,     0,     0,     0,
     136,   397,     0,   137,     0,   136,     0,     0,   137,     0,
      27,    28,    29,     0,     0,    27,    28,    29,   138,    16,
       0,    48,     0,   138,    16,    49,    48,     0,     0,   604,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     136,     0,     0,   137,  -183,   136,     0,    16,   137,    48,
      27,    28,    29,    49,     0,    27,    28,    29,   138,     0,
       0,     0,     0,   138,   741,     0,     0,     0,   136,     0,
      16,   137,    48,     0,     0,     0,    49,     0,    27,    28,
      29,     0,     0,     0,     0,     0,   138,     0,     0,   765,
       0,   253,     0,    16,   137,    48,     0,     0,    16,    49,
      48,    27,    28,    29,    49,     0,    50,     0,     0,   138,
       0,     0,   813,     0,   253,     0,     0,   137,     0,   229,
       0,    16,   137,    48,    27,    28,    29,    49,     0,    27,
      28,    29,   138,     0,    16,     0,    48,   138,   742,     0,
      49,     0,   136,     0,     0,   137,     0,     0,     0,     0,
       0,     0,    27,    28,    29,   136,     0,    16,   137,    48,
     138,     0,     0,    49,     0,    27,    28,    29,     0,     0,
      16,     0,   430,   138,    18,     0,   431,     0,   253,     0,
       0,   137,     0,     0,     0,     0,     0,     0,    27,    28,
      29,   432,   433,   434,     0,     0,   138,     0,     0,   435,
       0,    27,    28,    29
};

static const yytype_int16 yycheck[] =
{
       3,    51,   153,    51,    71,   305,    65,    66,   110,   148,
       3,   147,   148,   435,   103,   604,   105,   153,    25,   147,
     148,   136,    25,   619,    71,   153,   147,   148,   103,   154,
     105,   774,   153,    25,   159,    11,   709,   162,   503,   127,
      23,   129,    25,     1,    36,    25,    38,   600,    51,   693,
      24,    54,    66,   235,   156,     1,    36,   450,    38,   452,
       1,   454,   130,   485,    71,     1,    40,   806,    71,   137,
      12,     1,     1,     1,   770,    62,   287,   773,     1,     1,
     147,   148,    58,    22,   631,   589,   153,     1,    91,     1,
     452,     0,   454,    55,    38,   306,   307,    59,   157,   603,
      58,    29,    23,    95,    87,    63,    35,   195,    47,   112,
       1,   807,    70,    69,   229,    95,    58,    47,    54,   112,
      50,    35,   865,     1,    70,   769,   770,   130,    69,   773,
       1,   134,   805,   136,   137,    58,    58,   733,   253,   878,
     143,   124,   125,   157,   609,   692,    58,    70,    70,    34,
     289,    95,    43,   289,   128,   828,    47,    13,    70,    69,
     288,   289,    33,   807,    69,    43,    87,   288,   289,   813,
      54,    69,   164,    42,   763,    60,    69,     1,   168,   229,
      64,   229,     1,    54,   309,   177,   178,   275,   777,   785,
       3,     1,   657,    64,    65,    66,    44,   187,    45,    47,
     622,    71,    72,   124,   125,   253,    42,    55,   256,    34,
     258,   214,   260,    38,    33,    55,    55,    56,    42,    43,
      45,   288,   289,    47,    35,    35,   229,   210,    39,    39,
      70,    50,   235,   236,    46,    60,    42,   590,    51,    69,
     793,   288,     7,    54,    15,    64,    65,    66,    73,    55,
     253,   604,   255,   256,    42,   258,   239,   260,    54,   242,
      34,    34,   265,    42,    38,    38,     1,    55,    64,   384,
      66,   330,    45,   341,   277,   377,    55,    55,    56,    45,
     283,   288,   285,   286,   277,   288,    60,    60,    69,   210,
     283,    69,   285,   286,   847,   287,    31,   850,   851,   112,
      35,    47,   724,   418,    39,   420,   699,   700,   701,    55,
      56,   704,    55,    56,   306,   307,   869,   405,   239,    55,
      56,   242,   875,   136,    47,   393,   394,   395,   308,   397,
      65,    35,    55,    56,   384,    39,   384,    33,   341,    54,
     476,   641,    32,   333,   495,   348,    36,    43,   416,    64,
      56,    66,   342,   343,   422,    56,   424,    42,    48,   495,
     428,    46,    52,   353,    56,   758,   333,   495,    64,    65,
      66,    61,   508,   509,   495,   342,   343,   513,    35,    32,
     383,   384,    39,    73,    55,    56,   353,   390,    56,    47,
     393,   394,   395,   518,   397,    48,   464,    55,    56,    52,
      55,    56,    56,   756,    31,    56,    33,   410,    61,   412,
     763,   414,   501,   416,    56,   418,   229,   420,   288,   422,
      73,   424,   425,    43,   777,   428,   501,    54,   495,    55,
      56,   499,     1,   786,     3,     4,     5,    64,    65,    66,
     253,   444,    55,   256,   447,   258,   561,   260,   563,   588,
      55,    56,   588,   806,    55,    56,   638,    59,   546,   462,
     588,   464,    55,    56,   277,   468,   859,   588,   338,   472,
     283,   474,   285,   286,    31,   345,    54,   347,    35,    55,
      56,   351,    39,    32,   354,    34,    69,    36,    11,    38,
     626,    49,   560,   496,    58,   583,   499,   585,   626,    48,
      33,   481,    35,    52,    37,   626,    39,    55,    22,    55,
      56,    60,    61,   561,    33,   563,   698,    56,   700,    56,
     873,   588,     1,   591,    73,   878,   739,   740,   741,   742,
      42,    64,    65,    66,    43,    54,   280,   281,   528,    55,
      56,    56,   532,    60,   534,    64,    65,    66,    55,    56,
      29,   554,   542,    32,   557,    34,    70,   560,   561,   626,
     563,   528,    76,   688,    54,   532,   655,   534,    60,    48,
     573,   384,   575,    52,    60,   542,    60,   759,     1,   694,
     655,    60,    61,   765,    55,    56,    56,   590,   591,   771,
     593,    47,    50,    53,    73,    70,    55,   600,    43,    43,
      42,   604,    70,    56,    59,   418,    29,   420,    13,    32,
      58,    34,    58,    70,    12,    29,    70,    29,    30,    31,
      55,    33,    43,    35,    56,    48,    55,    39,   631,    52,
      49,    44,   783,    53,    56,   638,   639,    60,    61,    51,
      56,     1,    54,   711,   134,    57,   694,   783,    54,    53,
      73,    53,    64,    65,    66,   783,   648,    56,   783,   784,
      72,    54,   783,    71,    53,   179,   754,    31,    53,    29,
     184,   674,    32,    47,    34,    53,    53,   547,    33,    13,
     194,   674,    47,   496,    56,    56,    56,   690,    48,   692,
     693,   694,    52,    33,    53,   698,    33,   700,   701,    54,
      60,    61,    58,    53,   707,    56,    43,    55,   711,    64,
      65,    66,   715,    73,   717,    70,   783,   720,    56,    55,
      69,    61,    54,    31,    64,    65,    66,    64,    65,    66,
      50,    53,    22,    43,    47,   738,   739,   740,   741,   742,
     855,    56,    56,    69,    56,   235,   236,    15,   561,    55,
     563,    22,    33,   756,   757,    55,   759,   760,    54,    56,
     763,    55,   765,    50,    56,   255,   769,   770,   771,    50,
     773,    35,    56,    42,   777,   265,    42,   590,    69,    42,
      70,    71,    72,    64,    65,    66,    76,   600,    55,   303,
     793,   604,    42,   796,   308,   798,    56,   610,    56,   867,
     613,    55,    33,   806,   807,   855,    37,   855,    42,    33,
     813,    35,    53,   806,   882,    39,    56,    41,    55,   583,
     754,     1,   336,    54,   493,   732,   493,   830,   788,   873,
      54,   626,   495,    64,    65,    66,   868,   127,   756,   129,
      64,    65,    66,   865,   847,   777,   601,   850,   851,    29,
      30,    31,   855,    33,   780,    35,   859,   147,   148,    39,
     674,   674,   781,   153,   867,   796,   869,    32,   867,    34,
      33,    51,   875,    38,    54,   878,    56,    57,   168,   882,
     693,   694,    22,    48,    64,    65,    66,    52,   384,   179,
     425,   288,    72,   288,   184,    60,    61,   187,    61,   511,
     390,    64,    65,    66,   194,   195,     1,    54,    73,   504,
      57,   546,   348,    26,    31,   214,    33,   516,   658,   661,
     410,   330,   412,   129,   414,    42,   739,   740,   741,   742,
      70,   405,   382,    28,   551,    25,    76,    54,    33,    -1,
      35,   704,    33,   756,    39,    -1,    41,    64,    65,    66,
     763,    -1,     1,    -1,   444,    -1,   769,   770,    -1,    54,
     773,    -1,    57,    -1,   777,   479,    -1,   481,    -1,    64,
      65,    66,   462,    64,    65,    66,    -1,    72,   468,    33,
     793,    35,   472,   796,    33,    39,    35,   134,    -1,    -1,
      39,    -1,   282,   806,   807,    -1,    -1,    -1,   288,   289,
     813,    50,   292,    -1,    -1,    54,    55,    56,    57,    -1,
      64,    65,    66,   303,    -1,    64,    65,    66,   308,    -1,
     310,    -1,    -1,    72,    -1,    -1,    -1,    -1,   168,    32,
      -1,    34,    -1,    36,   847,    38,    -1,   850,   851,   179,
      33,    -1,   855,   333,   184,    48,   336,   187,   338,    52,
      43,    -1,   342,   343,   194,   345,   869,   347,    61,    -1,
      -1,   351,   875,   353,   354,   878,    -1,   557,    -1,    -1,
      73,    64,    65,    66,    -1,    -1,    32,    -1,    34,    -1,
      -1,    -1,     1,   573,    -1,   575,    -1,    -1,   235,   236,
      -1,    -1,    48,    33,    -1,    35,    52,    16,    17,    18,
      -1,    -1,   616,   617,   618,    61,    -1,    -1,   255,    -1,
      29,    30,    31,    -1,    33,   405,    35,    73,   265,    -1,
      39,    -1,    41,    -1,    64,    65,    66,    -1,   275,    48,
      -1,    -1,    51,    -1,    -1,    54,    -1,    -1,    57,    58,
      -1,    33,    32,   290,    34,    64,    65,    66,   638,   639,
      -1,    70,    -1,    72,    -1,    -1,    -1,    -1,    48,    -1,
      -1,     2,    52,   303,    -1,    -1,     7,     8,   308,    61,
      60,    61,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    25,    26,   476,    -1,    -1,   479,
      -1,   481,    -1,   333,    -1,    -1,   336,     1,    -1,    -1,
     490,   491,   342,   343,    -1,   495,    -1,   497,   698,    -1,
     700,   701,    -1,   353,    33,    -1,    35,    -1,   508,   509,
      39,   511,    41,   513,    28,    -1,   516,   717,    -1,    33,
     720,    35,    -1,    -1,    -1,    39,    -1,    56,   528,     1,
      -1,    -1,   532,   390,   534,    64,    65,    66,    52,    -1,
      54,    -1,   542,    57,    -1,    -1,   546,   547,    -1,    -1,
      64,    65,    66,   410,    -1,   412,    -1,   414,    72,   759,
     760,    33,    -1,    35,    -1,   765,    -1,    39,    -1,    41,
      -1,   771,    -1,    -1,    -1,    33,    -1,    35,   435,    37,
      52,    39,    54,    -1,    -1,    57,    44,   444,   588,    -1,
      -1,    -1,    64,    65,    66,    -1,    54,    55,   798,   150,
      72,   601,    -1,    -1,   155,   462,    64,    65,    66,    -1,
      -1,   468,     1,    -1,    -1,   472,   616,   617,   618,    -1,
     620,    33,    -1,    35,   624,    37,   626,    39,   485,   479,
      -1,   481,     1,    -1,    -1,    -1,    -1,    -1,    -1,    28,
      -1,   192,    54,    -1,    33,    -1,    35,    -1,    -1,    -1,
      39,    -1,    64,    65,    66,    -1,    -1,   208,    -1,   859,
     211,   661,   213,    52,    33,    54,    35,   218,    57,    -1,
      39,   222,    41,   224,     1,    64,    65,    66,   528,    -1,
      -1,    -1,   532,    72,   534,    54,    -1,    -1,    -1,    16,
      17,    18,   542,    -1,    -1,    64,    65,    66,    -1,    -1,
     557,    -1,    29,    30,    31,    -1,    33,    -1,    35,    33,
      -1,    35,    39,    -1,    -1,    39,   573,    41,   575,    -1,
     229,    48,    -1,    -1,    51,    33,   583,    54,    -1,    -1,
      57,    58,   732,    -1,   734,   735,   736,    64,    65,    66,
      64,    65,    66,    70,   253,    72,    54,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    63,    64,    65,    66,    -1,
      -1,   312,   313,    -1,    -1,   622,   616,   617,   618,    -1,
      -1,    -1,    -1,     2,   774,   775,    -1,   328,     7,     8,
      28,   638,   639,   783,    -1,    33,   786,    35,   788,     1,
      -1,    39,   792,    41,    23,    24,    25,    26,    -1,    33,
      -1,    35,    50,    37,    -1,    39,    54,    55,    56,    57,
      -1,    40,    -1,    -1,    -1,   496,    64,    65,    66,    -1,
      54,    33,    56,    35,    72,    -1,    -1,    39,    62,    41,
      64,    65,    66,    -1,    -1,    -1,    65,    66,    33,    -1,
      35,   698,    54,   700,   701,    -1,    -1,     1,    -1,    -1,
      -1,    -1,    64,    65,    66,   406,    -1,    -1,    87,    54,
     717,    -1,    -1,   720,    -1,   865,    -1,   724,   868,    64,
      65,    66,    -1,   873,   103,   384,   105,    -1,    -1,    33,
      -1,    35,    -1,    -1,    -1,    39,    -1,    41,    -1,    -1,
      22,    -1,    -1,    -1,    -1,   124,   125,    -1,    -1,   128,
      54,    -1,   759,   760,    -1,   762,    -1,    -1,   765,   590,
      64,    65,    66,    -1,   771,    33,    -1,    35,    -1,   600,
      -1,   150,    -1,   604,    -1,    -1,   155,     1,   157,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    54,    55,    70,    71,
      72,   798,    -1,    -1,    76,    -1,    64,    65,    66,   500,
      -1,    -1,   809,    -1,    -1,    29,    30,    31,    -1,    33,
      -1,    35,    -1,   192,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,   523,    -1,   525,    -1,    33,    51,    35,   208,
      54,   210,   211,    57,   213,    -1,     1,   496,    -1,   218,
      64,    65,    66,   222,   223,   224,    -1,    54,    72,   550,
       1,   552,   859,    -1,    -1,    -1,    -1,    64,    65,    66,
     239,    -1,   693,   242,    -1,   147,   148,    -1,    33,    -1,
      35,   153,    -1,   880,    39,    -1,    41,    -1,    -1,    -1,
      -1,    -1,    33,    -1,    35,    -1,   168,    -1,    39,    54,
      41,    -1,    57,    -1,    -1,    -1,   275,   179,    -1,    64,
      65,    66,   184,    54,    -1,   187,    57,    72,   739,   740,
     741,   742,   194,    64,    65,    66,     1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,   756,    -1,    -1,    -1,    -1,
      -1,   590,   763,   312,   313,    -1,    -1,    -1,   769,   770,
      -1,   600,   773,    -1,    -1,   604,   777,    -1,    33,   328,
      35,   330,    -1,    -1,    39,   656,    -1,    -1,    -1,     1,
      -1,    -1,   793,    -1,    -1,   796,    -1,    -1,    -1,    54,
      -1,    -1,    57,    -1,    59,   806,   807,    -1,    -1,    64,
      65,    66,   813,    -1,    -1,    -1,    28,    72,    -1,    -1,
      -1,    33,    34,    35,    -1,    -1,    -1,    39,    -1,    41,
      -1,    -1,    -1,   382,    -1,    -1,   288,   289,    50,    -1,
      -1,    -1,    54,    55,    56,    57,   847,    -1,    -1,   850,
     851,   303,    64,    65,    66,    -1,   308,   406,   310,    -1,
      72,    -1,    -1,    -1,    -1,   694,    -1,    -1,   869,   698,
      -1,    -1,   701,   744,   875,    -1,    -1,   878,    -1,    -1,
      -1,   333,     1,    -1,   336,    -1,   338,    -1,    -1,    -1,
     342,   343,    -1,   345,    -1,   347,    -1,    -1,    -1,   351,
      -1,   353,   354,    -1,    -1,    -1,    -1,    -1,    -1,    28,
     739,   740,   741,   742,    33,    34,    35,    -1,    -1,    -1,
      39,    -1,    41,    -1,    -1,    -1,    -1,   756,     1,    -1,
      -1,    50,   803,    -1,   763,    54,    55,    56,    57,    -1,
     769,   770,    -1,    -1,   773,    64,    65,    66,   777,    -1,
      -1,   500,   501,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      33,    -1,    35,    -1,   793,    -1,    39,   796,    -1,    -1,
      -1,    -1,    -1,    33,   523,    35,   525,   806,   807,    39,
      -1,    54,    -1,    56,    57,    -1,    -1,    -1,    -1,    -1,
      50,    64,    65,    66,    54,    55,    56,    57,    -1,    72,
      -1,   550,   551,   552,    64,    65,    66,     1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   847,    -1,
      -1,   850,   851,    -1,   476,    -1,   855,   479,    -1,   481,
     859,    -1,    -1,    -1,   583,    -1,   585,    -1,   867,    33,
     869,    35,    -1,   495,    -1,    39,   875,    -1,    -1,   878,
      -1,    -1,    -1,    -1,    -1,    -1,   508,   509,    52,   511,
      54,   513,    -1,    57,   516,    -1,    -1,    -1,    -1,    -1,
      64,    65,    66,    -1,    -1,    -1,   528,    -1,    72,    -1,
     532,    -1,   534,    -1,    -1,    -1,    -1,     1,    -1,    -1,
     542,    -1,     6,    -1,    -1,   547,    10,    -1,    -1,    -1,
      14,    -1,    -1,    -1,    -1,    -1,   655,   656,    -1,    23,
      -1,    -1,    26,    27,    -1,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    -1,    -1,
      -1,    -1,    46,    -1,    48,    -1,   588,    51,    52,    -1,
      54,    55,    56,    57,    -1,    -1,    60,    61,    -1,    -1,
      64,    65,    66,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,   616,   617,   618,    -1,     1,    -1,
      -1,    -1,    -1,     6,   626,    -1,    -1,    10,    -1,    -1,
      -1,    14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      23,    -1,    -1,    26,    27,   744,    29,    30,    31,    -1,
      33,    -1,    35,    -1,    37,   754,    39,    40,    41,   661,
      -1,    -1,    -1,    46,    -1,    48,    -1,    -1,    51,    -1,
      -1,    54,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    -1,     1,    -1,    -1,    -1,    72,
       6,    -1,    -1,    -1,    10,    -1,    -1,    -1,    14,     1,
      -1,    -1,    -1,    -1,   803,    -1,    -1,    23,    -1,    -1,
      26,    27,    -1,    29,    30,    31,    -1,    33,    -1,    35,
      -1,    37,    -1,    39,    40,    41,    -1,    -1,    -1,    -1,
      46,    33,    48,    35,    -1,    51,    -1,    39,    54,    41,
      -1,    57,    -1,    59,    -1,    -1,     1,    -1,    64,    65,
      66,     6,    54,    -1,    -1,    10,    72,    -1,    -1,    14,
      -1,    -1,    64,    65,    66,    -1,    -1,    -1,    23,    -1,
      -1,    26,    27,    -1,    29,    30,    31,    -1,    33,    -1,
      35,   783,    37,    -1,    39,    40,    41,    -1,    -1,    -1,
      -1,    46,    -1,    48,    -1,    -1,    51,    -1,    -1,    54,
      -1,    -1,    57,     1,    59,    -1,    -1,    -1,     6,    64,
      65,    66,    10,    -1,    -1,    -1,    14,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    23,    -1,    -1,    26,    27,
      -1,    29,    30,    31,    -1,    33,    -1,    35,    -1,    37,
      -1,    39,    40,    41,    -1,    -1,    -1,    -1,    46,    -1,
      48,    -1,    -1,    51,    -1,    -1,    54,    -1,    -1,    57,
       1,    -1,    -1,    -1,    -1,     6,    64,    65,    66,    10,
      -1,    -1,    -1,    14,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    23,    -1,    -1,    26,    27,    -1,    29,    30,
      31,    -1,    33,    -1,    35,     1,    37,    -1,    39,    40,
      41,    -1,    -1,    -1,    -1,    46,    -1,    48,     1,    -1,
      51,    -1,    -1,    54,    -1,    -1,    57,    -1,    -1,    -1,
      -1,    -1,    28,    64,    65,    66,    -1,    33,    -1,    35,
      -1,    72,    -1,    39,    -1,    41,    -1,    -1,    -1,    -1,
      33,    -1,    35,    -1,    50,    -1,    39,    -1,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    64,    65,
      66,    54,    -1,    -1,    57,    -1,    72,    -1,    -1,    -1,
       6,    64,    65,    66,    10,    -1,    -1,    -1,    14,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,    -1,
      26,    27,    -1,    29,    30,    31,    -1,    33,    -1,    35,
      -1,    37,    -1,    39,    40,    41,    -1,    -1,    -1,    -1,
      46,    -1,    48,    -1,    -1,    51,    -1,    -1,    54,    -1,
      -1,    57,    58,     6,    -1,    -1,    -1,    10,    64,    65,
      66,    14,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      23,    -1,    -1,    26,    27,    -1,    29,    30,    31,    -1,
      33,    -1,    35,    -1,    37,    -1,    39,    40,    41,    -1,
      -1,    -1,    -1,    46,    -1,    48,    -1,    -1,    51,    -1,
      -1,    54,    -1,    56,    57,    -1,     6,    -1,    -1,    -1,
      10,    64,    65,    66,    14,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    23,    -1,    -1,    26,    27,    -1,    29,
      30,    31,    -1,    33,    -1,    35,    -1,    37,    -1,    39,
      40,    41,    -1,    -1,    -1,    -1,    46,    -1,    -1,    -1,
      -1,    51,    -1,    -1,    54,    -1,    56,    57,    -1,     6,
      -1,    -1,    -1,    10,    64,    65,    66,    14,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    23,    -1,    -1,    26,
      27,    -1,    29,    30,    31,    -1,    33,    -1,    35,    -1,
      37,    -1,    39,    40,    41,    -1,    -1,    -1,    -1,    46,
      -1,    48,    -1,    -1,    51,    -1,    -1,    54,    -1,    -1,
      57,    -1,     6,    -1,    -1,    -1,    10,    64,    65,    66,
      14,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    23,
      -1,    -1,    26,    27,    -1,    29,    30,    31,    -1,    33,
      -1,    35,    -1,    37,    -1,    39,    40,    41,    -1,    -1,
      -1,    -1,    46,    -1,    48,    -1,    -1,    51,    -1,    -1,
      54,    -1,    -1,    57,    -1,     6,    -1,    -1,    -1,    10,
      64,    65,    66,    14,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    23,    -1,    -1,    26,    27,    -1,    29,    30,
      31,    -1,    33,    -1,    35,    -1,    37,    -1,    39,    40,
      41,    -1,    -1,    -1,    -1,    46,    -1,    48,    -1,    -1,
      51,    -1,    -1,    54,    -1,    -1,    57,    -1,     6,    -1,
      -1,    -1,    10,    64,    65,    66,    14,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    23,    -1,    -1,    26,    27,
      -1,    29,    30,    31,    -1,    33,    -1,    35,    -1,    37,
      -1,    39,    40,    41,    -1,    -1,    -1,    -1,    46,    -1,
      48,    -1,    -1,    51,    -1,    -1,    54,    -1,    -1,    57,
      -1,     6,    -1,    -1,    -1,    10,    64,    65,    66,    14,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    23,    -1,
      -1,    26,    27,    -1,    29,    30,    31,    -1,    33,    -1,
      35,    -1,    37,    -1,    39,    40,    41,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    51,    -1,    -1,    54,
      -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    64,
      65,    66,     8,     9,    -1,    -1,    -1,    72,    -1,    -1,
      16,    17,    18,    19,    20,    21,    -1,    -1,    24,    25,
      -1,    -1,    -1,    29,    30,    31,    -1,    33,    -1,    35,
      -1,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    -1,    -1,    51,    -1,    -1,    54,    -1,
      -1,    57,    58,    -1,    -1,    -1,    -1,    63,    64,    65,
      66,    67,    68,     8,     9,    -1,    72,    -1,    -1,    -1,
      -1,    16,    17,    18,    19,    20,    21,    -1,    -1,    24,
      25,    -1,    -1,    -1,    29,    30,    31,    -1,    33,    -1,
      35,    -1,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    -1,    -1,    51,    -1,    -1,    54,
      -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    -1,    -1,    27,    72,    29,    30,
      31,    -1,    33,    -1,    35,    -1,    37,    -1,    39,    40,
      41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      51,    -1,    -1,    54,    -1,    -1,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    64,    65,    66,    -1,    -1,    -1,    -1,
      -1,    72,    29,    30,    31,    32,    33,    34,    35,    -1,
      -1,    38,    39,    -1,    -1,    42,    -1,    -1,    45,    -1,
      -1,    48,    -1,    -1,    51,    52,    -1,    54,    55,    -1,
      57,    -1,    -1,    60,    61,    -1,    -1,    64,    65,    66,
      -1,    -1,    -1,    -1,    -1,    72,    73,    29,    30,    31,
      32,    33,    34,    35,    -1,    -1,    38,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    51,
      52,    -1,    54,    55,    56,    57,    -1,    -1,    -1,    61,
      -1,    -1,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,
      72,    73,    29,    30,    31,    32,    33,    34,    35,    -1,
      -1,    38,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    -1,    -1,    51,    52,    -1,    54,    55,    56,
      57,    -1,    -1,    -1,    61,    -1,    -1,    64,    65,    66,
      -1,    -1,    -1,    -1,    -1,    72,    73,    29,    30,    31,
      32,    33,    34,    35,    -1,    -1,    38,    39,    -1,    -1,
      28,    -1,    -1,    45,    -1,    33,    48,    35,    -1,    51,
      52,    39,    54,    41,    -1,    57,    -1,    -1,    60,    61,
      -1,    -1,    64,    65,    66,    -1,    54,    -1,    -1,    57,
      72,    73,    -1,    -1,    -1,    -1,    64,    65,    66,    29,
      30,    31,    -1,    33,    72,    35,    -1,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,
      -1,    51,    -1,    -1,    54,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    29,    30,    31,
      -1,    33,    72,    35,    -1,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    51,
      -1,    -1,    54,    -1,    -1,    57,    58,    -1,    -1,    -1,
      -1,    -1,    64,    65,    66,    29,    30,    31,    -1,    33,
      72,    35,    -1,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,
      54,    -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      64,    65,    66,    29,    30,    31,    -1,    33,    72,    35,
      -1,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    -1,    -1,    51,    -1,    -1,    54,    -1,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    64,    65,
      66,    29,    30,    31,    -1,    33,    72,    35,    -1,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    -1,    -1,    51,    -1,    -1,    54,    -1,    -1,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    64,    65,    66,    29,
      30,    31,    -1,    33,    72,    35,    -1,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,
      -1,    51,    -1,    -1,    54,    -1,    -1,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    64,    65,    66,    29,    30,    31,
      -1,    33,    72,    35,    -1,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    51,
      -1,    -1,    54,    -1,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    64,    65,    66,    29,    30,    31,    -1,    33,
      72,    35,    -1,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    -1,    -1,    51,    -1,    -1,
      54,    -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      64,    65,    66,    29,    30,    31,    -1,    33,    72,    35,
      -1,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    51,    -1,    -1,    54,    -1,
      56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    64,    65,
      66,    29,    30,    31,    -1,    33,    72,    35,    -1,    -1,
      33,    39,    35,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    44,    -1,    51,    -1,    -1,    54,    -1,    -1,    57,
      -1,    54,    55,    33,    34,    35,    64,    65,    66,    39,
      -1,    64,    65,    66,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    52,    53,    54,    -1,    33,    57,    35,    -1,
      60,    -1,    39,    -1,    64,    65,    66,    -1,    -1,    33,
      -1,    35,    72,    50,    -1,    39,    -1,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    -1,    50,    64,    65,    66,
      54,    55,    56,    57,    -1,    72,    -1,    33,    34,    35,
      64,    65,    66,    39,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    33,    -1,    35,    -1,    -1,    52,    39,    54,    -1,
      -1,    57,    -1,    -1,    60,    -1,    -1,    -1,    64,    65,
      66,    -1,    54,    55,    56,    57,    72,    -1,    -1,    33,
      -1,    35,    64,    65,    66,    39,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    33,    50,    35,    -1,    53,
      54,    39,    -1,    57,    -1,    43,    -1,    -1,    -1,    -1,
      64,    65,    66,    -1,    -1,    53,    54,    -1,    72,    57,
      -1,    -1,    -1,    33,    -1,    35,    64,    65,    66,    39,
      -1,    -1,    -1,    -1,    72,    -1,    33,    34,    35,    -1,
      50,    -1,    39,    53,    54,    -1,    -1,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    64,    65,    66,    54,    -1,    33,
      57,    35,    72,    60,    33,    39,    35,    64,    65,    66,
      39,    -1,    -1,    -1,    -1,    72,    50,    -1,    -1,    -1,
      54,    50,    -1,    57,    -1,    54,    -1,    -1,    57,    -1,
      64,    65,    66,    -1,    -1,    64,    65,    66,    72,    33,
      -1,    35,    -1,    72,    33,    39,    35,    -1,    -1,    43,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      54,    -1,    -1,    57,    53,    54,    -1,    33,    57,    35,
      64,    65,    66,    39,    -1,    64,    65,    66,    72,    -1,
      -1,    -1,    -1,    72,    50,    -1,    -1,    -1,    54,    -1,
      33,    57,    35,    -1,    -1,    -1,    39,    -1,    64,    65,
      66,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    52,
      -1,    54,    -1,    33,    57,    35,    -1,    -1,    33,    39,
      35,    64,    65,    66,    39,    -1,    41,    -1,    -1,    72,
      -1,    -1,    52,    -1,    54,    -1,    -1,    57,    -1,    54,
      -1,    33,    57,    35,    64,    65,    66,    39,    -1,    64,
      65,    66,    72,    -1,    33,    -1,    35,    72,    50,    -1,
      39,    -1,    54,    -1,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    64,    65,    66,    54,    -1,    33,    57,    35,
      72,    -1,    -1,    39,    -1,    64,    65,    66,    -1,    -1,
      33,    -1,    35,    72,    37,    -1,    39,    -1,    54,    -1,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    64,    65,
      66,    54,    55,    56,    -1,    -1,    72,    -1,    -1,    62,
      -1,    64,    65,    66
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     3,     4,     5,    75,     1,     6,    10,    14,
      23,    26,    27,    29,    30,    31,    33,    35,    37,    39,
      40,    41,    46,    48,    51,    54,    57,    64,    65,    66,
      72,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     198,   199,   219,   221,   223,   224,   225,   226,    35,    39,
      41,    54,   131,   133,   139,   221,   222,    62,    76,    77,
       0,   186,   186,    69,   163,    69,    69,    29,    30,    31,
      51,    54,    57,    72,   177,   178,   197,   219,   223,   226,
     193,   194,   199,    32,    34,    36,    38,    48,    52,    55,
      56,    60,    61,    73,   186,   193,   200,   201,   202,   220,
     221,   228,   230,   232,   233,   234,    59,   186,   200,   215,
      13,   162,    42,    32,    34,    36,    38,    48,    52,    61,
      73,   231,   232,   234,   236,   236,   199,    69,    45,    69,
      42,    56,   133,   134,   139,   142,    54,    57,    72,   140,
     141,   221,   222,    46,     1,    78,   222,    69,   237,     7,
      11,    58,   195,   164,   165,    15,    14,    58,   187,   209,
     210,   211,   209,   177,    29,    32,    34,    38,    48,    52,
      61,    73,   167,   168,   169,   170,   172,   173,   175,   176,
     178,   179,   184,   185,   219,   221,   223,    48,   167,   168,
     169,   180,    50,   177,    45,    69,    56,    56,    56,    56,
      56,    56,    35,    37,    39,   221,    56,    56,    55,    56,
     236,    55,    56,    47,    55,    56,    55,    56,    43,   188,
     189,   188,    44,    47,    55,   216,    59,   163,     1,    54,
     128,   130,   131,   135,   136,   138,   139,   141,   221,    48,
     193,   194,    48,   193,   194,    54,   212,   213,   214,   223,
     224,   199,   212,    54,   135,   139,    55,    56,    55,    56,
      55,    56,    50,    56,   136,   139,   142,   143,   144,   145,
     220,   221,    59,   135,   221,    54,    81,     8,     9,    16,
      17,    18,    19,    20,    21,    24,    25,    29,    54,    58,
      63,    67,    68,    80,    87,    89,    97,    98,   146,   149,
     153,   154,   155,   156,   169,   170,   173,   175,   223,    80,
      69,   186,    11,    12,    58,   196,     1,    41,    58,    70,
     153,   166,   238,    58,   238,   186,   163,   209,    49,   238,
      58,   238,    60,   234,     1,   174,   176,   177,    55,    56,
      56,    42,   234,   234,   177,    55,    56,    47,    55,    56,
     177,    43,    73,   234,    55,    59,   186,   177,   181,   182,
     183,   223,   224,    60,    60,    60,    60,   186,    56,   186,
     186,   202,   221,   186,    56,    56,   186,    14,   186,   217,
     218,   186,    47,    28,    54,    56,   127,   128,   132,   133,
     139,   142,   221,    50,    53,    50,   140,    50,   193,   194,
     193,   194,    36,    48,    70,    55,    43,    70,   133,   133,
     139,   133,   139,   133,   139,    56,    55,    56,    55,    56,
      55,    56,    55,    56,    47,    55,    56,    56,    42,    59,
      35,    39,    54,    55,    56,    62,    82,    83,   222,   224,
     226,    13,     1,   131,   139,     1,    35,    99,     1,    29,
     147,     1,   147,     1,   147,     1,    54,   114,   115,   223,
       1,   131,   139,     1,    54,     1,   116,   131,   139,     1,
     117,   131,   139,    48,    60,    73,   227,   230,   155,   156,
     169,   223,    80,     1,    31,    65,    79,   222,     1,    29,
      63,   223,    70,    58,    88,    58,    42,    55,     1,    42,
      43,    47,   157,   158,   159,   160,   177,   157,   227,   227,
      48,    73,   177,   229,   230,   238,    58,   167,   203,   204,
     205,   186,   186,    12,     1,    43,   186,   211,    48,   174,
     167,   135,    48,   174,    48,   174,   167,   167,   185,   221,
     167,    29,    48,   174,   167,    70,    55,    43,    56,   163,
      49,    55,    44,   217,   129,   221,   134,   139,   142,    56,
      53,    55,    56,    55,    56,   135,   135,   135,   135,   214,
     186,   135,   136,   139,   136,   139,   135,   135,   145,   221,
     135,    56,    79,    55,    56,    54,    71,     1,    69,    53,
      43,    43,   221,    60,   148,   227,   233,   235,   148,   148,
      42,    55,    31,    53,    43,   118,   119,   135,    47,   120,
      53,    13,   161,    53,   170,   223,    56,    56,    56,    79,
      54,    64,    66,    90,   223,   223,    88,    89,    98,   153,
       1,    28,   124,   125,   126,   128,   131,   137,   138,   139,
     223,   135,   186,   188,   161,   160,   170,   170,    29,   171,
     172,   223,   170,   203,     1,    47,    50,   206,   207,   208,
     238,    58,   186,   186,   174,   174,   174,    56,   174,   183,
     167,   186,   218,   186,    61,   221,    56,   135,    56,    56,
      83,    44,    55,    84,    85,    86,   224,   226,    80,     1,
      99,     1,    28,    52,    54,   102,   103,   105,   106,   128,
     138,   139,   225,   135,    55,   124,   115,    99,   103,   108,
      56,    55,     1,   121,   122,   123,   161,   139,    69,   150,
     139,   177,   177,   177,    66,    90,    35,    55,    91,    92,
      93,   223,    54,    79,    31,   223,    31,    97,   129,    50,
      53,    50,    50,   157,    43,   188,   186,   161,   208,   205,
     130,    56,    56,    56,    55,   238,    43,   129,   137,   138,
     139,    34,    22,    47,   111,    52,   128,   140,    60,   233,
     233,    52,   128,   233,    69,    15,   235,    43,   111,   135,
      55,    50,   221,   151,   152,    79,    54,    56,    55,    91,
      90,   223,    31,    42,   223,   223,    61,   126,   139,   126,
     126,   126,   186,    50,    86,   102,    61,   233,    56,    54,
     222,   103,   140,    52,   107,   128,   137,   107,   140,   107,
      70,   109,   110,   149,   223,   100,   101,   223,   108,   122,
     123,    58,   153,   238,    58,   238,    90,    44,    54,    55,
      94,    95,    96,   223,   225,    93,    56,    42,   223,   124,
      42,    42,   125,   186,   111,    54,   104,   105,   131,   139,
     107,   112,   113,   222,   137,    55,    70,    42,    55,    42,
     111,    56,    56,    55,   124,    42,   124,   124,    53,    56,
      55,   110,    52,   127,   135,   101,   124,    96,   124,   105,
     222,   135
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,    74,    75,    75,    75,    75,    76,    76,    76,    76,
      76,    77,    78,    79,    79,    80,    80,    80,    80,    80,
      81,    81,    81,    81,    81,    82,    82,    83,    83,    83,
      83,    83,    83,    84,    84,    84,    84,    85,    85,    86,
      86,    87,    87,    87,    88,    89,    89,    89,    89,    89,
      90,    90,    90,    91,    91,    91,    91,    92,    92,    93,
      93,    93,    93,    94,    94,    94,    94,    95,    95,    96,
      96,    97,    97,    97,    97,    97,    98,    98,    98,    98,
      98,    98,    98,    98,    98,    98,    98,    98,    98,    99,
      99,    99,   100,   100,   101,   101,   102,   102,   103,   103,
     104,   104,   105,   105,   105,   105,   105,   105,   105,   105,
     105,   106,   106,   106,   106,   106,   106,   107,   107,   107,
     108,   109,   109,   110,   110,   110,   111,   111,   111,   112,
     112,   113,   113,    98,   114,   114,   114,   115,   115,    98,
      98,    98,    98,    98,    98,    98,    98,    98,    98,    98,
     116,   116,   117,   117,   118,   118,   119,   119,   120,   120,
     121,   121,   122,   122,   123,   123,   124,   124,   125,   125,
     126,   126,   126,   126,   127,   127,   128,   128,   129,   129,
     130,   130,   131,   131,   131,   131,   131,   131,   132,   132,
     133,   133,   134,   134,   134,   134,   134,   135,   135,   136,
     136,   136,   136,   136,   137,   137,   138,   138,   139,   139,
     140,   140,   141,   141,   141,   141,   141,   141,   141,   141,
     141,   141,   141,   141,   141,   142,   142,   143,   143,   143,
     143,   144,   144,   145,   146,   146,   146,   146,   146,   146,
     146,   146,   147,   147,   148,   148,   149,   149,   150,   150,
     151,   151,   151,   152,   153,   153,   153,   153,   154,   154,
     154,   155,   155,   155,   155,   155,   156,   156,   156,   156,
     156,   157,   157,   158,   158,   159,   159,   160,   161,   161,
     162,   162,   163,   163,   164,   164,   164,   165,   166,   166,
     166,   167,   167,   168,   168,   169,   170,   170,   170,   171,
     171,   172,   172,   173,   173,   173,   173,   173,   173,   173,
     173,   173,   173,   174,   174,   175,   175,   176,   176,   177,
     177,   177,   178,   178,   178,   178,   178,   178,   178,   178,
     178,   178,   178,   178,   178,   179,   179,   180,   180,   181,
     181,   182,   182,   183,   183,   184,   184,   185,   186,   186,
     187,   187,   188,   188,   189,   189,   190,   190,   191,   191,
     191,   191,   191,   192,   192,   192,   192,   192,   193,   193,
     193,   193,   194,   194,   194,   195,   195,   196,   196,   197,
     197,   198,   198,   199,   199,   199,   199,   199,   199,   199,
     199,   199,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   199,   199,   199,   200,   200,   201,   201,   202,   203,
     203,   204,   204,   204,   205,   206,   206,   206,   207,   207,
     208,   209,   209,   210,   210,   210,   211,   211,   211,   212,
     212,   213,   213,   214,   214,   215,   215,   215,   215,   215,
     215,   215,   216,   216,   217,   217,   218,   218,   218,   219,
     219,   219,   219,   220,   220,   221,   221,   221,   221,   222,
     222,   223,   223,   223,   223,   223,   223,   224,   224,   224,
     225,   225,   226,   226,   226,   227,   227,   227,   228,   228,
     229,   229,   230,   230,   230,   230,   231,   231,   232,   232,
     232,   233,   233,   234,   234,   234,   235,   235,   236,   236,
     237,   238,   238
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     3,     2,     2,     1,     4,     4,     7,     5,
       2,     0,     1,     1,     1,     0,     2,     1,     2,     4,
       0,     2,     3,     3,     4,     3,     1,     1,     2,     1,
       4,     4,     2,     0,     1,     1,     2,     3,     1,     1,
       1,     3,     2,     1,     0,     3,     5,     6,     4,     2,
       0,     4,     3,     0,     1,     1,     2,     3,     1,     1,
       1,     4,     4,     0,     1,     1,     2,     3,     1,     1,
       1,     2,     3,     3,     1,     1,     4,     6,     2,     5,
       7,     2,     4,     2,     5,     7,     2,     2,     2,     2,
       1,     1,     3,     1,     3,     1,     3,     1,     4,     1,
       3,     1,     4,     3,     3,     3,     1,     1,     4,     3,
       1,     3,     2,     2,     3,     2,     3,     2,     1,     1,
       1,     3,     1,     3,     3,     4,     0,     2,     4,     0,
       1,     3,     1,     4,     3,     1,     1,     2,     1,     7,
       6,     8,     7,     7,     4,     3,     4,     2,     2,     2,
       3,     1,     3,     1,     0,     1,     3,     1,     0,     2,
       3,     1,     3,     1,     0,     2,     4,     1,     3,     1,
       3,     3,     3,     1,     4,     1,     3,     5,     2,     1,
       3,     1,     2,     1,     3,     3,     1,     3,     1,     3,
       3,     3,     3,     3,     3,     3,     1,     1,     1,     1,
       3,     3,     3,     1,     1,     1,     2,     1,     2,     1,
       1,     1,     1,     2,     3,     3,     3,     3,     3,     3,
       3,     5,     3,     2,     1,     3,     3,     3,     3,     3,
       3,     3,     1,     3,     3,     2,     3,     2,     3,     2,
       3,     3,     1,     0,     3,     1,     3,     1,     3,     3,
       0,     2,     2,     2,     1,     2,     4,     2,     1,     1,
       1,     3,     3,     3,     3,     3,     4,     4,     4,     2,
       2,     2,     1,     2,     1,     2,     1,     4,     0,     2,
       0,     2,     3,     3,     0,     2,     2,     2,     3,     2,
       1,     1,     1,     3,     1,     3,     1,     1,     1,     1,
       1,     1,     1,     2,     2,     3,     4,     3,     4,     3,
       4,     3,     4,     1,     1,     1,     1,     2,     2,     1,
       1,     1,     3,     1,     4,     1,     1,     1,     3,     3,
       3,     3,     2,     3,     5,     3,     3,     3,     1,     0,
       1,     3,     1,     3,     1,     3,     1,     3,     1,     1,
       3,     1,     1,     1,     1,     1,     1,     1,     4,     3,
       2,     4,     3,     4,     3,     2,     4,     3,     6,     4,
       4,     1,     4,     4,     4,     3,     2,     3,     2,     2,
       1,     2,     1,     1,     3,     2,     1,     1,     1,     4,
       4,     1,     1,     1,     1,     3,     3,     3,     5,     1,
       3,     4,     4,     4,     3,     3,     3,     1,     3,     1,
       2,     3,     2,     1,     3,     1,     2,     1,     2,     1,
       4,     1,     2,     3,     2,     1,     3,     2,     1,     0,
       1,     3,     1,     1,     3,     1,     1,     2,     3,     4,
       2,     5,     3,     2,     3,     1,     3,     1,     2,     1,
       2,     2,     3,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     3,     3,     3,     3,     1,     3,     1,
       1,     3,     1,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     1,     1,     1,     1,     1,     3,
       1,     1,     3,     1,     3,     1,     1,     1,     1,     1,
       0,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* start: EXPR exp lwherePart  */
#line 120 "parser.y"
                                        {inputExpr = letrec(yyvsp[0],yyvsp[-1]); sp-=2;}
#line 2813 "y.tab.c"
    break;

  case 3: /* start: CTXT context  */
#line 121 "parser.y"
                                        {inputContext = yyvsp[0];	    sp-=1;}
#line 2819 "y.tab.c"
    break;

  case 4: /* start: SCRIPT topModule  */
#line 122 "parser.y"
                                        {valDefns  = yyvsp[0];	    sp-=1;}
#line 2825 "y.tab.c"
    break;

  case 5: /* start: error  */
#line 123 "parser.y"
                                        {syntaxError("input");}
#line 2831 "y.tab.c"
    break;

  case 6: /* topModule: startMain begin modBody end  */
#line 136 "parser.y"
                                        {
					 setExportList(singleton(ap(MODULEENT,mkCon(module(currentModule).text))));
					 yyval = gc3(yyvsp[-1]);
					}
#line 2840 "y.tab.c"
    break;

  case 7: /* topModule: startMain '{' modBody '}'  */
#line 140 "parser.y"
                                        {
					 setExportList(singleton(ap(MODULEENT,mkCon(module(currentModule).text))));
					 yyval = gc4(yyvsp[-1]);
					}
#line 2849 "y.tab.c"
    break;

  case 8: /* topModule: TMODULE modname expspec WHERE '{' modBody end  */
#line 145 "parser.y"
                                        {setExportList(yyvsp[-4]);   yyval = gc7(yyvsp[-1]);}
#line 2855 "y.tab.c"
    break;

  case 9: /* topModule: TMODULE modname expspec WHERE error  */
#line 147 "parser.y"
                                        {syntaxError("declaration");}
#line 2861 "y.tab.c"
    break;

  case 10: /* topModule: TMODULE error  */
#line 148 "parser.y"
                                        {syntaxError("module definition");}
#line 2867 "y.tab.c"
    break;

  case 11: /* startMain: %empty  */
#line 154 "parser.y"
                                        {startModule(conMain); 
					 yyval = gc0(NIL);}
#line 2874 "y.tab.c"
    break;

  case 12: /* modname: qconid  */
#line 157 "parser.y"
                                        {startModule(mkCon(mkNestedQual(yyvsp[0]))); yyval = gc1(NIL);}
#line 2880 "y.tab.c"
    break;

  case 13: /* modid: qconid  */
#line 159 "parser.y"
                                        {yyval = mkCon(mkNestedQual(yyvsp[0]));}
#line 2886 "y.tab.c"
    break;

  case 14: /* modid: STRINGLIT  */
#line 160 "parser.y"
                                        { String modName = findPathname(textToStr(textOf(yyvsp[0])));
					  if (modName) { /* fillin pathname if known */
					      yyval = mkStr(findText(modName));
					  } else {
					      yyval = yyvsp[0];
					  }
					}
#line 2898 "y.tab.c"
    break;

  case 15: /* modBody: %empty  */
#line 168 "parser.y"
                                        {yyval = gc0(NIL); }
#line 2904 "y.tab.c"
    break;

  case 16: /* modBody: ';' modBody  */
#line 169 "parser.y"
                                        {yyval = gc2(yyvsp[0]);}
#line 2910 "y.tab.c"
    break;

  case 17: /* modBody: topDecls  */
#line 170 "parser.y"
                                        {yyval = gc1(yyvsp[0]);}
#line 2916 "y.tab.c"
    break;

  case 18: /* modBody: impDecls chase  */
#line 171 "parser.y"
                                        {yyval = gc2(NIL);}
#line 2922 "y.tab.c"
    break;

  case 19: /* modBody: impDecls ';' chase topDecls  */
#line 172 "parser.y"
                                        {yyval = gc4(yyvsp[0]);}
#line 2928 "y.tab.c"
    break;

  case 20: /* expspec: %empty  */
#line 177 "parser.y"
                                        {yyval = gc0(exportSelf());}
#line 2934 "y.tab.c"
    break;

  case 21: /* expspec: '(' ')'  */
#line 178 "parser.y"
                                        {yyval = gc2(NIL);}
#line 2940 "y.tab.c"
    break;

  case 22: /* expspec: '(' ',' ')'  */
#line 179 "parser.y"
                                        {yyval = gc3(NIL);}
#line 2946 "y.tab.c"
    break;

  case 23: /* expspec: '(' exports ')'  */
#line 180 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 2952 "y.tab.c"
    break;

  case 24: /* expspec: '(' exports ',' ')'  */
#line 181 "parser.y"
                                        {yyval = gc4(yyvsp[-2]);}
#line 2958 "y.tab.c"
    break;

  case 25: /* exports: exports ',' export  */
#line 183 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 2964 "y.tab.c"
    break;

  case 26: /* exports: export  */
#line 184 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 2970 "y.tab.c"
    break;

  case 27: /* export: qvar  */
#line 189 "parser.y"
                                        {yyval = yyvsp[0];}
#line 2976 "y.tab.c"
    break;

  case 28: /* export: qvar '#'  */
#line 191 "parser.y"
                                        {yyval = yyvsp[-1];}
#line 2982 "y.tab.c"
    break;

  case 29: /* export: qcon  */
#line 192 "parser.y"
                                        {yyval = yyvsp[0];}
#line 2988 "y.tab.c"
    break;

  case 30: /* export: qconid '(' UPTO ')'  */
#line 193 "parser.y"
                                        {yyval = gc4(pair(yyvsp[-3],DOTDOT));}
#line 2994 "y.tab.c"
    break;

  case 31: /* export: qconid '(' qnames ')'  */
#line 194 "parser.y"
                                        {yyval = gc4(pair(yyvsp[-3],yyvsp[-1]));}
#line 3000 "y.tab.c"
    break;

  case 32: /* export: TMODULE modid  */
#line 195 "parser.y"
                                        {yyval = gc2(ap(MODULEENT,yyvsp[0]));}
#line 3006 "y.tab.c"
    break;

  case 33: /* qnames: %empty  */
#line 197 "parser.y"
                                        {yyval = gc0(NIL);}
#line 3012 "y.tab.c"
    break;

  case 34: /* qnames: ','  */
#line 198 "parser.y"
                                        {yyval = gc1(NIL);}
#line 3018 "y.tab.c"
    break;

  case 35: /* qnames: qnames1  */
#line 199 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3024 "y.tab.c"
    break;

  case 36: /* qnames: qnames1 ','  */
#line 200 "parser.y"
                                        {yyval = gc2(yyvsp[-1]);}
#line 3030 "y.tab.c"
    break;

  case 37: /* qnames1: qnames1 ',' qname  */
#line 202 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 3036 "y.tab.c"
    break;

  case 38: /* qnames1: qname  */
#line 203 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 3042 "y.tab.c"
    break;

  case 39: /* qname: qvar  */
#line 205 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3048 "y.tab.c"
    break;

  case 40: /* qname: qcon  */
#line 206 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3054 "y.tab.c"
    break;

  case 41: /* impDecls: impDecls ';' impDecl  */
#line 211 "parser.y"
                                        {imps = cons(yyvsp[0],imps); yyval=gc3(NIL);}
#line 3060 "y.tab.c"
    break;

  case 42: /* impDecls: impDecls ';'  */
#line 212 "parser.y"
                                        {yyval   = gc2(NIL); }
#line 3066 "y.tab.c"
    break;

  case 43: /* impDecls: impDecl  */
#line 213 "parser.y"
                                        {imps = singleton(yyvsp[0]); yyval=gc1(NIL);}
#line 3072 "y.tab.c"
    break;

  case 44: /* chase: %empty  */
#line 215 "parser.y"
                                        {if (chase(imps)) {
					     clearStack();
					     onto(imps);
					     done();
					     closeAnyInput();
					     return 0;
					 }
					 yyval = gc0(NIL);
					}
#line 3086 "y.tab.c"
    break;

  case 45: /* impDecl: IMPORT modid impspec  */
#line 226 "parser.y"
                                        {addUnqualImport(yyvsp[-1],NIL,yyvsp[0]);
					 yyval = gc3(yyvsp[-1]);}
#line 3093 "y.tab.c"
    break;

  case 46: /* impDecl: IMPORT modid ASMOD modid impspec  */
#line 229 "parser.y"
                                        {addUnqualImport(yyvsp[-3],yyvsp[-1],yyvsp[0]);
					 yyval = gc5(yyvsp[-3]);}
#line 3100 "y.tab.c"
    break;

  case 47: /* impDecl: IMPORT QUALIFIED modid ASMOD modid impspec  */
#line 232 "parser.y"
                                        {addQualImport(yyvsp[-3],yyvsp[-1],yyvsp[0]);
					 yyval = gc6(yyvsp[-3]);}
#line 3107 "y.tab.c"
    break;

  case 48: /* impDecl: IMPORT QUALIFIED modid impspec  */
#line 235 "parser.y"
                                        {addQualImport(yyvsp[-1],yyvsp[-1],yyvsp[0]);
					 yyval = gc4(yyvsp[-1]);}
#line 3114 "y.tab.c"
    break;

  case 49: /* impDecl: IMPORT error  */
#line 237 "parser.y"
                                        {syntaxError("import declaration");}
#line 3120 "y.tab.c"
    break;

  case 50: /* impspec: %empty  */
#line 239 "parser.y"
                                        {yyval = gc0(DOTDOT);}
#line 3126 "y.tab.c"
    break;

  case 51: /* impspec: HIDING '(' imports ')'  */
#line 240 "parser.y"
                                        {yyval = gc4(ap(HIDDEN,yyvsp[-1]));}
#line 3132 "y.tab.c"
    break;

  case 52: /* impspec: '(' imports ')'  */
#line 241 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 3138 "y.tab.c"
    break;

  case 53: /* imports: %empty  */
#line 243 "parser.y"
                                        {yyval = gc0(NIL);}
#line 3144 "y.tab.c"
    break;

  case 54: /* imports: ','  */
#line 244 "parser.y"
                                        {yyval = gc1(NIL);}
#line 3150 "y.tab.c"
    break;

  case 55: /* imports: imports1  */
#line 245 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3156 "y.tab.c"
    break;

  case 56: /* imports: imports1 ','  */
#line 246 "parser.y"
                                        {yyval = gc2(yyvsp[-1]);}
#line 3162 "y.tab.c"
    break;

  case 57: /* imports1: imports1 ',' import  */
#line 248 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 3168 "y.tab.c"
    break;

  case 58: /* imports1: import  */
#line 249 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 3174 "y.tab.c"
    break;

  case 59: /* import: var  */
#line 251 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3180 "y.tab.c"
    break;

  case 60: /* import: CONID  */
#line 252 "parser.y"
                                        {yyval = gc1(pair(yyvsp[0],NONE));}
#line 3186 "y.tab.c"
    break;

  case 61: /* import: CONID '(' UPTO ')'  */
#line 253 "parser.y"
                                        {yyval = gc4(pair(yyvsp[-3],DOTDOT));}
#line 3192 "y.tab.c"
    break;

  case 62: /* import: CONID '(' names ')'  */
#line 254 "parser.y"
                                        {yyval = gc4(pair(yyvsp[-3],yyvsp[-1]));}
#line 3198 "y.tab.c"
    break;

  case 63: /* names: %empty  */
#line 256 "parser.y"
                                        {yyval = gc0(NIL);}
#line 3204 "y.tab.c"
    break;

  case 64: /* names: ','  */
#line 257 "parser.y"
                                        {yyval = gc1(NIL);}
#line 3210 "y.tab.c"
    break;

  case 65: /* names: names1  */
#line 258 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3216 "y.tab.c"
    break;

  case 66: /* names: names1 ','  */
#line 259 "parser.y"
                                        {yyval = gc2(yyvsp[-1]);}
#line 3222 "y.tab.c"
    break;

  case 67: /* names1: names1 ',' name  */
#line 261 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 3228 "y.tab.c"
    break;

  case 68: /* names1: name  */
#line 262 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 3234 "y.tab.c"
    break;

  case 69: /* name: var  */
#line 264 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3240 "y.tab.c"
    break;

  case 70: /* name: con  */
#line 265 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3246 "y.tab.c"
    break;

  case 71: /* topDecls: topDecls ';'  */
#line 270 "parser.y"
                                        {yyval = gc2(yyvsp[-1]);}
#line 3252 "y.tab.c"
    break;

  case 72: /* topDecls: topDecls ';' topDecl  */
#line 271 "parser.y"
                                        {yyval = gc2(yyvsp[-2]);}
#line 3258 "y.tab.c"
    break;

  case 73: /* topDecls: topDecls ';' decl  */
#line 272 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 3264 "y.tab.c"
    break;

  case 74: /* topDecls: topDecl  */
#line 273 "parser.y"
                                        {yyval = gc0(NIL);}
#line 3270 "y.tab.c"
    break;

  case 75: /* topDecls: decl  */
#line 274 "parser.y"
                                        {yyval = gc1(cons(yyvsp[0],NIL));}
#line 3276 "y.tab.c"
    break;

  case 76: /* topDecl: TYPE tyLhs '=' type  */
#line 279 "parser.y"
                                        {defTycon(4,yyvsp[-1],yyvsp[-2],yyvsp[0],SYNONYM);}
#line 3282 "y.tab.c"
    break;

  case 77: /* topDecl: TYPE tyLhs '=' type IN invars  */
#line 281 "parser.y"
                                        {defTycon(6,yyvsp[-3],yyvsp[-4],
						    ap(yyvsp[-2],yyvsp[0]),RESTRICTSYN);}
#line 3289 "y.tab.c"
    break;

  case 78: /* topDecl: TYPE error  */
#line 283 "parser.y"
                                        {syntaxError("type declaration");}
#line 3295 "y.tab.c"
    break;

  case 79: /* topDecl: DATA btype2 '=' constrs deriving  */
#line 285 "parser.y"
                                        {defTycon(5,yyvsp[-2],checkTyLhs(yyvsp[-3]),
						    ap(rev(yyvsp[-1]),yyvsp[0]),DATATYPE);}
#line 3302 "y.tab.c"
    break;

  case 80: /* topDecl: DATA context IMPLIES tyLhs '=' constrs deriving  */
#line 288 "parser.y"
                                        {defTycon(7,yyvsp[-2],yyvsp[-3],
						  ap(qualify(yyvsp[-5],rev(yyvsp[-1])),
						     yyvsp[0]),DATATYPE);}
#line 3310 "y.tab.c"
    break;

  case 81: /* topDecl: DATA btype2  */
#line 291 "parser.y"
                                        {defTycon(2,yyvsp[-1],checkTyLhs(yyvsp[0]),
						    ap(NIL,NIL),DATATYPE);}
#line 3317 "y.tab.c"
    break;

  case 82: /* topDecl: DATA context IMPLIES tyLhs  */
#line 293 "parser.y"
                                        {defTycon(4,yyvsp[-3],yyvsp[0],
						  ap(qualify(yyvsp[-2],NIL),
						     NIL),DATATYPE);}
#line 3325 "y.tab.c"
    break;

  case 83: /* topDecl: DATA error  */
#line 296 "parser.y"
                                        {syntaxError("data declaration");}
#line 3331 "y.tab.c"
    break;

  case 84: /* topDecl: TNEWTYPE btype2 '=' nconstr deriving  */
#line 298 "parser.y"
                                        {defTycon(5,yyvsp[-2],checkTyLhs(yyvsp[-3]),
						    ap(yyvsp[-1],yyvsp[0]),NEWTYPE);}
#line 3338 "y.tab.c"
    break;

  case 85: /* topDecl: TNEWTYPE context IMPLIES tyLhs '=' nconstr deriving  */
#line 301 "parser.y"
                                        {defTycon(7,yyvsp[-2],yyvsp[-3],
						  ap(qualify(yyvsp[-5],yyvsp[-1]),
						     yyvsp[0]),NEWTYPE);}
#line 3346 "y.tab.c"
    break;

  case 86: /* topDecl: TNEWTYPE error  */
#line 304 "parser.y"
                                        {syntaxError("newtype declaration");}
#line 3352 "y.tab.c"
    break;

  case 87: /* topDecl: NEEDPRIMS NUMLIT  */
#line 305 "parser.y"
                                        {if (isInt(yyvsp[0])) {
					     needPrims(intOf(yyvsp[0]), NULL);
					 } else {
					     syntaxError("needprims decl");
					 }
					 sp-=2;}
#line 3363 "y.tab.c"
    break;

  case 88: /* topDecl: NEEDPRIMS error  */
#line 311 "parser.y"
                                        {syntaxError("needprims decl");}
#line 3369 "y.tab.c"
    break;

  case 89: /* tyLhs: tyLhs varid  */
#line 313 "parser.y"
                                        {yyval = gc2(ap(yyvsp[-1],yyvsp[0]));}
#line 3375 "y.tab.c"
    break;

  case 90: /* tyLhs: CONID  */
#line 314 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3381 "y.tab.c"
    break;

  case 91: /* tyLhs: error  */
#line 315 "parser.y"
                                        {syntaxError("type defn lhs");}
#line 3387 "y.tab.c"
    break;

  case 92: /* invars: invars ',' invar  */
#line 317 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 3393 "y.tab.c"
    break;

  case 93: /* invars: invar  */
#line 318 "parser.y"
                                        {yyval = gc1(cons(yyvsp[0],NIL));}
#line 3399 "y.tab.c"
    break;

  case 94: /* invar: var COCO topType  */
#line 320 "parser.y"
                                        {yyval = gc3(sigdecl(yyvsp[-1],singleton(yyvsp[-2]),
									yyvsp[0]));}
#line 3406 "y.tab.c"
    break;

  case 95: /* invar: var  */
#line 322 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3412 "y.tab.c"
    break;

  case 96: /* constrs: constrs '|' pconstr  */
#line 324 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 3418 "y.tab.c"
    break;

  case 97: /* constrs: pconstr  */
#line 325 "parser.y"
                                        {yyval = gc1(cons(yyvsp[0],NIL));}
#line 3424 "y.tab.c"
    break;

  case 98: /* pconstr: ALL varids '.' qconstr  */
#line 327 "parser.y"
                                        {yyval = gc4(ap(POLYTYPE,
						     pair(rev(yyvsp[-2]),yyvsp[0])));}
#line 3431 "y.tab.c"
    break;

  case 99: /* pconstr: constr  */
#line 329 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3437 "y.tab.c"
    break;

  case 100: /* qconstr: context IMPLIES constr  */
#line 331 "parser.y"
                                        {yyval = gc3(qualify(yyvsp[-2],yyvsp[0]));}
#line 3443 "y.tab.c"
    break;

  case 101: /* qconstr: constr  */
#line 332 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3449 "y.tab.c"
    break;

  case 102: /* constr: '!' btype conop bbtype  */
#line 334 "parser.y"
                                        {yyval = gc4(ap(ap(yyvsp[-1],bang(yyvsp[-2])),yyvsp[0]));}
#line 3455 "y.tab.c"
    break;

  case 103: /* constr: btype1 conop bbtype  */
#line 335 "parser.y"
                                        {yyval = gc3(ap(ap(yyvsp[-1],yyvsp[-2]),yyvsp[0]));}
#line 3461 "y.tab.c"
    break;

  case 104: /* constr: btype2 conop bbtype  */
#line 336 "parser.y"
                                        {yyval = gc3(ap(ap(yyvsp[-1],yyvsp[-2]),yyvsp[0]));}
#line 3467 "y.tab.c"
    break;

  case 105: /* constr: bpolyType conop bbtype  */
#line 337 "parser.y"
                                        {yyval = gc3(ap(ap(yyvsp[-1],yyvsp[-2]),yyvsp[0]));}
#line 3473 "y.tab.c"
    break;

  case 106: /* constr: btype2  */
#line 338 "parser.y"
                                        {yyval = checkConstr(yyvsp[0]);}
#line 3479 "y.tab.c"
    break;

  case 107: /* constr: btype3  */
#line 339 "parser.y"
                                        {yyval = checkConstr(yyvsp[0]);}
#line 3485 "y.tab.c"
    break;

  case 108: /* constr: con '{' fieldspecs '}'  */
#line 340 "parser.y"
                                        {yyval = gc4(ap(LABC,pair(yyvsp[-3],rev(yyvsp[-1]))));}
#line 3491 "y.tab.c"
    break;

  case 109: /* constr: con '{' '}'  */
#line 341 "parser.y"
                                        {yyval = gc3(ap(LABC,pair(yyvsp[-2],NIL)));}
#line 3497 "y.tab.c"
    break;

  case 110: /* constr: error  */
#line 342 "parser.y"
                                        {syntaxError("data type declaration");}
#line 3503 "y.tab.c"
    break;

  case 111: /* btype3: btype2 '!' atype  */
#line 344 "parser.y"
                                        {yyval = gc3(ap(yyvsp[-2],bang(yyvsp[0])));}
#line 3509 "y.tab.c"
    break;

  case 112: /* btype3: btype2 bpolyType  */
#line 345 "parser.y"
                                        {yyval = gc2(ap(yyvsp[-1],yyvsp[0]));}
#line 3515 "y.tab.c"
    break;

  case 113: /* btype3: btype3 atype  */
#line 346 "parser.y"
                                        {yyval = gc2(ap(yyvsp[-1],yyvsp[0]));}
#line 3521 "y.tab.c"
    break;

  case 114: /* btype3: btype3 '!' atype  */
#line 347 "parser.y"
                                        {yyval = gc3(ap(yyvsp[-2],bang(yyvsp[0])));}
#line 3527 "y.tab.c"
    break;

  case 115: /* btype3: btype3 bpolyType  */
#line 348 "parser.y"
                                        {yyval = gc2(ap(yyvsp[-1],yyvsp[0]));}
#line 3533 "y.tab.c"
    break;

  case 116: /* btype3: '(' CONOP ')'  */
#line 349 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 3539 "y.tab.c"
    break;

  case 117: /* bbtype: '!' btype  */
#line 351 "parser.y"
                                        {yyval = gc2(bang(yyvsp[0]));}
#line 3545 "y.tab.c"
    break;

  case 118: /* bbtype: btype  */
#line 352 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3551 "y.tab.c"
    break;

  case 119: /* bbtype: bpolyType  */
#line 353 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3557 "y.tab.c"
    break;

  case 120: /* nconstr: pconstr  */
#line 355 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 3563 "y.tab.c"
    break;

  case 121: /* fieldspecs: fieldspecs ',' fieldspec  */
#line 357 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 3569 "y.tab.c"
    break;

  case 122: /* fieldspecs: fieldspec  */
#line 358 "parser.y"
                                        {yyval = gc1(cons(yyvsp[0],NIL));}
#line 3575 "y.tab.c"
    break;

  case 123: /* fieldspec: vars COCO polyType  */
#line 360 "parser.y"
                                        {yyval = gc3(pair(rev(yyvsp[-2]),yyvsp[0]));}
#line 3581 "y.tab.c"
    break;

  case 124: /* fieldspec: vars COCO type  */
#line 361 "parser.y"
                                        {yyval = gc3(pair(rev(yyvsp[-2]),yyvsp[0]));}
#line 3587 "y.tab.c"
    break;

  case 125: /* fieldspec: vars COCO '!' type  */
#line 362 "parser.y"
                                        {yyval = gc4(pair(rev(yyvsp[-3]),bang(yyvsp[0])));}
#line 3593 "y.tab.c"
    break;

  case 126: /* deriving: %empty  */
#line 364 "parser.y"
                                        {yyval = gc0(NIL);}
#line 3599 "y.tab.c"
    break;

  case 127: /* deriving: DERIVING qconid  */
#line 365 "parser.y"
                                        {yyval = gc2(singleton(yyvsp[0]));}
#line 3605 "y.tab.c"
    break;

  case 128: /* deriving: DERIVING '(' derivs0 ')'  */
#line 366 "parser.y"
                                        {yyval = gc4(yyvsp[-1]);}
#line 3611 "y.tab.c"
    break;

  case 129: /* derivs0: %empty  */
#line 368 "parser.y"
                                        {yyval = gc0(NIL);}
#line 3617 "y.tab.c"
    break;

  case 130: /* derivs0: derivs  */
#line 369 "parser.y"
                                        {yyval = gc1(rev(yyvsp[0]));}
#line 3623 "y.tab.c"
    break;

  case 131: /* derivs: derivs ',' qconid  */
#line 371 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 3629 "y.tab.c"
    break;

  case 132: /* derivs: qconid  */
#line 372 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 3635 "y.tab.c"
    break;

  case 133: /* topDecl: PRIMITIVE prims COCO topType  */
#line 377 "parser.y"
                                        {primDefn(yyvsp[-3],yyvsp[-2],yyvsp[0]); sp-=4;}
#line 3641 "y.tab.c"
    break;

  case 134: /* prims: prims ',' prim  */
#line 379 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 3647 "y.tab.c"
    break;

  case 135: /* prims: prim  */
#line 380 "parser.y"
                                        {yyval = gc1(cons(yyvsp[0],NIL));}
#line 3653 "y.tab.c"
    break;

  case 136: /* prims: error  */
#line 381 "parser.y"
                                        {syntaxError("primitive defn");}
#line 3659 "y.tab.c"
    break;

  case 137: /* prim: var STRINGLIT  */
#line 383 "parser.y"
                                        {yyval = gc2(pair(yyvsp[-1],yyvsp[0]));}
#line 3665 "y.tab.c"
    break;

  case 138: /* prim: var  */
#line 384 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3671 "y.tab.c"
    break;

  case 139: /* topDecl: FOREIGN IMPORT var STRINGLIT var COCO topType  */
#line 390 "parser.y"
               {foreignImport(yyvsp[-6],yyvsp[-4],NIL,yyvsp[-3],yyvsp[-2],yyvsp[0]); sp-=7;}
#line 3677 "y.tab.c"
    break;

  case 140: /* topDecl: FOREIGN IMPORT var var COCO topType  */
#line 392 "parser.y"
               {foreignImport(yyvsp[-5],yyvsp[-3],NIL,yyvsp[-2],yyvsp[-2],yyvsp[0]); sp-=6;}
#line 3683 "y.tab.c"
    break;

  case 141: /* topDecl: FOREIGN IMPORT var var STRINGLIT var COCO topType  */
#line 394 "parser.y"
               {foreignImport(yyvsp[-7],yyvsp[-5],yyvsp[-4],yyvsp[-3],yyvsp[-2],yyvsp[0]); sp-=8;}
#line 3689 "y.tab.c"
    break;

  case 142: /* topDecl: FOREIGN IMPORT var var var COCO topType  */
#line 396 "parser.y"
               {foreignImport(yyvsp[-6],yyvsp[-4],yyvsp[-3],yyvsp[-2],yyvsp[-2],yyvsp[0]); sp-=7;}
#line 3695 "y.tab.c"
    break;

  case 143: /* topDecl: FOREIGN var var STRINGLIT var COCO topType  */
#line 398 "parser.y"
               {foreignExport(yyvsp[-6],yyvsp[-5],yyvsp[-4],yyvsp[-3],yyvsp[-2],yyvsp[0]); sp-=7;}
#line 3701 "y.tab.c"
    break;

  case 144: /* topDecl: TCLASS crule fds wherePart  */
#line 403 "parser.y"
                                        {classDefn(intOf(yyvsp[-3]),yyvsp[-2],yyvsp[0],yyvsp[-1]); sp-=4;}
#line 3707 "y.tab.c"
    break;

  case 145: /* topDecl: TINSTANCE irule wherePart  */
#line 404 "parser.y"
                                        {instDefn(intOf(yyvsp[-2]),yyvsp[-1],yyvsp[0]);  sp-=3;}
#line 3713 "y.tab.c"
    break;

  case 146: /* topDecl: DEFAULT '(' dtypes ')'  */
#line 405 "parser.y"
                                        {defaultDefn(intOf(yyvsp[-3]),yyvsp[-1]);  sp-=4;}
#line 3719 "y.tab.c"
    break;

  case 147: /* topDecl: TCLASS error  */
#line 406 "parser.y"
                                        {syntaxError("class declaration");}
#line 3725 "y.tab.c"
    break;

  case 148: /* topDecl: TINSTANCE error  */
#line 407 "parser.y"
                                        {syntaxError("instance declaration");}
#line 3731 "y.tab.c"
    break;

  case 149: /* topDecl: DEFAULT error  */
#line 408 "parser.y"
                                        {syntaxError("default declaration");}
#line 3737 "y.tab.c"
    break;

  case 150: /* crule: context IMPLIES btype2  */
#line 410 "parser.y"
                                        {yyval = gc3(pair(yyvsp[-2],checkPred(yyvsp[0])));}
#line 3743 "y.tab.c"
    break;

  case 151: /* crule: btype2  */
#line 411 "parser.y"
                                        {yyval = gc1(pair(NIL,checkPred(yyvsp[0])));}
#line 3749 "y.tab.c"
    break;

  case 152: /* irule: context IMPLIES btype2  */
#line 413 "parser.y"
                                        {yyval = gc3(pair(yyvsp[-2],checkPred(yyvsp[0])));}
#line 3755 "y.tab.c"
    break;

  case 153: /* irule: btype2  */
#line 414 "parser.y"
                                        {yyval = gc1(pair(NIL,checkPred(yyvsp[0])));}
#line 3761 "y.tab.c"
    break;

  case 154: /* dtypes: %empty  */
#line 416 "parser.y"
                                        {yyval = gc0(NIL);}
#line 3767 "y.tab.c"
    break;

  case 155: /* dtypes: dtypes1  */
#line 417 "parser.y"
                                        {yyval = gc1(rev(yyvsp[0]));}
#line 3773 "y.tab.c"
    break;

  case 156: /* dtypes1: dtypes1 ',' type  */
#line 419 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 3779 "y.tab.c"
    break;

  case 157: /* dtypes1: type  */
#line 420 "parser.y"
                                        {yyval = gc1(cons(yyvsp[0],NIL));}
#line 3785 "y.tab.c"
    break;

  case 158: /* fds: %empty  */
#line 422 "parser.y"
                                        {yyval = gc0(NIL);}
#line 3791 "y.tab.c"
    break;

  case 159: /* fds: '|' fds1  */
#line 423 "parser.y"
                                        {h98DoesntSupport(row,"dependent parameters");
					 yyval = gc2(rev(yyvsp[0]));}
#line 3798 "y.tab.c"
    break;

  case 160: /* fds1: fds1 ',' fd  */
#line 426 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 3804 "y.tab.c"
    break;

  case 161: /* fds1: fd  */
#line 427 "parser.y"
                                        {yyval = gc1(cons(yyvsp[0],NIL));}
#line 3810 "y.tab.c"
    break;

  case 162: /* fd: varids0 ARROW varids0  */
#line 429 "parser.y"
                                        {yyval = gc3(pair(rev(yyvsp[-2]),rev(yyvsp[0])));}
#line 3816 "y.tab.c"
    break;

  case 163: /* fd: error  */
#line 430 "parser.y"
                                        {syntaxError("functional dependency");}
#line 3822 "y.tab.c"
    break;

  case 164: /* varids0: %empty  */
#line 432 "parser.y"
                                        {yyval = gc0(NIL);}
#line 3828 "y.tab.c"
    break;

  case 165: /* varids0: varids0 varid  */
#line 433 "parser.y"
                                        {yyval = gc2(cons(yyvsp[0],yyvsp[-1]));}
#line 3834 "y.tab.c"
    break;

  case 166: /* topType: ALL varids '.' topType0  */
#line 438 "parser.y"
                                        {yyval = gc4(ap(POLYTYPE,
						     pair(rev(yyvsp[-2]),yyvsp[0])));}
#line 3841 "y.tab.c"
    break;

  case 167: /* topType: topType0  */
#line 440 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3847 "y.tab.c"
    break;

  case 168: /* topType0: context IMPLIES topType1  */
#line 442 "parser.y"
                                        {yyval = gc3(qualify(yyvsp[-2],yyvsp[0]));}
#line 3853 "y.tab.c"
    break;

  case 169: /* topType0: topType1  */
#line 443 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3859 "y.tab.c"
    break;

  case 170: /* topType1: bpolyType ARROW topType1  */
#line 445 "parser.y"
                                        {yyval = gc3(fn(yyvsp[-2],yyvsp[0]));}
#line 3865 "y.tab.c"
    break;

  case 171: /* topType1: btype1 ARROW topType1  */
#line 446 "parser.y"
                                        {yyval = gc3(fn(yyvsp[-2],yyvsp[0]));}
#line 3871 "y.tab.c"
    break;

  case 172: /* topType1: btype2 ARROW topType1  */
#line 447 "parser.y"
                                        {yyval = gc3(fn(yyvsp[-2],yyvsp[0]));}
#line 3877 "y.tab.c"
    break;

  case 173: /* topType1: btype  */
#line 448 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3883 "y.tab.c"
    break;

  case 174: /* polyType: ALL varids '.' sigType  */
#line 450 "parser.y"
                                        {yyval = gc4(ap(POLYTYPE,
						     pair(rev(yyvsp[-2]),yyvsp[0])));}
#line 3890 "y.tab.c"
    break;

  case 175: /* polyType: bpolyType  */
#line 452 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3896 "y.tab.c"
    break;

  case 176: /* bpolyType: '(' polyType ')'  */
#line 454 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 3902 "y.tab.c"
    break;

  case 177: /* bpolyType: '(' lcontext IMPLIES type ')'  */
#line 455 "parser.y"
                                          {yyval = gc5(qualify(yyvsp[-3],yyvsp[-1]));}
#line 3908 "y.tab.c"
    break;

  case 178: /* varids: varids varid  */
#line 457 "parser.y"
                                        {yyval = gc2(cons(yyvsp[0],yyvsp[-1]));}
#line 3914 "y.tab.c"
    break;

  case 179: /* varids: varid  */
#line 458 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 3920 "y.tab.c"
    break;

  case 180: /* sigType: context IMPLIES type  */
#line 460 "parser.y"
                                        {yyval = gc3(qualify(yyvsp[-2],yyvsp[0]));}
#line 3926 "y.tab.c"
    break;

  case 181: /* sigType: type  */
#line 461 "parser.y"
                                        {yyval = yyvsp[0];}
#line 3932 "y.tab.c"
    break;

  case 182: /* context: '(' ')'  */
#line 463 "parser.y"
                                        {yyval = gc2(NIL);}
#line 3938 "y.tab.c"
    break;

  case 183: /* context: btype2  */
#line 464 "parser.y"
                                        {yyval = gc1(singleton(checkPred(yyvsp[0])));}
#line 3944 "y.tab.c"
    break;

  case 184: /* context: '(' btype2 ')'  */
#line 465 "parser.y"
                                        {yyval = gc3(singleton(checkPred(yyvsp[-1])));}
#line 3950 "y.tab.c"
    break;

  case 185: /* context: '(' btypes2 ')'  */
#line 466 "parser.y"
                                        {yyval = gc3(checkCtxt(rev(yyvsp[-1])));}
#line 3956 "y.tab.c"
    break;

  case 186: /* context: lacks  */
#line 467 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 3962 "y.tab.c"
    break;

  case 187: /* context: '(' lacks1 ')'  */
#line 468 "parser.y"
                                        {yyval = gc3(checkCtxt(rev(yyvsp[-1])));}
#line 3968 "y.tab.c"
    break;

  case 188: /* lcontext: lacks  */
#line 470 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 3974 "y.tab.c"
    break;

  case 189: /* lcontext: '(' lacks1 ')'  */
#line 471 "parser.y"
                                        {yyval = gc3(checkCtxt(rev(yyvsp[-1])));}
#line 3980 "y.tab.c"
    break;

  case 190: /* lacks: varid '\\' varid  */
#line 473 "parser.y"
                                        {
#if TREX
					 yyval = gc3(ap(mkExt(textOf(yyvsp[0])),yyvsp[-2]));
#else
					 noTREX("a type context");
#endif
					}
#line 3992 "y.tab.c"
    break;

  case 191: /* lacks: IPVARID COCO type  */
#line 480 "parser.y"
                                        {
#if IPARAM
					 yyval = gc3(pair(mkIParam(yyvsp[-2]),yyvsp[0]));
#else
					 noIP("a type context");
#endif
					}
#line 4004 "y.tab.c"
    break;

  case 192: /* lacks1: btypes2 ',' lacks  */
#line 488 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 4010 "y.tab.c"
    break;

  case 193: /* lacks1: lacks1 ',' btype2  */
#line 489 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 4016 "y.tab.c"
    break;

  case 194: /* lacks1: lacks1 ',' lacks  */
#line 490 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 4022 "y.tab.c"
    break;

  case 195: /* lacks1: btype2 ',' lacks  */
#line 491 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],cons(yyvsp[-2],NIL)));}
#line 4028 "y.tab.c"
    break;

  case 196: /* lacks1: lacks  */
#line 492 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 4034 "y.tab.c"
    break;

  case 197: /* type: type1  */
#line 495 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4040 "y.tab.c"
    break;

  case 198: /* type: btype2  */
#line 496 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4046 "y.tab.c"
    break;

  case 199: /* type1: btype1  */
#line 498 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4052 "y.tab.c"
    break;

  case 200: /* type1: bpolyType ARROW type  */
#line 499 "parser.y"
                                        {yyval = gc3(fn(yyvsp[-2],yyvsp[0]));}
#line 4058 "y.tab.c"
    break;

  case 201: /* type1: btype1 ARROW type  */
#line 500 "parser.y"
                                        {yyval = gc3(fn(yyvsp[-2],yyvsp[0]));}
#line 4064 "y.tab.c"
    break;

  case 202: /* type1: btype2 ARROW type  */
#line 501 "parser.y"
                                        {yyval = gc3(fn(yyvsp[-2],yyvsp[0]));}
#line 4070 "y.tab.c"
    break;

  case 203: /* type1: error  */
#line 502 "parser.y"
                                        {syntaxError("type expression");}
#line 4076 "y.tab.c"
    break;

  case 204: /* btype: btype1  */
#line 504 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4082 "y.tab.c"
    break;

  case 205: /* btype: btype2  */
#line 505 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4088 "y.tab.c"
    break;

  case 206: /* btype1: btype1 atype  */
#line 507 "parser.y"
                                        {yyval = gc2(ap(yyvsp[-1],yyvsp[0]));}
#line 4094 "y.tab.c"
    break;

  case 207: /* btype1: atype1  */
#line 508 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4100 "y.tab.c"
    break;

  case 208: /* btype2: btype2 atype  */
#line 510 "parser.y"
                                        {yyval = gc2(ap(yyvsp[-1],yyvsp[0]));}
#line 4106 "y.tab.c"
    break;

  case 209: /* btype2: qconid  */
#line 511 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4112 "y.tab.c"
    break;

  case 210: /* atype: atype1  */
#line 513 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4118 "y.tab.c"
    break;

  case 211: /* atype: qconid  */
#line 514 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4124 "y.tab.c"
    break;

  case 212: /* atype1: varid  */
#line 516 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4130 "y.tab.c"
    break;

  case 213: /* atype1: '(' ')'  */
#line 517 "parser.y"
                                        {yyval = gc2(typeUnit);}
#line 4136 "y.tab.c"
    break;

  case 214: /* atype1: '(' ARROW ')'  */
#line 518 "parser.y"
                                        {yyval = gc3(typeArrow);}
#line 4142 "y.tab.c"
    break;

  case 215: /* atype1: '(' type1 ')'  */
#line 519 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 4148 "y.tab.c"
    break;

  case 216: /* atype1: '(' btype2 ')'  */
#line 520 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 4154 "y.tab.c"
    break;

  case 217: /* atype1: '(' tupCommas ')'  */
#line 521 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 4160 "y.tab.c"
    break;

  case 218: /* atype1: '(' btypes2 ')'  */
#line 522 "parser.y"
                                        {yyval = gc3(buildTuple(yyvsp[-1]));}
#line 4166 "y.tab.c"
    break;

  case 219: /* atype1: '(' typeTuple ')'  */
#line 523 "parser.y"
                                        {yyval = gc3(buildTuple(yyvsp[-1]));}
#line 4172 "y.tab.c"
    break;

  case 220: /* atype1: '(' tfields ')'  */
#line 524 "parser.y"
                                        {
#if TREX
					 yyval = gc3(revOnto(yyvsp[-1],typeNoRow));
#else
					 noTREX("a type");
#endif
					}
#line 4184 "y.tab.c"
    break;

  case 221: /* atype1: '(' tfields '|' type ')'  */
#line 531 "parser.y"
                                        {
#if TREX
					 yyval = gc5(revOnto(yyvsp[-3],yyvsp[-1]));
#else
					 noTREX("a type");
#endif
					}
#line 4196 "y.tab.c"
    break;

  case 222: /* atype1: '[' type ']'  */
#line 538 "parser.y"
                                        {yyval = gc3(ap(typeList,yyvsp[-1]));}
#line 4202 "y.tab.c"
    break;

  case 223: /* atype1: '[' ']'  */
#line 539 "parser.y"
                                        {yyval = gc2(typeList);}
#line 4208 "y.tab.c"
    break;

  case 224: /* atype1: '_'  */
#line 540 "parser.y"
                                        {h98DoesntSupport(row,"anonymous type variables");
					 yyval = gc1(inventVar());}
#line 4215 "y.tab.c"
    break;

  case 225: /* btypes2: btypes2 ',' btype2  */
#line 543 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 4221 "y.tab.c"
    break;

  case 226: /* btypes2: btype2 ',' btype2  */
#line 544 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],cons(yyvsp[-2],NIL)));}
#line 4227 "y.tab.c"
    break;

  case 227: /* typeTuple: type1 ',' type  */
#line 546 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],cons(yyvsp[-2],NIL)));}
#line 4233 "y.tab.c"
    break;

  case 228: /* typeTuple: btype2 ',' type1  */
#line 547 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],cons(yyvsp[-2],NIL)));}
#line 4239 "y.tab.c"
    break;

  case 229: /* typeTuple: btypes2 ',' type1  */
#line 548 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 4245 "y.tab.c"
    break;

  case 230: /* typeTuple: typeTuple ',' type  */
#line 549 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 4251 "y.tab.c"
    break;

  case 231: /* tfields: tfields ',' tfield  */
#line 552 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 4257 "y.tab.c"
    break;

  case 232: /* tfields: tfield  */
#line 553 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 4263 "y.tab.c"
    break;

  case 233: /* tfield: varid COCO type  */
#line 555 "parser.y"
                                        {h98DoesntSupport(row,"extensible records");
					 yyval = gc3(ap(mkExt(textOf(yyvsp[-2])),yyvsp[0]));}
#line 4270 "y.tab.c"
    break;

  case 234: /* gendecl: INFIXN optDigit ops  */
#line 562 "parser.y"
                                        {yyval = gc3(fixdecl(yyvsp[-2],yyvsp[0],NON_ASS,yyvsp[-1]));}
#line 4276 "y.tab.c"
    break;

  case 235: /* gendecl: INFIXN error  */
#line 563 "parser.y"
                                        {syntaxError("fixity decl");}
#line 4282 "y.tab.c"
    break;

  case 236: /* gendecl: INFIXL optDigit ops  */
#line 564 "parser.y"
                                        {yyval = gc3(fixdecl(yyvsp[-2],yyvsp[0],LEFT_ASS,yyvsp[-1]));}
#line 4288 "y.tab.c"
    break;

  case 237: /* gendecl: INFIXL error  */
#line 565 "parser.y"
                                        {syntaxError("fixity decl");}
#line 4294 "y.tab.c"
    break;

  case 238: /* gendecl: INFIXR optDigit ops  */
#line 566 "parser.y"
                                        {yyval = gc3(fixdecl(yyvsp[-2],yyvsp[0],RIGHT_ASS,yyvsp[-1]));}
#line 4300 "y.tab.c"
    break;

  case 239: /* gendecl: INFIXR error  */
#line 567 "parser.y"
                                        {syntaxError("fixity decl");}
#line 4306 "y.tab.c"
    break;

  case 240: /* gendecl: vars COCO topType  */
#line 568 "parser.y"
                                        {yyval = gc3(sigdecl(yyvsp[-1],yyvsp[-2],yyvsp[0]));}
#line 4312 "y.tab.c"
    break;

  case 241: /* gendecl: vars COCO error  */
#line 569 "parser.y"
                                        {syntaxError("type signature");}
#line 4318 "y.tab.c"
    break;

  case 242: /* optDigit: NUMLIT  */
#line 571 "parser.y"
                                        {yyval = gc1(checkPrec(yyvsp[0]));}
#line 4324 "y.tab.c"
    break;

  case 243: /* optDigit: %empty  */
#line 572 "parser.y"
                                        {yyval = gc0(mkInt(DEF_PREC));}
#line 4330 "y.tab.c"
    break;

  case 244: /* ops: ops ',' op  */
#line 574 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 4336 "y.tab.c"
    break;

  case 245: /* ops: op  */
#line 575 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 4342 "y.tab.c"
    break;

  case 246: /* vars: vars ',' var  */
#line 577 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 4348 "y.tab.c"
    break;

  case 247: /* vars: var  */
#line 578 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 4354 "y.tab.c"
    break;

  case 248: /* decls: '{' decls0 end  */
#line 580 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 4360 "y.tab.c"
    break;

  case 249: /* decls: '{' decls1 end  */
#line 581 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 4366 "y.tab.c"
    break;

  case 250: /* decls0: %empty  */
#line 583 "parser.y"
                                        {yyval = gc0(NIL);}
#line 4372 "y.tab.c"
    break;

  case 251: /* decls0: decls0 ';'  */
#line 584 "parser.y"
                                        {yyval = gc2(yyvsp[-1]);}
#line 4378 "y.tab.c"
    break;

  case 252: /* decls0: decls1 ';'  */
#line 585 "parser.y"
                                        {yyval = gc2(yyvsp[-1]);}
#line 4384 "y.tab.c"
    break;

  case 253: /* decls1: decls0 decl  */
#line 587 "parser.y"
                                        {yyval = gc2(cons(yyvsp[0],yyvsp[-1]));}
#line 4390 "y.tab.c"
    break;

  case 254: /* decl: gendecl  */
#line 589 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4396 "y.tab.c"
    break;

  case 255: /* decl: funlhs rhs  */
#line 590 "parser.y"
                                        {yyval = gc2(ap(FUNBIND,pair(yyvsp[-1],yyvsp[0])));}
#line 4402 "y.tab.c"
    break;

  case 256: /* decl: funlhs COCO type rhs  */
#line 591 "parser.y"
                                        {yyval = gc4(ap(FUNBIND,
						     pair(yyvsp[-3],ap(RSIGN,
								ap(yyvsp[0],yyvsp[-1])))));}
#line 4410 "y.tab.c"
    break;

  case 257: /* decl: pat0 rhs  */
#line 594 "parser.y"
                                        {yyval = gc2(ap(PATBIND,pair(yyvsp[-1],yyvsp[0])));}
#line 4416 "y.tab.c"
    break;

  case 258: /* funlhs: funlhs0  */
#line 596 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4422 "y.tab.c"
    break;

  case 259: /* funlhs: funlhs1  */
#line 597 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4428 "y.tab.c"
    break;

  case 260: /* funlhs: npk  */
#line 598 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4434 "y.tab.c"
    break;

  case 261: /* funlhs0: pat10_vI varop pat0  */
#line 600 "parser.y"
                                        {yyval = gc3(ap2(yyvsp[-1],yyvsp[-2],yyvsp[0]));}
#line 4440 "y.tab.c"
    break;

  case 262: /* funlhs0: infixPat varop pat0  */
#line 601 "parser.y"
                                        {yyval = gc3(ap2(yyvsp[-1],yyvsp[-2],yyvsp[0]));}
#line 4446 "y.tab.c"
    break;

  case 263: /* funlhs0: NUMLIT varop pat0  */
#line 602 "parser.y"
                                        {yyval = gc3(ap2(yyvsp[-1],yyvsp[-2],yyvsp[0]));}
#line 4452 "y.tab.c"
    break;

  case 264: /* funlhs0: var varop_pl pat0  */
#line 603 "parser.y"
                                        {yyval = gc3(ap2(yyvsp[-1],yyvsp[-2],yyvsp[0]));}
#line 4458 "y.tab.c"
    break;

  case 265: /* funlhs0: var '+' pat0_INT  */
#line 604 "parser.y"
                                        {yyval = gc3(ap2(varPlus,yyvsp[-2],yyvsp[0]));}
#line 4464 "y.tab.c"
    break;

  case 266: /* funlhs1: '(' funlhs0 ')' apat  */
#line 606 "parser.y"
                                        {yyval = gc4(ap(yyvsp[-2],yyvsp[0]));}
#line 4470 "y.tab.c"
    break;

  case 267: /* funlhs1: '(' funlhs1 ')' apat  */
#line 607 "parser.y"
                                        {yyval = gc4(ap(yyvsp[-2],yyvsp[0]));}
#line 4476 "y.tab.c"
    break;

  case 268: /* funlhs1: '(' npk ')' apat  */
#line 608 "parser.y"
                                        {yyval = gc4(ap(yyvsp[-2],yyvsp[0]));}
#line 4482 "y.tab.c"
    break;

  case 269: /* funlhs1: var apat  */
#line 609 "parser.y"
                                        {yyval = gc2(ap(yyvsp[-1],yyvsp[0]));}
#line 4488 "y.tab.c"
    break;

  case 270: /* funlhs1: funlhs1 apat  */
#line 610 "parser.y"
                                        {yyval = gc2(ap(yyvsp[-1],yyvsp[0]));}
#line 4494 "y.tab.c"
    break;

  case 271: /* rhs: rhs1 wherePart  */
#line 612 "parser.y"
                                        {yyval = gc2(letrec(yyvsp[0],yyvsp[-1]));}
#line 4500 "y.tab.c"
    break;

  case 272: /* rhs: error  */
#line 613 "parser.y"
                                        {syntaxError("declaration");}
#line 4506 "y.tab.c"
    break;

  case 273: /* rhs1: '=' exp  */
#line 615 "parser.y"
                                        {yyval = gc2(pair(yyvsp[-1],yyvsp[0]));}
#line 4512 "y.tab.c"
    break;

  case 274: /* rhs1: gdrhs  */
#line 616 "parser.y"
                                        {yyval = gc1(grded(rev(yyvsp[0])));}
#line 4518 "y.tab.c"
    break;

  case 275: /* gdrhs: gdrhs gddef  */
#line 618 "parser.y"
                                        {yyval = gc2(cons(yyvsp[0],yyvsp[-1]));}
#line 4524 "y.tab.c"
    break;

  case 276: /* gdrhs: gddef  */
#line 619 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 4530 "y.tab.c"
    break;

  case 277: /* gddef: '|' exp0 '=' exp  */
#line 621 "parser.y"
                                        {yyval = gc4(pair(yyvsp[-1],pair(yyvsp[-2],yyvsp[0])));}
#line 4536 "y.tab.c"
    break;

  case 278: /* wherePart: %empty  */
#line 623 "parser.y"
                                        {yyval = gc0(NIL);}
#line 4542 "y.tab.c"
    break;

  case 279: /* wherePart: WHERE decls  */
#line 624 "parser.y"
                                        {yyval = gc2(yyvsp[0]);}
#line 4548 "y.tab.c"
    break;

  case 280: /* lwherePart: %empty  */
#line 629 "parser.y"
                                        {yyval = gc0(NIL);}
#line 4554 "y.tab.c"
    break;

  case 281: /* lwherePart: WHERE ldecls  */
#line 630 "parser.y"
                                        {yyval = gc2(yyvsp[0]);}
#line 4560 "y.tab.c"
    break;

  case 282: /* ldecls: '{' ldecls0 end  */
#line 633 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 4566 "y.tab.c"
    break;

  case 283: /* ldecls: '{' ldecls1 end  */
#line 634 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 4572 "y.tab.c"
    break;

  case 284: /* ldecls0: %empty  */
#line 637 "parser.y"
                                        {yyval = gc0(NIL);}
#line 4578 "y.tab.c"
    break;

  case 285: /* ldecls0: ldecls0 ';'  */
#line 638 "parser.y"
                                        {yyval = gc2(yyvsp[-1]);}
#line 4584 "y.tab.c"
    break;

  case 286: /* ldecls0: ldecls1 ';'  */
#line 639 "parser.y"
                                        {yyval = gc2(yyvsp[-1]);}
#line 4590 "y.tab.c"
    break;

  case 287: /* ldecls1: ldecls0 ldecl  */
#line 642 "parser.y"
                                        {yyval = gc2(cons(yyvsp[0],yyvsp[-1]));}
#line 4596 "y.tab.c"
    break;

  case 288: /* ldecl: IPVARID '=' exp  */
#line 644 "parser.y"
                                        {
#if IPARAM
				         yyval = gc3(pair(yyvsp[-2],yyvsp[0]));
#else
					 noIP("a binding");
#endif
					}
#line 4608 "y.tab.c"
    break;

  case 289: /* ldecl: IPVARID error  */
#line 651 "parser.y"
                                        {syntaxError("a binding");}
#line 4614 "y.tab.c"
    break;

  case 290: /* ldecl: decl  */
#line 652 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4620 "y.tab.c"
    break;

  case 291: /* pat: npk  */
#line 657 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4626 "y.tab.c"
    break;

  case 292: /* pat: pat_npk  */
#line 658 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4632 "y.tab.c"
    break;

  case 293: /* pat_npk: pat0 COCO type  */
#line 660 "parser.y"
                                        {yyval = gc3(ap(ESIGN,pair(yyvsp[-2],yyvsp[0])));}
#line 4638 "y.tab.c"
    break;

  case 294: /* pat_npk: pat0  */
#line 661 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4644 "y.tab.c"
    break;

  case 295: /* npk: var '+' NUMLIT  */
#line 663 "parser.y"
                                        {yyval = gc3(ap2(varPlus,yyvsp[-2],yyvsp[0]));}
#line 4650 "y.tab.c"
    break;

  case 296: /* pat0: var  */
#line 665 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4656 "y.tab.c"
    break;

  case 297: /* pat0: NUMLIT  */
#line 666 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4662 "y.tab.c"
    break;

  case 298: /* pat0: pat0_vI  */
#line 667 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4668 "y.tab.c"
    break;

  case 299: /* pat0_INT: var  */
#line 669 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4674 "y.tab.c"
    break;

  case 300: /* pat0_INT: pat0_vI  */
#line 670 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4680 "y.tab.c"
    break;

  case 301: /* pat0_vI: pat10_vI  */
#line 672 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4686 "y.tab.c"
    break;

  case 302: /* pat0_vI: infixPat  */
#line 673 "parser.y"
                                        {yyval = gc1(ap(INFIX,yyvsp[0]));}
#line 4692 "y.tab.c"
    break;

  case 303: /* infixPat: '-' pat10  */
#line 675 "parser.y"
                                        {yyval = gc2(ap(NEG,only(yyvsp[0])));}
#line 4698 "y.tab.c"
    break;

  case 304: /* infixPat: '-' error  */
#line 676 "parser.y"
                                        {syntaxError("pattern");}
#line 4704 "y.tab.c"
    break;

  case 305: /* infixPat: var qconop pat10  */
#line 677 "parser.y"
                                        {yyval = gc3(ap(ap(yyvsp[-1],only(yyvsp[-2])),yyvsp[0]));}
#line 4710 "y.tab.c"
    break;

  case 306: /* infixPat: var qconop '-' pat10  */
#line 678 "parser.y"
                                        {yyval = gc4(ap(NEG,ap2(yyvsp[-2],only(yyvsp[-3]),yyvsp[0])));}
#line 4716 "y.tab.c"
    break;

  case 307: /* infixPat: NUMLIT qconop pat10  */
#line 679 "parser.y"
                                        {yyval = gc3(ap(ap(yyvsp[-1],only(yyvsp[-2])),yyvsp[0]));}
#line 4722 "y.tab.c"
    break;

  case 308: /* infixPat: NUMLIT qconop '-' pat10  */
#line 680 "parser.y"
                                        {yyval = gc4(ap(NEG,ap2(yyvsp[-2],only(yyvsp[-3]),yyvsp[0])));}
#line 4728 "y.tab.c"
    break;

  case 309: /* infixPat: pat10_vI qconop pat10  */
#line 681 "parser.y"
                                        {yyval = gc3(ap(ap(yyvsp[-1],only(yyvsp[-2])),yyvsp[0]));}
#line 4734 "y.tab.c"
    break;

  case 310: /* infixPat: pat10_vI qconop '-' pat10  */
#line 682 "parser.y"
                                        {yyval = gc4(ap(NEG,ap2(yyvsp[-2],only(yyvsp[-3]),yyvsp[0])));}
#line 4740 "y.tab.c"
    break;

  case 311: /* infixPat: infixPat qconop pat10  */
#line 683 "parser.y"
                                        {yyval = gc3(ap(ap(yyvsp[-1],yyvsp[-2]),yyvsp[0]));}
#line 4746 "y.tab.c"
    break;

  case 312: /* infixPat: infixPat qconop '-' pat10  */
#line 684 "parser.y"
                                        {yyval = gc4(ap(NEG,ap(ap(yyvsp[-2],yyvsp[-3]),yyvsp[0])));}
#line 4752 "y.tab.c"
    break;

  case 313: /* pat10: fpat  */
#line 686 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4758 "y.tab.c"
    break;

  case 314: /* pat10: apat  */
#line 687 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4764 "y.tab.c"
    break;

  case 315: /* pat10_vI: fpat  */
#line 689 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4770 "y.tab.c"
    break;

  case 316: /* pat10_vI: apat_vI  */
#line 690 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4776 "y.tab.c"
    break;

  case 317: /* fpat: fpat apat  */
#line 692 "parser.y"
                                        {yyval = gc2(ap(yyvsp[-1],yyvsp[0]));}
#line 4782 "y.tab.c"
    break;

  case 318: /* fpat: gcon apat  */
#line 693 "parser.y"
                                        {yyval = gc2(ap(yyvsp[-1],yyvsp[0]));}
#line 4788 "y.tab.c"
    break;

  case 319: /* apat: NUMLIT  */
#line 695 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4794 "y.tab.c"
    break;

  case 320: /* apat: var  */
#line 696 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4800 "y.tab.c"
    break;

  case 321: /* apat: apat_vI  */
#line 697 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4806 "y.tab.c"
    break;

  case 322: /* apat_vI: var '@' apat  */
#line 699 "parser.y"
                                        {yyval = gc3(ap(ASPAT,pair(yyvsp[-2],yyvsp[0])));}
#line 4812 "y.tab.c"
    break;

  case 323: /* apat_vI: gcon  */
#line 700 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4818 "y.tab.c"
    break;

  case 324: /* apat_vI: qcon '{' patbinds '}'  */
#line 701 "parser.y"
                                        {yyval = gc4(ap(CONFLDS,pair(yyvsp[-3],yyvsp[-1])));}
#line 4824 "y.tab.c"
    break;

  case 325: /* apat_vI: CHARLIT  */
#line 702 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4830 "y.tab.c"
    break;

  case 326: /* apat_vI: STRINGLIT  */
#line 703 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4836 "y.tab.c"
    break;

  case 327: /* apat_vI: '_'  */
#line 704 "parser.y"
                                        {yyval = gc1(WILDCARD);}
#line 4842 "y.tab.c"
    break;

  case 328: /* apat_vI: '(' pat_npk ')'  */
#line 705 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 4848 "y.tab.c"
    break;

  case 329: /* apat_vI: '(' npk ')'  */
#line 706 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 4854 "y.tab.c"
    break;

  case 330: /* apat_vI: '(' pats2 ')'  */
#line 707 "parser.y"
                                        {yyval = gc3(buildTuple(yyvsp[-1]));}
#line 4860 "y.tab.c"
    break;

  case 331: /* apat_vI: '[' pats1 ']'  */
#line 708 "parser.y"
                                        {yyval = gc3(ap(FINLIST,rev(yyvsp[-1])));}
#line 4866 "y.tab.c"
    break;

  case 332: /* apat_vI: '~' apat  */
#line 709 "parser.y"
                                        {yyval = gc2(ap(LAZYPAT,yyvsp[0]));}
#line 4872 "y.tab.c"
    break;

  case 333: /* apat_vI: '(' patfields ')'  */
#line 711 "parser.y"
                                        {
#if TREX
					 yyval = gc3(revOnto(yyvsp[-1],nameNoRec));
#else
					 yyval = gc3(NIL);
#endif
					}
#line 4884 "y.tab.c"
    break;

  case 334: /* apat_vI: '(' patfields '|' pat ')'  */
#line 718 "parser.y"
                                        {yyval = gc5(revOnto(yyvsp[-3],yyvsp[-1]));}
#line 4890 "y.tab.c"
    break;

  case 335: /* pats2: pats2 ',' pat  */
#line 721 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 4896 "y.tab.c"
    break;

  case 336: /* pats2: pat ',' pat  */
#line 722 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],singleton(yyvsp[-2])));}
#line 4902 "y.tab.c"
    break;

  case 337: /* pats1: pats1 ',' pat  */
#line 724 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 4908 "y.tab.c"
    break;

  case 338: /* pats1: pat  */
#line 725 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 4914 "y.tab.c"
    break;

  case 339: /* patbinds: %empty  */
#line 727 "parser.y"
                                        {yyval = gc0(NIL);}
#line 4920 "y.tab.c"
    break;

  case 340: /* patbinds: patbinds1  */
#line 728 "parser.y"
                                        {yyval = gc1(rev(yyvsp[0]));}
#line 4926 "y.tab.c"
    break;

  case 341: /* patbinds1: patbinds1 ',' patbind  */
#line 730 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 4932 "y.tab.c"
    break;

  case 342: /* patbinds1: patbind  */
#line 731 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 4938 "y.tab.c"
    break;

  case 343: /* patbind: qvar '=' pat  */
#line 733 "parser.y"
                                        {yyval = gc3(pair(yyvsp[-2],yyvsp[0]));}
#line 4944 "y.tab.c"
    break;

  case 344: /* patbind: var  */
#line 734 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4950 "y.tab.c"
    break;

  case 345: /* patfields: patfields ',' patfield  */
#line 737 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 4956 "y.tab.c"
    break;

  case 346: /* patfields: patfield  */
#line 738 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 4962 "y.tab.c"
    break;

  case 347: /* patfield: varid '=' pat  */
#line 740 "parser.y"
                                        {
#if TREX
					 yyval = gc3(ap(mkExt(textOf(yyvsp[-2])),yyvsp[0]));
#else
					 noTREX("a pattern");
#endif
					}
#line 4974 "y.tab.c"
    break;

  case 348: /* exp: exp_err  */
#line 752 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4980 "y.tab.c"
    break;

  case 349: /* exp: error  */
#line 753 "parser.y"
                                        {syntaxError("expression");}
#line 4986 "y.tab.c"
    break;

  case 350: /* exp_err: exp0a COCO sigType  */
#line 755 "parser.y"
                                        {yyval = gc3(ap(ESIGN,pair(yyvsp[-2],yyvsp[0])));}
#line 4992 "y.tab.c"
    break;

  case 351: /* exp_err: exp0  */
#line 756 "parser.y"
                                        {yyval = yyvsp[0];}
#line 4998 "y.tab.c"
    break;

  case 352: /* exp0: exp0a  */
#line 758 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5004 "y.tab.c"
    break;

  case 353: /* exp0: exp0b  */
#line 759 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5010 "y.tab.c"
    break;

  case 354: /* exp0a: infixExpa  */
#line 761 "parser.y"
                                        {yyval = gc1(ap(INFIX,yyvsp[0]));}
#line 5016 "y.tab.c"
    break;

  case 355: /* exp0a: exp10a  */
#line 762 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5022 "y.tab.c"
    break;

  case 356: /* exp0b: infixExpb  */
#line 764 "parser.y"
                                        {yyval = gc1(ap(INFIX,yyvsp[0]));}
#line 5028 "y.tab.c"
    break;

  case 357: /* exp0b: exp10b  */
#line 765 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5034 "y.tab.c"
    break;

  case 358: /* infixExpa: infixExpa qop '-' exp10a  */
#line 767 "parser.y"
                                        {yyval = gc4(ap(NEG,ap(ap(yyvsp[-2],yyvsp[-3]),yyvsp[0])));}
#line 5040 "y.tab.c"
    break;

  case 359: /* infixExpa: infixExpa qop exp10a  */
#line 768 "parser.y"
                                        {yyval = gc3(ap(ap(yyvsp[-1],yyvsp[-2]),yyvsp[0]));}
#line 5046 "y.tab.c"
    break;

  case 360: /* infixExpa: '-' exp10a  */
#line 769 "parser.y"
                                        {yyval = gc2(ap(NEG,only(yyvsp[0])));}
#line 5052 "y.tab.c"
    break;

  case 361: /* infixExpa: exp10a qop '-' exp10a  */
#line 770 "parser.y"
                                        {yyval = gc4(ap(NEG,
						     ap(ap(yyvsp[-2],only(yyvsp[-3])),yyvsp[0])));}
#line 5059 "y.tab.c"
    break;

  case 362: /* infixExpa: exp10a qop exp10a  */
#line 772 "parser.y"
                                        {yyval = gc3(ap(ap(yyvsp[-1],only(yyvsp[-2])),yyvsp[0]));}
#line 5065 "y.tab.c"
    break;

  case 363: /* infixExpb: infixExpa qop '-' exp10b  */
#line 774 "parser.y"
                                        {yyval = gc4(ap(NEG,ap(ap(yyvsp[-2],yyvsp[-3]),yyvsp[0])));}
#line 5071 "y.tab.c"
    break;

  case 364: /* infixExpb: infixExpa qop exp10b  */
#line 775 "parser.y"
                                        {yyval = gc3(ap(ap(yyvsp[-1],yyvsp[-2]),yyvsp[0]));}
#line 5077 "y.tab.c"
    break;

  case 365: /* infixExpb: '-' exp10b  */
#line 776 "parser.y"
                                        {yyval = gc2(ap(NEG,only(yyvsp[0])));}
#line 5083 "y.tab.c"
    break;

  case 366: /* infixExpb: exp10a qop '-' exp10b  */
#line 777 "parser.y"
                                        {yyval = gc4(ap(NEG,
						     ap(ap(yyvsp[-2],only(yyvsp[-3])),yyvsp[0])));}
#line 5090 "y.tab.c"
    break;

  case 367: /* infixExpb: exp10a qop exp10b  */
#line 779 "parser.y"
                                        {yyval = gc3(ap(ap(yyvsp[-1],only(yyvsp[-2])),yyvsp[0]));}
#line 5096 "y.tab.c"
    break;

  case 368: /* exp10a: CASEXP exp OF '{' alts end  */
#line 781 "parser.y"
                                        {yyval = gc6(ap(CASE,pair(yyvsp[-4],rev(yyvsp[-1]))));}
#line 5102 "y.tab.c"
    break;

  case 369: /* exp10a: DO '{' stmts end  */
#line 782 "parser.y"
                                        {yyval = gc4(ap(DOCOMP,checkDo(yyvsp[-1])));}
#line 5108 "y.tab.c"
    break;

  case 370: /* exp10a: MDO '{' stmts end  */
#line 783 "parser.y"
                                        {
#if MUDO
					 yyval = gc4(ap(MDOCOMP, checkMDo(yyvsp[-1])));
#else
					 noMDo("an expression");
#endif
					}
#line 5120 "y.tab.c"
    break;

  case 371: /* exp10a: appExp  */
#line 790 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5126 "y.tab.c"
    break;

  case 372: /* exp10b: '\\' pats ARROW exp  */
#line 792 "parser.y"
                                        {yyval = gc4(ap(LAMBDA,      
						     pair(rev(yyvsp[-2]),
							  pair(yyvsp[-1],yyvsp[0]))));}
#line 5134 "y.tab.c"
    break;

  case 373: /* exp10b: LET ldecls IN exp  */
#line 795 "parser.y"
                                        {yyval = gc4(letrec(yyvsp[-2],yyvsp[0]));}
#line 5140 "y.tab.c"
    break;

  case 374: /* exp10b: IF exp then_exp else_exp  */
#line 796 "parser.y"
                                        {yyval = gc4(ap(COND,triple(yyvsp[-2],yyvsp[-1],yyvsp[0])));}
#line 5146 "y.tab.c"
    break;

  case 375: /* then_exp: ';' THEN exp  */
#line 801 "parser.y"
                                        {yyval = gc3(yyvsp[0]);}
#line 5152 "y.tab.c"
    break;

  case 376: /* then_exp: THEN exp  */
#line 802 "parser.y"
                                        {yyval = gc2(yyvsp[0]);}
#line 5158 "y.tab.c"
    break;

  case 377: /* else_exp: ';' ELSE exp  */
#line 804 "parser.y"
                                        {yyval = gc3(yyvsp[0]);}
#line 5164 "y.tab.c"
    break;

  case 378: /* else_exp: ELSE exp  */
#line 805 "parser.y"
                                        {yyval = gc2(yyvsp[0]);}
#line 5170 "y.tab.c"
    break;

  case 379: /* pats: pats apat  */
#line 808 "parser.y"
                                        {yyval = gc2(cons(yyvsp[0],yyvsp[-1]));}
#line 5176 "y.tab.c"
    break;

  case 380: /* pats: apat  */
#line 809 "parser.y"
                                        {yyval = gc1(cons(yyvsp[0],NIL));}
#line 5182 "y.tab.c"
    break;

  case 381: /* appExp: appExp aexp  */
#line 811 "parser.y"
                                        {yyval = gc2(ap(yyvsp[-1],yyvsp[0]));}
#line 5188 "y.tab.c"
    break;

  case 382: /* appExp: aexp  */
#line 812 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5194 "y.tab.c"
    break;

  case 383: /* aexp: qvar  */
#line 814 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5200 "y.tab.c"
    break;

  case 384: /* aexp: qvar '@' aexp  */
#line 815 "parser.y"
                                        {yyval = gc3(ap(ASPAT,pair(yyvsp[-2],yyvsp[0])));}
#line 5206 "y.tab.c"
    break;

  case 385: /* aexp: '~' aexp  */
#line 816 "parser.y"
                                        {yyval = gc2(ap(LAZYPAT,yyvsp[0]));}
#line 5212 "y.tab.c"
    break;

  case 386: /* aexp: IPVARID  */
#line 817 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5218 "y.tab.c"
    break;

  case 387: /* aexp: '_'  */
#line 818 "parser.y"
                                        {yyval = gc1(WILDCARD);}
#line 5224 "y.tab.c"
    break;

  case 388: /* aexp: gcon  */
#line 819 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5230 "y.tab.c"
    break;

  case 389: /* aexp: qcon '{' fbinds '}'  */
#line 820 "parser.y"
                                        {yyval = gc4(ap(CONFLDS,pair(yyvsp[-3],yyvsp[-1])));}
#line 5236 "y.tab.c"
    break;

  case 390: /* aexp: aexp '{' fbinds '}'  */
#line 821 "parser.y"
                                        {yyval = gc4(ap(UPDFLDS,
						     triple(yyvsp[-3],NIL,yyvsp[-1])));}
#line 5243 "y.tab.c"
    break;

  case 391: /* aexp: NUMLIT  */
#line 823 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5249 "y.tab.c"
    break;

  case 392: /* aexp: CHARLIT  */
#line 824 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5255 "y.tab.c"
    break;

  case 393: /* aexp: STRINGLIT  */
#line 825 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5261 "y.tab.c"
    break;

  case 394: /* aexp: REPEAT  */
#line 826 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5267 "y.tab.c"
    break;

  case 395: /* aexp: '(' exp ')'  */
#line 827 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 5273 "y.tab.c"
    break;

  case 396: /* aexp: '(' exps2 ')'  */
#line 828 "parser.y"
                                        {yyval = gc3(buildTuple(yyvsp[-1]));}
#line 5279 "y.tab.c"
    break;

  case 397: /* aexp: '(' vfields ')'  */
#line 830 "parser.y"
                                        {
#if TREX
					 yyval = gc3(revOnto(yyvsp[-1],nameNoRec));
#else
					 yyval = gc3(NIL);
#endif
					}
#line 5291 "y.tab.c"
    break;

  case 398: /* aexp: '(' vfields '|' exp ')'  */
#line 837 "parser.y"
                                        {yyval = gc5(revOnto(yyvsp[-3],yyvsp[-1]));}
#line 5297 "y.tab.c"
    break;

  case 399: /* aexp: RECSELID  */
#line 838 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5303 "y.tab.c"
    break;

  case 400: /* aexp: '[' list ']'  */
#line 840 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 5309 "y.tab.c"
    break;

  case 401: /* aexp: '(' exp10a qop ')'  */
#line 841 "parser.y"
                                        {yyval = gc4(ap(yyvsp[-1],yyvsp[-2]));}
#line 5315 "y.tab.c"
    break;

  case 402: /* aexp: '(' qvarop_mi exp0 ')'  */
#line 842 "parser.y"
                                        {yyval = gc4(ap(ap(nameFlip,yyvsp[-2]),yyvsp[-1]));}
#line 5321 "y.tab.c"
    break;

  case 403: /* aexp: '(' qconop exp0 ')'  */
#line 843 "parser.y"
                                        {yyval = gc4(ap(ap(nameFlip,yyvsp[-2]),yyvsp[-1]));}
#line 5327 "y.tab.c"
    break;

  case 404: /* exps2: exps2 ',' exp  */
#line 845 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 5333 "y.tab.c"
    break;

  case 405: /* exps2: exp ',' exp  */
#line 846 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],cons(yyvsp[-2],NIL)));}
#line 5339 "y.tab.c"
    break;

  case 406: /* vfields: vfields ',' vfield  */
#line 849 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 5345 "y.tab.c"
    break;

  case 407: /* vfields: vfield  */
#line 850 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 5351 "y.tab.c"
    break;

  case 408: /* vfield: varid '=' exp  */
#line 852 "parser.y"
                                        {
#if TREX
					 yyval = gc3(ap(mkExt(textOf(yyvsp[-2])),yyvsp[0]));
#else
					 noTREX("an expression");
#endif
					}
#line 5363 "y.tab.c"
    break;

  case 409: /* alts: alts1  */
#line 861 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5369 "y.tab.c"
    break;

  case 410: /* alts: ';' alts  */
#line 862 "parser.y"
                                        {yyval = gc2(yyvsp[0]);}
#line 5375 "y.tab.c"
    break;

  case 411: /* alts1: alts1 ';' alt  */
#line 864 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 5381 "y.tab.c"
    break;

  case 412: /* alts1: alts1 ';'  */
#line 865 "parser.y"
                                        {yyval = gc2(yyvsp[-1]);}
#line 5387 "y.tab.c"
    break;

  case 413: /* alts1: alt  */
#line 866 "parser.y"
                                        {yyval = gc1(cons(yyvsp[0],NIL));}
#line 5393 "y.tab.c"
    break;

  case 414: /* alt: pat altRhs wherePart  */
#line 868 "parser.y"
                                        {yyval = gc3(pair(yyvsp[-2],letrec(yyvsp[0],yyvsp[-1])));}
#line 5399 "y.tab.c"
    break;

  case 415: /* altRhs: guardAlts  */
#line 870 "parser.y"
                                        {yyval = gc1(grded(rev(yyvsp[0])));}
#line 5405 "y.tab.c"
    break;

  case 416: /* altRhs: ARROW exp  */
#line 871 "parser.y"
                                        {yyval = gc2(pair(yyvsp[-1],yyvsp[0]));}
#line 5411 "y.tab.c"
    break;

  case 417: /* altRhs: error  */
#line 872 "parser.y"
                                        {syntaxError("case expression");}
#line 5417 "y.tab.c"
    break;

  case 418: /* guardAlts: guardAlts guardAlt  */
#line 874 "parser.y"
                                        {yyval = gc2(cons(yyvsp[0],yyvsp[-1]));}
#line 5423 "y.tab.c"
    break;

  case 419: /* guardAlts: guardAlt  */
#line 875 "parser.y"
                                        {yyval = gc1(cons(yyvsp[0],NIL));}
#line 5429 "y.tab.c"
    break;

  case 420: /* guardAlt: '|' exp0 ARROW exp  */
#line 877 "parser.y"
                                        {yyval = gc4(pair(yyvsp[-1],pair(yyvsp[-2],yyvsp[0])));}
#line 5435 "y.tab.c"
    break;

  case 421: /* stmts: stmts1  */
#line 880 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5441 "y.tab.c"
    break;

  case 422: /* stmts: ';' stmts  */
#line 881 "parser.y"
                                        {yyval = gc2(yyvsp[0]);}
#line 5447 "y.tab.c"
    break;

  case 423: /* stmts1: stmts1 ';' stmt  */
#line 883 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 5453 "y.tab.c"
    break;

  case 424: /* stmts1: stmts1 ';'  */
#line 884 "parser.y"
                                        {yyval = gc2(yyvsp[-1]);}
#line 5459 "y.tab.c"
    break;

  case 425: /* stmts1: stmt  */
#line 885 "parser.y"
                                        {yyval = gc1(cons(yyvsp[0],NIL));}
#line 5465 "y.tab.c"
    break;

  case 426: /* stmt: exp_err FROM exp  */
#line 888 "parser.y"
                                        {yyval = gc3(ap(FROMQUAL,pair(yyvsp[-2],yyvsp[0])));}
#line 5471 "y.tab.c"
    break;

  case 427: /* stmt: LET ldecls  */
#line 889 "parser.y"
                                        {yyval = gc2(ap(QWHERE,yyvsp[0]));}
#line 5477 "y.tab.c"
    break;

  case 428: /* stmt: exp_err  */
#line 891 "parser.y"
                                        {yyval = gc1(ap(DOQUAL,yyvsp[0]));}
#line 5483 "y.tab.c"
    break;

  case 429: /* fbinds: %empty  */
#line 893 "parser.y"
                                        {yyval = gc0(NIL);}
#line 5489 "y.tab.c"
    break;

  case 430: /* fbinds: fbinds1  */
#line 894 "parser.y"
                                        {yyval = gc1(rev(yyvsp[0]));}
#line 5495 "y.tab.c"
    break;

  case 431: /* fbinds1: fbinds1 ',' fbind  */
#line 896 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 5501 "y.tab.c"
    break;

  case 432: /* fbinds1: fbind  */
#line 897 "parser.y"
                                        {yyval = gc1(singleton(yyvsp[0]));}
#line 5507 "y.tab.c"
    break;

  case 433: /* fbind: var  */
#line 899 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5513 "y.tab.c"
    break;

  case 434: /* fbind: qvar '=' exp  */
#line 900 "parser.y"
                                        {yyval = gc3(pair(yyvsp[-2],yyvsp[0]));}
#line 5519 "y.tab.c"
    break;

  case 435: /* list: exp  */
#line 905 "parser.y"
                                        {yyval = gc1(ap(FINLIST,cons(yyvsp[0],NIL)));}
#line 5525 "y.tab.c"
    break;

  case 436: /* list: exps2  */
#line 906 "parser.y"
                                        {yyval = gc1(ap(FINLIST,rev(yyvsp[0])));}
#line 5531 "y.tab.c"
    break;

  case 437: /* list: exp zipquals  */
#line 907 "parser.y"
                                        {
#if ZIP_COMP
					 if (length(yyvsp[0])==1) {
					     yyval = gc2(ap(COMP,pair(yyvsp[-1],hd(yyvsp[0]))));
					 } else {
					     if (haskell98)
						 syntaxError("list comprehension");
					     yyval = gc2(ap(ZCOMP,pair(yyvsp[-1],rev(yyvsp[0]))));
					 }
#else
					 if (length(yyvsp[0])!=1) {
					     syntaxError("list comprehension");
					 }
					 yyval = gc2(ap(COMP,pair(yyvsp[-1],hd(yyvsp[0]))));
#endif
					}
#line 5552 "y.tab.c"
    break;

  case 438: /* list: exp UPTO exp  */
#line 923 "parser.y"
                                        {yyval = gc3(ap(ap(nameFromTo,yyvsp[-2]),yyvsp[0]));}
#line 5558 "y.tab.c"
    break;

  case 439: /* list: exp ',' exp UPTO  */
#line 924 "parser.y"
                                        {yyval = gc4(ap(ap(nameFromThen,yyvsp[-3]),yyvsp[-1]));}
#line 5564 "y.tab.c"
    break;

  case 440: /* list: exp UPTO  */
#line 925 "parser.y"
                                        {yyval = gc2(ap(nameFrom,yyvsp[-1]));}
#line 5570 "y.tab.c"
    break;

  case 441: /* list: exp ',' exp UPTO exp  */
#line 926 "parser.y"
                                        {yyval = gc5(ap(ap(ap(nameFromThenTo,
								yyvsp[-4]),yyvsp[-2]),yyvsp[0]));}
#line 5577 "y.tab.c"
    break;

  case 442: /* zipquals: zipquals '|' quals  */
#line 929 "parser.y"
                                        {yyval = gc3(cons(rev(yyvsp[0]),yyvsp[-2]));}
#line 5583 "y.tab.c"
    break;

  case 443: /* zipquals: '|' quals  */
#line 930 "parser.y"
                                        {yyval = gc2(cons(rev(yyvsp[0]),NIL));}
#line 5589 "y.tab.c"
    break;

  case 444: /* quals: quals ',' qual  */
#line 932 "parser.y"
                                        {yyval = gc3(cons(yyvsp[0],yyvsp[-2]));}
#line 5595 "y.tab.c"
    break;

  case 445: /* quals: qual  */
#line 933 "parser.y"
                                        {yyval = gc1(cons(yyvsp[0],NIL));}
#line 5601 "y.tab.c"
    break;

  case 446: /* qual: exp FROM exp  */
#line 935 "parser.y"
                                        {yyval = gc3(ap(FROMQUAL,pair(yyvsp[-2],yyvsp[0])));}
#line 5607 "y.tab.c"
    break;

  case 447: /* qual: exp  */
#line 936 "parser.y"
                                        {yyval = gc1(ap(BOOLQUAL,yyvsp[0]));}
#line 5613 "y.tab.c"
    break;

  case 448: /* qual: LET ldecls  */
#line 937 "parser.y"
                                        {yyval = gc2(ap(QWHERE,yyvsp[0]));}
#line 5619 "y.tab.c"
    break;

  case 449: /* gcon: qcon  */
#line 942 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5625 "y.tab.c"
    break;

  case 450: /* gcon: '(' ')'  */
#line 943 "parser.y"
                                        {yyval = gc2(nameUnit);}
#line 5631 "y.tab.c"
    break;

  case 451: /* gcon: '[' ']'  */
#line 944 "parser.y"
                                        {yyval = gc2(nameNil);}
#line 5637 "y.tab.c"
    break;

  case 452: /* gcon: '(' tupCommas ')'  */
#line 945 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 5643 "y.tab.c"
    break;

  case 453: /* tupCommas: tupCommas ','  */
#line 947 "parser.y"
                                        {yyval = gc2(mkTuple(tupleOf(yyvsp[-1])+1));}
#line 5649 "y.tab.c"
    break;

  case 454: /* tupCommas: ','  */
#line 948 "parser.y"
                                        {yyval = gc1(mkTuple(2));}
#line 5655 "y.tab.c"
    break;

  case 455: /* varid: VARID  */
#line 950 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5661 "y.tab.c"
    break;

  case 456: /* varid: HIDING  */
#line 951 "parser.y"
                                        {yyval = gc1(varHiding);}
#line 5667 "y.tab.c"
    break;

  case 457: /* varid: QUALIFIED  */
#line 952 "parser.y"
                                        {yyval = gc1(varQualified);}
#line 5673 "y.tab.c"
    break;

  case 458: /* varid: ASMOD  */
#line 953 "parser.y"
                                        {yyval = gc1(varAsMod);}
#line 5679 "y.tab.c"
    break;

  case 459: /* qconid: QCONID  */
#line 955 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5685 "y.tab.c"
    break;

  case 460: /* qconid: CONID  */
#line 956 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5691 "y.tab.c"
    break;

  case 461: /* var: varid  */
#line 958 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5697 "y.tab.c"
    break;

  case 462: /* var: '(' VAROP ')'  */
#line 959 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 5703 "y.tab.c"
    break;

  case 463: /* var: '(' '+' ')'  */
#line 960 "parser.y"
                                        {yyval = gc3(varPlus);}
#line 5709 "y.tab.c"
    break;

  case 464: /* var: '(' '-' ')'  */
#line 961 "parser.y"
                                        {yyval = gc3(varMinus);}
#line 5715 "y.tab.c"
    break;

  case 465: /* var: '(' '!' ')'  */
#line 962 "parser.y"
                                        {yyval = gc3(varBang);}
#line 5721 "y.tab.c"
    break;

  case 466: /* var: '(' '.' ')'  */
#line 963 "parser.y"
                                        {yyval = gc3(varDot);}
#line 5727 "y.tab.c"
    break;

  case 467: /* qvar: QVARID  */
#line 965 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5733 "y.tab.c"
    break;

  case 468: /* qvar: '(' QVAROP ')'  */
#line 966 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 5739 "y.tab.c"
    break;

  case 469: /* qvar: var  */
#line 967 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5745 "y.tab.c"
    break;

  case 470: /* con: CONID  */
#line 969 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5751 "y.tab.c"
    break;

  case 471: /* con: '(' CONOP ')'  */
#line 970 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 5757 "y.tab.c"
    break;

  case 472: /* qcon: QCONID  */
#line 972 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5763 "y.tab.c"
    break;

  case 473: /* qcon: '(' QCONOP ')'  */
#line 973 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 5769 "y.tab.c"
    break;

  case 474: /* qcon: con  */
#line 974 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5775 "y.tab.c"
    break;

  case 475: /* varop: '+'  */
#line 976 "parser.y"
                                        {yyval = gc1(varPlus);}
#line 5781 "y.tab.c"
    break;

  case 476: /* varop: '-'  */
#line 977 "parser.y"
                                        {yyval = gc1(varMinus);}
#line 5787 "y.tab.c"
    break;

  case 477: /* varop: varop_mipl  */
#line 978 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5793 "y.tab.c"
    break;

  case 478: /* varop_mi: '+'  */
#line 980 "parser.y"
                                        {yyval = gc1(varPlus);}
#line 5799 "y.tab.c"
    break;

  case 479: /* varop_mi: varop_mipl  */
#line 981 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5805 "y.tab.c"
    break;

  case 480: /* varop_pl: '-'  */
#line 983 "parser.y"
                                        {yyval = gc1(varMinus);}
#line 5811 "y.tab.c"
    break;

  case 481: /* varop_pl: varop_mipl  */
#line 984 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5817 "y.tab.c"
    break;

  case 482: /* varop_mipl: VAROP  */
#line 986 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5823 "y.tab.c"
    break;

  case 483: /* varop_mipl: '`' varid '`'  */
#line 987 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 5829 "y.tab.c"
    break;

  case 484: /* varop_mipl: '!'  */
#line 988 "parser.y"
                                        {yyval = gc1(varBang);}
#line 5835 "y.tab.c"
    break;

  case 485: /* varop_mipl: '.'  */
#line 989 "parser.y"
                                        {yyval = gc1(varDot);}
#line 5841 "y.tab.c"
    break;

  case 486: /* qvarop: '-'  */
#line 991 "parser.y"
                                        {yyval = gc1(varMinus);}
#line 5847 "y.tab.c"
    break;

  case 487: /* qvarop: qvarop_mi  */
#line 992 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5853 "y.tab.c"
    break;

  case 488: /* qvarop_mi: QVAROP  */
#line 994 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5859 "y.tab.c"
    break;

  case 489: /* qvarop_mi: '`' QVARID '`'  */
#line 995 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 5865 "y.tab.c"
    break;

  case 490: /* qvarop_mi: varop_mi  */
#line 996 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5871 "y.tab.c"
    break;

  case 491: /* conop: CONOP  */
#line 999 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5877 "y.tab.c"
    break;

  case 492: /* conop: '`' CONID '`'  */
#line 1000 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 5883 "y.tab.c"
    break;

  case 493: /* qconop: QCONOP  */
#line 1002 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5889 "y.tab.c"
    break;

  case 494: /* qconop: '`' QCONID '`'  */
#line 1003 "parser.y"
                                        {yyval = gc3(yyvsp[-1]);}
#line 5895 "y.tab.c"
    break;

  case 495: /* qconop: conop  */
#line 1004 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5901 "y.tab.c"
    break;

  case 496: /* op: varop  */
#line 1006 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5907 "y.tab.c"
    break;

  case 497: /* op: conop  */
#line 1007 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5913 "y.tab.c"
    break;

  case 498: /* qop: qvarop  */
#line 1009 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5919 "y.tab.c"
    break;

  case 499: /* qop: qconop  */
#line 1010 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5925 "y.tab.c"
    break;

  case 500: /* begin: %empty  */
#line 1015 "parser.y"
                                        {goOffside(startColumn);}
#line 5931 "y.tab.c"
    break;

  case 501: /* end: '}'  */
#line 1018 "parser.y"
                                        {yyval = yyvsp[0];}
#line 5937 "y.tab.c"
    break;

  case 502: /* end: error  */
#line 1019 "parser.y"
                                        {yyerrok; 
					 if (canUnOffside()) {
					     unOffside();
					     /* insert extra token on stack*/
					     push(NIL);
					     pushed(0) = pushed(1);
					     pushed(1) = mkInt(column);
					 }
					 else
					     syntaxError("declaration");
					}
#line 5953 "y.tab.c"
    break;


#line 5957 "y.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 1034 "parser.y"


static Cell local gcShadow(n,e)		/* keep parsed fragments on stack  */
Int  n;
Cell e; {
    /* If a look ahead token is held then the required stack transformation
     * is:
     *   pushed: n               1     0          1     0
     *           x1  |  ...  |  xn  |  la   ===>  e  |  la
     *                                top()            top()
     *
     * Othwerwise, the transformation is:
     *   pushed: n-1             0        0
     *           x1  |  ...  |  xn  ===>  e
     *                         top()     top()
     */
    if (yychar>=0) {
	pushed(n-1) = top();
	pushed(n)   = e;
    }
    else
	pushed(n-1) = e;
    sp -= (n-1);
    return e;
}

static Void local syntaxError(s)	/* report on syntax error	   */
String s; {
    ERRMSG(row) "Syntax error in %s (unexpected %s)", s, unexpected()
    EEND;
}

static String local unexpected() {     /* find name for unexpected token   */
    static char buffer[100];
    static char *fmt = "%s \"%s\"";
    static char *kwd = "keyword";

    switch (yychar) {
	case 0         : return "end of input";

#define keyword(kw) sprintf(buffer,fmt,kwd,kw); return buffer;
	case INFIXL    : keyword("infixl");
	case INFIXR    : keyword("infixr");
	case INFIXN    : keyword("infix");
	case TINSTANCE : keyword("instance");
	case TCLASS    : keyword("class");
	case PRIMITIVE : keyword("primitive");
	case CASEXP    : keyword("case");
	case OF        : keyword("of");
	case IF        : keyword("if");
	case THEN      : keyword("then");
	case ELSE      : keyword("else");
	case WHERE     : keyword("where");
	case TYPE      : keyword("type");
	case DATA      : keyword("data");
	case TNEWTYPE  : keyword("newtype");
	case LET       : keyword("let");
	case IN        : keyword("in");
	case DERIVING  : keyword("deriving");
	case DEFAULT   : keyword("default");
	case IMPORT    : keyword("import");
	case TMODULE   : keyword("module");
	case ALL       : keyword("forall");
#undef keyword

	case ARROW     : return "`->'";
	case '='       : return "`='";
	case COCO      : return "`::'";
	case '-'       : return "`-'";
	case '!'       : return "`!'";
	case ','       : return "comma";
	case '@'       : return "`@'";
	case '('       : return "`('";
	case ')'       : return "`)'";
	case '{'       : return "`{', possibly due to bad layout";
	case '}'       : return "`}', possibly due to bad layout";
	case '_'       : return "`_'";
	case '|'       : return "`|'";
	case '.'       : return "`.'";
	case ';'       : return "`;', possibly due to bad layout";
	case UPTO      : return "`..'";
	case '['       : return "`['";
	case ']'       : return "`]'";
	case FROM      : return "`<-'";
	case '\\'      : return "backslash (lambda)";
	case '~'       : return "tilde";
	case '`'       : return "backquote";
#if TREX
	case RECSELID  : sprintf(buffer,"selector \"#%s\"",
				 textToStr(extText(snd(yylval))));
			 return buffer;
#endif
#if IPARAM
	case IPVARID   : sprintf(buffer,"implicit parameter \"?%s\"",
				 textToStr(textOf(yylval)));
			 return buffer;
#endif
	case VAROP     :
	case VARID     :
	case CONOP     :
	case CONID     : sprintf(buffer,"symbol \"%s\"",
				 textToStr(textOf(yylval)));
			 return buffer;
	case QVAROP    :
	case QVARID    :
	case QCONOP    : 
	case QCONID    : sprintf(buffer,"symbol \"%s\"",
				 identToStr(yylval));
			 return buffer;
	case HIDING    : return "symbol \"hiding\"";
	case QUALIFIED : return "symbol \"qualified\"";
	case ASMOD     : return "symbol \"as\"";
	case NUMLIT    : return "numeric literal";
	case CHARLIT   : return "character literal";
	case STRINGLIT : return "string literal";
	case IMPLIES   : return "`=>'";
	default        : return "token";
    }
}

static Cell local checkPrec(p)		/* Check for valid precedence value*/
Cell p; {
    if (!isInt(p) || intOf(p)<MIN_PREC || intOf(p)>MAX_PREC) {
	ERRMSG(row) "Precedence value must be an integer in the range [%d..%d]",
		    MIN_PREC, MAX_PREC
	EEND;
    }
    return p;
}

static Cell local buildTuple(tup)	/* build tuple (x1,...,xn) from	   */
List tup; {				/* list [xn,...,x1]		   */
    Int  n = 0;
    Cell t = tup;
    Cell x;

    do {				/*    .                    .	   */
	x      = fst(t);		/*   / \                  / \	   */
	fst(t) = snd(t);		/*  xn  .                .   xn	   */
	snd(t) = x;			/*       .    ===>      .	   */
	x      = t;			/*        .            .	   */
	t      = fun(x);		/*         .          .		   */
	n++;				/*        / \        / \	   */
    } while (nonNull(t));		/*       x1  NIL   (n)  x1	   */
    fst(x) = mkTuple(n);
    return tup;
}

static List local checkCtxt(con)	/* validate context		   */
Type con; {
    mapOver(checkPred, con);
    return con;
}

static Cell local checkPred(c)		/* check that type expr is a valid */
Cell c; {				/* constraint			   */
    Cell cn = getHead(c);
#if TREX
    if (isExt(cn) && argCount==1)
	return c;
#endif
#if IPARAM
    if (isIP(cn))
	return c;
#endif
    if (!isQCon(cn) /*|| argCount==0*/)
	syntaxError("class expression");
    return c;
}

static Pair local checkDo(dqs)		/* convert reversed list of dquals */
List dqs; {				/* to an (expr,quals) pair         */
    if (isNull(dqs) || whatIs(hd(dqs))!=DOQUAL) {
	ERRMSG(row) "Last generator in do {...} must be an expression"
	EEND;
    }
    fst(dqs) = snd(fst(dqs));		/* put expression in fst of pair   */
    snd(dqs) = rev(snd(dqs));		/* & reversed list of quals in snd */
    return dqs;
}

#if MUDO
static Pair local checkMDo(dqs)		/* convert reversed list of dquals */
List dqs; {				/* to an (expr,quals) pair         */
    if (isNull(dqs) || whatIs(hd(dqs))!=DOQUAL) {
	ERRMSG(row) "Last generator in mdo {...} must be an expression"
	EEND;
    }
    fst(dqs) = snd(fst(dqs));		/* put expression in fst of pair   */
    snd(dqs) = rev(snd(dqs));		/* & reversed list of quals in snd */
    return dqs;
}
#endif

static Cell local checkTyLhs(c)		/* check that lhs is of the form   */
Cell c; {				/* T a1 ... a			   */
    Cell tlhs = c;
    while (isAp(tlhs) && whatIs(arg(tlhs))==VARIDCELL) {
	tlhs = fun(tlhs);
    }
    if (whatIs(tlhs)!=CONIDCELL) {
	ERRMSG(row) "Illegal left hand side in data type declaration"
	EEND;
    }
    return c;
}

static Cell local checkConstr(c)	/* check that data constructor has */
Cell c; {				/* an unqualified conid as head    */
    Cell chd = c;
    while (isAp(chd)) {
	chd = fun(chd);
    }
    if (whatIs(chd)==QUALIDENT) {
	ERRMSG(row) "Qualified constructor in data type declaration"
	EEND;
    }
    return c;
}

#if !TREX
static Void local noTREX(where)
String where; {
    ERRMSG(row) "Attempt to use TREX records while parsing %s.\n", where ETHEN
    ERRTEXT     "(TREX is disabled in this build of Hugs)"
    EEND;
}
#endif
#if !IPARAM
static Void local noIP(where)
String where; {
    ERRMSG(row) "Attempt to use Implicit Parameters while parsing %s.\n", where ETHEN
    ERRTEXT     "(Implicit Parameters are disabled in this build of Hugs)"
    EEND;
}
#endif

#if !MUDO
/***
   Due to the way we implement this stuff, this function will actually
   never be called. When MUDO is not defined, the lexer thinks that mdo
   is just another identifier, and hence the MDO token is never returned
   to the parser: consequently the mdo production is never reduced, making 
   this code unreachable. The alternative is to let the lexer to 
   recognize "mdo" all the time, but that's not Haskell compliant. In any 
   case we keep this function here, even if just for documentation purposes.
***/
static Void local noMDo(where)
String where; {
    ERRMSG(row) "Attempt to use MDO while parsing %s.\n", where ETHEN
    ERRTEXT     "(Recursive monadic bindings are disabled in this build of Hugs)"
    EEND;
}
#endif

/*-------------------------------------------------------------------------*/
