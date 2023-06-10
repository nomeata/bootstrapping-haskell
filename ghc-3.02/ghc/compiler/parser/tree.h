#ifndef tree_defined
#define tree_defined

#include <stdio.h>

#ifndef PROTO
#ifdef __STDC__
#define PROTO(x) x
#else
#define PROTO(x) /**/
#endif
#endif

#ifdef UGEN_DEBUG
int	fprintf PROTO((FILE *, const char *, ...));
#endif /* UGEN_DEBUG */

typedef enum {
	hmodule,
	fixop,
	ident,
	lit,
	ap,
	infixap,
	negate,
	lambda,
	let,
	casee,
	ife,
	doe,
	dobind,
	doexp,
	seqlet,
	record,
	rupdate,
	rbind,
	par,
	as,
	lazyp,
	plusp,
	wildp,
	restr,
	tuple,
	llist,
	eenum,
	comprh,
	qual,
	guard,
	lsection,
	rsection,
	ccall,
	scc
} Ttree;

typedef struct { Ttree tag; } *tree;

#ifdef __GNUC__
Ttree ttree(tree t);
extern __inline__ Ttree ttree(tree t)
{
	return(t -> tag);
}
#else  /* ! __GNUC__ */
extern Ttree ttree PROTO((tree));
#endif /* ! __GNUC__ */

struct Shmodule {
	Ttree tag;
	stringId Xghname;
	list Xghimplist;
	maybe Xghexplist;
	list Xghfixes;
	binding Xghmodlist;
	long Xghversion;
	long Xghmodline;
};

struct Sfixop {
	Ttree tag;
	qid Xgfixop;
	long Xgfixinfx;
	long Xgfixprec;
	long Xgfixline;
};

struct Sident {
	Ttree tag;
	qid Xgident;
};

struct Slit {
	Ttree tag;
	literal Xglit;
};

struct Sap {
	Ttree tag;
	tree Xgfun;
	tree Xgarg;
};

struct Sinfixap {
	Ttree tag;
	qid Xginffun;
	tree Xginfarg1;
	tree Xginfarg2;
};

struct Snegate {
	Ttree tag;
	tree Xgnexp;
};

struct Slambda {
	Ttree tag;
	list Xglampats;
	tree Xglamexpr;
	long Xglamline;
};

struct Slet {
	Ttree tag;
	binding Xgletvdefs;
	tree Xgletvexpr;
};

struct Scasee {
	Ttree tag;
	tree Xgcaseexpr;
	list Xgcasebody;
	long Xgcaseline;
};

struct Sife {
	Ttree tag;
	tree Xgifpred;
	tree Xgifthen;
	tree Xgifelse;
	long Xgifline;
};

struct Sdoe {
	Ttree tag;
	list Xgdo;
	long Xgdoline;
};

struct Sdobind {
	Ttree tag;
	tree Xgdobindpat;
	tree Xgdobindexp;
	long Xgdobindline;
};

struct Sdoexp {
	Ttree tag;
	tree Xgdoexp;
	long Xgdoexpline;
};

struct Sseqlet {
	Ttree tag;
	binding Xgseqlet;
};

struct Srecord {
	Ttree tag;
	qid Xgrcon;
	list Xgrbinds;
};

struct Srupdate {
	Ttree tag;
	tree Xgupdexp;
	list Xgupdbinds;
};

struct Srbind {
	Ttree tag;
	qid Xgrbindvar;
	maybe Xgrbindexp;
};

struct Spar {
	Ttree tag;
	tree Xgpare;
};

struct Sas {
	Ttree tag;
	qid Xgasid;
	tree Xgase;
};

struct Slazyp {
	Ttree tag;
	tree Xglazyp;
};

struct Splusp {
	Ttree tag;
	qid Xgplusp;
	literal Xgplusi;
};

struct Swildp {
	Ttree tag;
};

struct Srestr {
	Ttree tag;
	tree Xgrestre;
	ttype Xgrestrt;
};

struct Stuple {
	Ttree tag;
	list Xgtuplelist;
};

struct Sllist {
	Ttree tag;
	list Xgllist;
};

struct Seenum {
	Ttree tag;
	tree Xgefrom;
	maybe Xgestep;
	maybe Xgeto;
};

struct Scomprh {
	Ttree tag;
	tree Xgcexp;
	list Xgcquals;
};

struct Squal {
	Ttree tag;
	tree Xgqpat;
	tree Xgqexp;
};

struct Sguard {
	Ttree tag;
	tree Xggexp;
};

struct Slsection {
	Ttree tag;
	tree Xglsexp;
	qid Xglsop;
};

struct Srsection {
	Ttree tag;
	qid Xgrsop;
	tree Xgrsexp;
};

struct Sccall {
	Ttree tag;
	stringId Xgccid;
	stringId Xgccinfo;
	list Xgccargs;
};

struct Sscc {
	Ttree tag;
	hstring Xgsccid;
	tree Xgsccexp;
};

extern tree mkhmodule PROTO((stringId, list, maybe, list, binding, long, long));
#ifdef __GNUC__

stringId *Rghname PROTO((struct Shmodule *));

extern __inline__ stringId *Rghname(struct Shmodule *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghname: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghname);
}
#else  /* ! __GNUC__ */
extern stringId *Rghname PROTO((struct Shmodule *));
#endif /* ! __GNUC__ */

#define ghname(xyzxyz) (*Rghname((struct Shmodule *) (xyzxyz)))
#ifdef __GNUC__

list *Rghimplist PROTO((struct Shmodule *));

extern __inline__ list *Rghimplist(struct Shmodule *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghimplist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghimplist);
}
#else  /* ! __GNUC__ */
extern list *Rghimplist PROTO((struct Shmodule *));
#endif /* ! __GNUC__ */

#define ghimplist(xyzxyz) (*Rghimplist((struct Shmodule *) (xyzxyz)))
#ifdef __GNUC__

maybe *Rghexplist PROTO((struct Shmodule *));

extern __inline__ maybe *Rghexplist(struct Shmodule *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghexplist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghexplist);
}
#else  /* ! __GNUC__ */
extern maybe *Rghexplist PROTO((struct Shmodule *));
#endif /* ! __GNUC__ */

#define ghexplist(xyzxyz) (*Rghexplist((struct Shmodule *) (xyzxyz)))
#ifdef __GNUC__

list *Rghfixes PROTO((struct Shmodule *));

extern __inline__ list *Rghfixes(struct Shmodule *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghfixes: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghfixes);
}
#else  /* ! __GNUC__ */
extern list *Rghfixes PROTO((struct Shmodule *));
#endif /* ! __GNUC__ */

#define ghfixes(xyzxyz) (*Rghfixes((struct Shmodule *) (xyzxyz)))
#ifdef __GNUC__

binding *Rghmodlist PROTO((struct Shmodule *));

extern __inline__ binding *Rghmodlist(struct Shmodule *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghmodlist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghmodlist);
}
#else  /* ! __GNUC__ */
extern binding *Rghmodlist PROTO((struct Shmodule *));
#endif /* ! __GNUC__ */

#define ghmodlist(xyzxyz) (*Rghmodlist((struct Shmodule *) (xyzxyz)))
#ifdef __GNUC__

long *Rghversion PROTO((struct Shmodule *));

extern __inline__ long *Rghversion(struct Shmodule *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghversion: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghversion);
}
#else  /* ! __GNUC__ */
extern long *Rghversion PROTO((struct Shmodule *));
#endif /* ! __GNUC__ */

#define ghversion(xyzxyz) (*Rghversion((struct Shmodule *) (xyzxyz)))
#ifdef __GNUC__

long *Rghmodline PROTO((struct Shmodule *));

extern __inline__ long *Rghmodline(struct Shmodule *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghmodline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghmodline);
}
#else  /* ! __GNUC__ */
extern long *Rghmodline PROTO((struct Shmodule *));
#endif /* ! __GNUC__ */

#define ghmodline(xyzxyz) (*Rghmodline((struct Shmodule *) (xyzxyz)))

extern tree mkfixop PROTO((qid, long, long, long));
#ifdef __GNUC__

qid *Rgfixop PROTO((struct Sfixop *));

extern __inline__ qid *Rgfixop(struct Sfixop *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != fixop)
		fprintf(stderr,"gfixop: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfixop);
}
#else  /* ! __GNUC__ */
extern qid *Rgfixop PROTO((struct Sfixop *));
#endif /* ! __GNUC__ */

#define gfixop(xyzxyz) (*Rgfixop((struct Sfixop *) (xyzxyz)))
#ifdef __GNUC__

long *Rgfixinfx PROTO((struct Sfixop *));

extern __inline__ long *Rgfixinfx(struct Sfixop *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != fixop)
		fprintf(stderr,"gfixinfx: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfixinfx);
}
#else  /* ! __GNUC__ */
extern long *Rgfixinfx PROTO((struct Sfixop *));
#endif /* ! __GNUC__ */

#define gfixinfx(xyzxyz) (*Rgfixinfx((struct Sfixop *) (xyzxyz)))
#ifdef __GNUC__

long *Rgfixprec PROTO((struct Sfixop *));

extern __inline__ long *Rgfixprec(struct Sfixop *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != fixop)
		fprintf(stderr,"gfixprec: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfixprec);
}
#else  /* ! __GNUC__ */
extern long *Rgfixprec PROTO((struct Sfixop *));
#endif /* ! __GNUC__ */

#define gfixprec(xyzxyz) (*Rgfixprec((struct Sfixop *) (xyzxyz)))
#ifdef __GNUC__

long *Rgfixline PROTO((struct Sfixop *));

extern __inline__ long *Rgfixline(struct Sfixop *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != fixop)
		fprintf(stderr,"gfixline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfixline);
}
#else  /* ! __GNUC__ */
extern long *Rgfixline PROTO((struct Sfixop *));
#endif /* ! __GNUC__ */

#define gfixline(xyzxyz) (*Rgfixline((struct Sfixop *) (xyzxyz)))

extern tree mkident PROTO((qid));
#ifdef __GNUC__

qid *Rgident PROTO((struct Sident *));

extern __inline__ qid *Rgident(struct Sident *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ident)
		fprintf(stderr,"gident: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgident);
}
#else  /* ! __GNUC__ */
extern qid *Rgident PROTO((struct Sident *));
#endif /* ! __GNUC__ */

#define gident(xyzxyz) (*Rgident((struct Sident *) (xyzxyz)))

extern tree mklit PROTO((literal));
#ifdef __GNUC__

literal *Rglit PROTO((struct Slit *));

extern __inline__ literal *Rglit(struct Slit *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lit)
		fprintf(stderr,"glit: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglit);
}
#else  /* ! __GNUC__ */
extern literal *Rglit PROTO((struct Slit *));
#endif /* ! __GNUC__ */

#define glit(xyzxyz) (*Rglit((struct Slit *) (xyzxyz)))

extern tree mkap PROTO((tree, tree));
#ifdef __GNUC__

tree *Rgfun PROTO((struct Sap *));

extern __inline__ tree *Rgfun(struct Sap *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ap)
		fprintf(stderr,"gfun: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfun);
}
#else  /* ! __GNUC__ */
extern tree *Rgfun PROTO((struct Sap *));
#endif /* ! __GNUC__ */

#define gfun(xyzxyz) (*Rgfun((struct Sap *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgarg PROTO((struct Sap *));

extern __inline__ tree *Rgarg(struct Sap *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ap)
		fprintf(stderr,"garg: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgarg);
}
#else  /* ! __GNUC__ */
extern tree *Rgarg PROTO((struct Sap *));
#endif /* ! __GNUC__ */

#define garg(xyzxyz) (*Rgarg((struct Sap *) (xyzxyz)))

extern tree mkinfixap PROTO((qid, tree, tree));
#ifdef __GNUC__

qid *Rginffun PROTO((struct Sinfixap *));

extern __inline__ qid *Rginffun(struct Sinfixap *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != infixap)
		fprintf(stderr,"ginffun: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xginffun);
}
#else  /* ! __GNUC__ */
extern qid *Rginffun PROTO((struct Sinfixap *));
#endif /* ! __GNUC__ */

#define ginffun(xyzxyz) (*Rginffun((struct Sinfixap *) (xyzxyz)))
#ifdef __GNUC__

tree *Rginfarg1 PROTO((struct Sinfixap *));

extern __inline__ tree *Rginfarg1(struct Sinfixap *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != infixap)
		fprintf(stderr,"ginfarg1: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xginfarg1);
}
#else  /* ! __GNUC__ */
extern tree *Rginfarg1 PROTO((struct Sinfixap *));
#endif /* ! __GNUC__ */

#define ginfarg1(xyzxyz) (*Rginfarg1((struct Sinfixap *) (xyzxyz)))
#ifdef __GNUC__

tree *Rginfarg2 PROTO((struct Sinfixap *));

extern __inline__ tree *Rginfarg2(struct Sinfixap *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != infixap)
		fprintf(stderr,"ginfarg2: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xginfarg2);
}
#else  /* ! __GNUC__ */
extern tree *Rginfarg2 PROTO((struct Sinfixap *));
#endif /* ! __GNUC__ */

#define ginfarg2(xyzxyz) (*Rginfarg2((struct Sinfixap *) (xyzxyz)))

extern tree mknegate PROTO((tree));
#ifdef __GNUC__

tree *Rgnexp PROTO((struct Snegate *));

extern __inline__ tree *Rgnexp(struct Snegate *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != negate)
		fprintf(stderr,"gnexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnexp);
}
#else  /* ! __GNUC__ */
extern tree *Rgnexp PROTO((struct Snegate *));
#endif /* ! __GNUC__ */

#define gnexp(xyzxyz) (*Rgnexp((struct Snegate *) (xyzxyz)))

extern tree mklambda PROTO((list, tree, long));
#ifdef __GNUC__

list *Rglampats PROTO((struct Slambda *));

extern __inline__ list *Rglampats(struct Slambda *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lambda)
		fprintf(stderr,"glampats: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglampats);
}
#else  /* ! __GNUC__ */
extern list *Rglampats PROTO((struct Slambda *));
#endif /* ! __GNUC__ */

#define glampats(xyzxyz) (*Rglampats((struct Slambda *) (xyzxyz)))
#ifdef __GNUC__

tree *Rglamexpr PROTO((struct Slambda *));

extern __inline__ tree *Rglamexpr(struct Slambda *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lambda)
		fprintf(stderr,"glamexpr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglamexpr);
}
#else  /* ! __GNUC__ */
extern tree *Rglamexpr PROTO((struct Slambda *));
#endif /* ! __GNUC__ */

#define glamexpr(xyzxyz) (*Rglamexpr((struct Slambda *) (xyzxyz)))
#ifdef __GNUC__

long *Rglamline PROTO((struct Slambda *));

extern __inline__ long *Rglamline(struct Slambda *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lambda)
		fprintf(stderr,"glamline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglamline);
}
#else  /* ! __GNUC__ */
extern long *Rglamline PROTO((struct Slambda *));
#endif /* ! __GNUC__ */

#define glamline(xyzxyz) (*Rglamline((struct Slambda *) (xyzxyz)))

extern tree mklet PROTO((binding, tree));
#ifdef __GNUC__

binding *Rgletvdefs PROTO((struct Slet *));

extern __inline__ binding *Rgletvdefs(struct Slet *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != let)
		fprintf(stderr,"gletvdefs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgletvdefs);
}
#else  /* ! __GNUC__ */
extern binding *Rgletvdefs PROTO((struct Slet *));
#endif /* ! __GNUC__ */

#define gletvdefs(xyzxyz) (*Rgletvdefs((struct Slet *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgletvexpr PROTO((struct Slet *));

extern __inline__ tree *Rgletvexpr(struct Slet *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != let)
		fprintf(stderr,"gletvexpr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgletvexpr);
}
#else  /* ! __GNUC__ */
extern tree *Rgletvexpr PROTO((struct Slet *));
#endif /* ! __GNUC__ */

#define gletvexpr(xyzxyz) (*Rgletvexpr((struct Slet *) (xyzxyz)))

extern tree mkcasee PROTO((tree, list, long));
#ifdef __GNUC__

tree *Rgcaseexpr PROTO((struct Scasee *));

extern __inline__ tree *Rgcaseexpr(struct Scasee *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != casee)
		fprintf(stderr,"gcaseexpr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcaseexpr);
}
#else  /* ! __GNUC__ */
extern tree *Rgcaseexpr PROTO((struct Scasee *));
#endif /* ! __GNUC__ */

#define gcaseexpr(xyzxyz) (*Rgcaseexpr((struct Scasee *) (xyzxyz)))
#ifdef __GNUC__

list *Rgcasebody PROTO((struct Scasee *));

extern __inline__ list *Rgcasebody(struct Scasee *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != casee)
		fprintf(stderr,"gcasebody: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcasebody);
}
#else  /* ! __GNUC__ */
extern list *Rgcasebody PROTO((struct Scasee *));
#endif /* ! __GNUC__ */

#define gcasebody(xyzxyz) (*Rgcasebody((struct Scasee *) (xyzxyz)))
#ifdef __GNUC__

long *Rgcaseline PROTO((struct Scasee *));

extern __inline__ long *Rgcaseline(struct Scasee *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != casee)
		fprintf(stderr,"gcaseline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcaseline);
}
#else  /* ! __GNUC__ */
extern long *Rgcaseline PROTO((struct Scasee *));
#endif /* ! __GNUC__ */

#define gcaseline(xyzxyz) (*Rgcaseline((struct Scasee *) (xyzxyz)))

extern tree mkife PROTO((tree, tree, tree, long));
#ifdef __GNUC__

tree *Rgifpred PROTO((struct Sife *));

extern __inline__ tree *Rgifpred(struct Sife *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ife)
		fprintf(stderr,"gifpred: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgifpred);
}
#else  /* ! __GNUC__ */
extern tree *Rgifpred PROTO((struct Sife *));
#endif /* ! __GNUC__ */

#define gifpred(xyzxyz) (*Rgifpred((struct Sife *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgifthen PROTO((struct Sife *));

extern __inline__ tree *Rgifthen(struct Sife *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ife)
		fprintf(stderr,"gifthen: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgifthen);
}
#else  /* ! __GNUC__ */
extern tree *Rgifthen PROTO((struct Sife *));
#endif /* ! __GNUC__ */

#define gifthen(xyzxyz) (*Rgifthen((struct Sife *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgifelse PROTO((struct Sife *));

extern __inline__ tree *Rgifelse(struct Sife *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ife)
		fprintf(stderr,"gifelse: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgifelse);
}
#else  /* ! __GNUC__ */
extern tree *Rgifelse PROTO((struct Sife *));
#endif /* ! __GNUC__ */

#define gifelse(xyzxyz) (*Rgifelse((struct Sife *) (xyzxyz)))
#ifdef __GNUC__

long *Rgifline PROTO((struct Sife *));

extern __inline__ long *Rgifline(struct Sife *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ife)
		fprintf(stderr,"gifline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgifline);
}
#else  /* ! __GNUC__ */
extern long *Rgifline PROTO((struct Sife *));
#endif /* ! __GNUC__ */

#define gifline(xyzxyz) (*Rgifline((struct Sife *) (xyzxyz)))

extern tree mkdoe PROTO((list, long));
#ifdef __GNUC__

list *Rgdo PROTO((struct Sdoe *));

extern __inline__ list *Rgdo(struct Sdoe *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != doe)
		fprintf(stderr,"gdo: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdo);
}
#else  /* ! __GNUC__ */
extern list *Rgdo PROTO((struct Sdoe *));
#endif /* ! __GNUC__ */

#define gdo(xyzxyz) (*Rgdo((struct Sdoe *) (xyzxyz)))
#ifdef __GNUC__

long *Rgdoline PROTO((struct Sdoe *));

extern __inline__ long *Rgdoline(struct Sdoe *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != doe)
		fprintf(stderr,"gdoline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdoline);
}
#else  /* ! __GNUC__ */
extern long *Rgdoline PROTO((struct Sdoe *));
#endif /* ! __GNUC__ */

#define gdoline(xyzxyz) (*Rgdoline((struct Sdoe *) (xyzxyz)))

extern tree mkdobind PROTO((tree, tree, long));
#ifdef __GNUC__

tree *Rgdobindpat PROTO((struct Sdobind *));

extern __inline__ tree *Rgdobindpat(struct Sdobind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != dobind)
		fprintf(stderr,"gdobindpat: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdobindpat);
}
#else  /* ! __GNUC__ */
extern tree *Rgdobindpat PROTO((struct Sdobind *));
#endif /* ! __GNUC__ */

#define gdobindpat(xyzxyz) (*Rgdobindpat((struct Sdobind *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgdobindexp PROTO((struct Sdobind *));

extern __inline__ tree *Rgdobindexp(struct Sdobind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != dobind)
		fprintf(stderr,"gdobindexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdobindexp);
}
#else  /* ! __GNUC__ */
extern tree *Rgdobindexp PROTO((struct Sdobind *));
#endif /* ! __GNUC__ */

#define gdobindexp(xyzxyz) (*Rgdobindexp((struct Sdobind *) (xyzxyz)))
#ifdef __GNUC__

long *Rgdobindline PROTO((struct Sdobind *));

extern __inline__ long *Rgdobindline(struct Sdobind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != dobind)
		fprintf(stderr,"gdobindline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdobindline);
}
#else  /* ! __GNUC__ */
extern long *Rgdobindline PROTO((struct Sdobind *));
#endif /* ! __GNUC__ */

#define gdobindline(xyzxyz) (*Rgdobindline((struct Sdobind *) (xyzxyz)))

extern tree mkdoexp PROTO((tree, long));
#ifdef __GNUC__

tree *Rgdoexp PROTO((struct Sdoexp *));

extern __inline__ tree *Rgdoexp(struct Sdoexp *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != doexp)
		fprintf(stderr,"gdoexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdoexp);
}
#else  /* ! __GNUC__ */
extern tree *Rgdoexp PROTO((struct Sdoexp *));
#endif /* ! __GNUC__ */

#define gdoexp(xyzxyz) (*Rgdoexp((struct Sdoexp *) (xyzxyz)))
#ifdef __GNUC__

long *Rgdoexpline PROTO((struct Sdoexp *));

extern __inline__ long *Rgdoexpline(struct Sdoexp *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != doexp)
		fprintf(stderr,"gdoexpline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdoexpline);
}
#else  /* ! __GNUC__ */
extern long *Rgdoexpline PROTO((struct Sdoexp *));
#endif /* ! __GNUC__ */

#define gdoexpline(xyzxyz) (*Rgdoexpline((struct Sdoexp *) (xyzxyz)))

extern tree mkseqlet PROTO((binding));
#ifdef __GNUC__

binding *Rgseqlet PROTO((struct Sseqlet *));

extern __inline__ binding *Rgseqlet(struct Sseqlet *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != seqlet)
		fprintf(stderr,"gseqlet: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgseqlet);
}
#else  /* ! __GNUC__ */
extern binding *Rgseqlet PROTO((struct Sseqlet *));
#endif /* ! __GNUC__ */

#define gseqlet(xyzxyz) (*Rgseqlet((struct Sseqlet *) (xyzxyz)))

extern tree mkrecord PROTO((qid, list));
#ifdef __GNUC__

qid *Rgrcon PROTO((struct Srecord *));

extern __inline__ qid *Rgrcon(struct Srecord *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != record)
		fprintf(stderr,"grcon: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrcon);
}
#else  /* ! __GNUC__ */
extern qid *Rgrcon PROTO((struct Srecord *));
#endif /* ! __GNUC__ */

#define grcon(xyzxyz) (*Rgrcon((struct Srecord *) (xyzxyz)))
#ifdef __GNUC__

list *Rgrbinds PROTO((struct Srecord *));

extern __inline__ list *Rgrbinds(struct Srecord *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != record)
		fprintf(stderr,"grbinds: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrbinds);
}
#else  /* ! __GNUC__ */
extern list *Rgrbinds PROTO((struct Srecord *));
#endif /* ! __GNUC__ */

#define grbinds(xyzxyz) (*Rgrbinds((struct Srecord *) (xyzxyz)))

extern tree mkrupdate PROTO((tree, list));
#ifdef __GNUC__

tree *Rgupdexp PROTO((struct Srupdate *));

extern __inline__ tree *Rgupdexp(struct Srupdate *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != rupdate)
		fprintf(stderr,"gupdexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgupdexp);
}
#else  /* ! __GNUC__ */
extern tree *Rgupdexp PROTO((struct Srupdate *));
#endif /* ! __GNUC__ */

#define gupdexp(xyzxyz) (*Rgupdexp((struct Srupdate *) (xyzxyz)))
#ifdef __GNUC__

list *Rgupdbinds PROTO((struct Srupdate *));

extern __inline__ list *Rgupdbinds(struct Srupdate *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != rupdate)
		fprintf(stderr,"gupdbinds: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgupdbinds);
}
#else  /* ! __GNUC__ */
extern list *Rgupdbinds PROTO((struct Srupdate *));
#endif /* ! __GNUC__ */

#define gupdbinds(xyzxyz) (*Rgupdbinds((struct Srupdate *) (xyzxyz)))

extern tree mkrbind PROTO((qid, maybe));
#ifdef __GNUC__

qid *Rgrbindvar PROTO((struct Srbind *));

extern __inline__ qid *Rgrbindvar(struct Srbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != rbind)
		fprintf(stderr,"grbindvar: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrbindvar);
}
#else  /* ! __GNUC__ */
extern qid *Rgrbindvar PROTO((struct Srbind *));
#endif /* ! __GNUC__ */

#define grbindvar(xyzxyz) (*Rgrbindvar((struct Srbind *) (xyzxyz)))
#ifdef __GNUC__

maybe *Rgrbindexp PROTO((struct Srbind *));

extern __inline__ maybe *Rgrbindexp(struct Srbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != rbind)
		fprintf(stderr,"grbindexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrbindexp);
}
#else  /* ! __GNUC__ */
extern maybe *Rgrbindexp PROTO((struct Srbind *));
#endif /* ! __GNUC__ */

#define grbindexp(xyzxyz) (*Rgrbindexp((struct Srbind *) (xyzxyz)))

extern tree mkpar PROTO((tree));
#ifdef __GNUC__

tree *Rgpare PROTO((struct Spar *));

extern __inline__ tree *Rgpare(struct Spar *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != par)
		fprintf(stderr,"gpare: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgpare);
}
#else  /* ! __GNUC__ */
extern tree *Rgpare PROTO((struct Spar *));
#endif /* ! __GNUC__ */

#define gpare(xyzxyz) (*Rgpare((struct Spar *) (xyzxyz)))

extern tree mkas PROTO((qid, tree));
#ifdef __GNUC__

qid *Rgasid PROTO((struct Sas *));

extern __inline__ qid *Rgasid(struct Sas *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != as)
		fprintf(stderr,"gasid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgasid);
}
#else  /* ! __GNUC__ */
extern qid *Rgasid PROTO((struct Sas *));
#endif /* ! __GNUC__ */

#define gasid(xyzxyz) (*Rgasid((struct Sas *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgase PROTO((struct Sas *));

extern __inline__ tree *Rgase(struct Sas *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != as)
		fprintf(stderr,"gase: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgase);
}
#else  /* ! __GNUC__ */
extern tree *Rgase PROTO((struct Sas *));
#endif /* ! __GNUC__ */

#define gase(xyzxyz) (*Rgase((struct Sas *) (xyzxyz)))

extern tree mklazyp PROTO((tree));
#ifdef __GNUC__

tree *Rglazyp PROTO((struct Slazyp *));

extern __inline__ tree *Rglazyp(struct Slazyp *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lazyp)
		fprintf(stderr,"glazyp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglazyp);
}
#else  /* ! __GNUC__ */
extern tree *Rglazyp PROTO((struct Slazyp *));
#endif /* ! __GNUC__ */

#define glazyp(xyzxyz) (*Rglazyp((struct Slazyp *) (xyzxyz)))

extern tree mkplusp PROTO((qid, literal));
#ifdef __GNUC__

qid *Rgplusp PROTO((struct Splusp *));

extern __inline__ qid *Rgplusp(struct Splusp *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != plusp)
		fprintf(stderr,"gplusp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgplusp);
}
#else  /* ! __GNUC__ */
extern qid *Rgplusp PROTO((struct Splusp *));
#endif /* ! __GNUC__ */

#define gplusp(xyzxyz) (*Rgplusp((struct Splusp *) (xyzxyz)))
#ifdef __GNUC__

literal *Rgplusi PROTO((struct Splusp *));

extern __inline__ literal *Rgplusi(struct Splusp *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != plusp)
		fprintf(stderr,"gplusi: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgplusi);
}
#else  /* ! __GNUC__ */
extern literal *Rgplusi PROTO((struct Splusp *));
#endif /* ! __GNUC__ */

#define gplusi(xyzxyz) (*Rgplusi((struct Splusp *) (xyzxyz)))

extern tree mkwildp PROTO((void));

extern tree mkrestr PROTO((tree, ttype));
#ifdef __GNUC__

tree *Rgrestre PROTO((struct Srestr *));

extern __inline__ tree *Rgrestre(struct Srestr *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != restr)
		fprintf(stderr,"grestre: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrestre);
}
#else  /* ! __GNUC__ */
extern tree *Rgrestre PROTO((struct Srestr *));
#endif /* ! __GNUC__ */

#define grestre(xyzxyz) (*Rgrestre((struct Srestr *) (xyzxyz)))
#ifdef __GNUC__

ttype *Rgrestrt PROTO((struct Srestr *));

extern __inline__ ttype *Rgrestrt(struct Srestr *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != restr)
		fprintf(stderr,"grestrt: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrestrt);
}
#else  /* ! __GNUC__ */
extern ttype *Rgrestrt PROTO((struct Srestr *));
#endif /* ! __GNUC__ */

#define grestrt(xyzxyz) (*Rgrestrt((struct Srestr *) (xyzxyz)))

extern tree mktuple PROTO((list));
#ifdef __GNUC__

list *Rgtuplelist PROTO((struct Stuple *));

extern __inline__ list *Rgtuplelist(struct Stuple *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != tuple)
		fprintf(stderr,"gtuplelist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtuplelist);
}
#else  /* ! __GNUC__ */
extern list *Rgtuplelist PROTO((struct Stuple *));
#endif /* ! __GNUC__ */

#define gtuplelist(xyzxyz) (*Rgtuplelist((struct Stuple *) (xyzxyz)))

extern tree mkllist PROTO((list));
#ifdef __GNUC__

list *Rgllist PROTO((struct Sllist *));

extern __inline__ list *Rgllist(struct Sllist *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != llist)
		fprintf(stderr,"gllist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgllist);
}
#else  /* ! __GNUC__ */
extern list *Rgllist PROTO((struct Sllist *));
#endif /* ! __GNUC__ */

#define gllist(xyzxyz) (*Rgllist((struct Sllist *) (xyzxyz)))

extern tree mkeenum PROTO((tree, maybe, maybe));
#ifdef __GNUC__

tree *Rgefrom PROTO((struct Seenum *));

extern __inline__ tree *Rgefrom(struct Seenum *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != eenum)
		fprintf(stderr,"gefrom: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgefrom);
}
#else  /* ! __GNUC__ */
extern tree *Rgefrom PROTO((struct Seenum *));
#endif /* ! __GNUC__ */

#define gefrom(xyzxyz) (*Rgefrom((struct Seenum *) (xyzxyz)))
#ifdef __GNUC__

maybe *Rgestep PROTO((struct Seenum *));

extern __inline__ maybe *Rgestep(struct Seenum *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != eenum)
		fprintf(stderr,"gestep: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgestep);
}
#else  /* ! __GNUC__ */
extern maybe *Rgestep PROTO((struct Seenum *));
#endif /* ! __GNUC__ */

#define gestep(xyzxyz) (*Rgestep((struct Seenum *) (xyzxyz)))
#ifdef __GNUC__

maybe *Rgeto PROTO((struct Seenum *));

extern __inline__ maybe *Rgeto(struct Seenum *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != eenum)
		fprintf(stderr,"geto: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgeto);
}
#else  /* ! __GNUC__ */
extern maybe *Rgeto PROTO((struct Seenum *));
#endif /* ! __GNUC__ */

#define geto(xyzxyz) (*Rgeto((struct Seenum *) (xyzxyz)))

extern tree mkcomprh PROTO((tree, list));
#ifdef __GNUC__

tree *Rgcexp PROTO((struct Scomprh *));

extern __inline__ tree *Rgcexp(struct Scomprh *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != comprh)
		fprintf(stderr,"gcexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcexp);
}
#else  /* ! __GNUC__ */
extern tree *Rgcexp PROTO((struct Scomprh *));
#endif /* ! __GNUC__ */

#define gcexp(xyzxyz) (*Rgcexp((struct Scomprh *) (xyzxyz)))
#ifdef __GNUC__

list *Rgcquals PROTO((struct Scomprh *));

extern __inline__ list *Rgcquals(struct Scomprh *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != comprh)
		fprintf(stderr,"gcquals: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcquals);
}
#else  /* ! __GNUC__ */
extern list *Rgcquals PROTO((struct Scomprh *));
#endif /* ! __GNUC__ */

#define gcquals(xyzxyz) (*Rgcquals((struct Scomprh *) (xyzxyz)))

extern tree mkqual PROTO((tree, tree));
#ifdef __GNUC__

tree *Rgqpat PROTO((struct Squal *));

extern __inline__ tree *Rgqpat(struct Squal *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != qual)
		fprintf(stderr,"gqpat: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgqpat);
}
#else  /* ! __GNUC__ */
extern tree *Rgqpat PROTO((struct Squal *));
#endif /* ! __GNUC__ */

#define gqpat(xyzxyz) (*Rgqpat((struct Squal *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgqexp PROTO((struct Squal *));

extern __inline__ tree *Rgqexp(struct Squal *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != qual)
		fprintf(stderr,"gqexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgqexp);
}
#else  /* ! __GNUC__ */
extern tree *Rgqexp PROTO((struct Squal *));
#endif /* ! __GNUC__ */

#define gqexp(xyzxyz) (*Rgqexp((struct Squal *) (xyzxyz)))

extern tree mkguard PROTO((tree));
#ifdef __GNUC__

tree *Rggexp PROTO((struct Sguard *));

extern __inline__ tree *Rggexp(struct Sguard *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != guard)
		fprintf(stderr,"ggexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggexp);
}
#else  /* ! __GNUC__ */
extern tree *Rggexp PROTO((struct Sguard *));
#endif /* ! __GNUC__ */

#define ggexp(xyzxyz) (*Rggexp((struct Sguard *) (xyzxyz)))

extern tree mklsection PROTO((tree, qid));
#ifdef __GNUC__

tree *Rglsexp PROTO((struct Slsection *));

extern __inline__ tree *Rglsexp(struct Slsection *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lsection)
		fprintf(stderr,"glsexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglsexp);
}
#else  /* ! __GNUC__ */
extern tree *Rglsexp PROTO((struct Slsection *));
#endif /* ! __GNUC__ */

#define glsexp(xyzxyz) (*Rglsexp((struct Slsection *) (xyzxyz)))
#ifdef __GNUC__

qid *Rglsop PROTO((struct Slsection *));

extern __inline__ qid *Rglsop(struct Slsection *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lsection)
		fprintf(stderr,"glsop: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglsop);
}
#else  /* ! __GNUC__ */
extern qid *Rglsop PROTO((struct Slsection *));
#endif /* ! __GNUC__ */

#define glsop(xyzxyz) (*Rglsop((struct Slsection *) (xyzxyz)))

extern tree mkrsection PROTO((qid, tree));
#ifdef __GNUC__

qid *Rgrsop PROTO((struct Srsection *));

extern __inline__ qid *Rgrsop(struct Srsection *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != rsection)
		fprintf(stderr,"grsop: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrsop);
}
#else  /* ! __GNUC__ */
extern qid *Rgrsop PROTO((struct Srsection *));
#endif /* ! __GNUC__ */

#define grsop(xyzxyz) (*Rgrsop((struct Srsection *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgrsexp PROTO((struct Srsection *));

extern __inline__ tree *Rgrsexp(struct Srsection *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != rsection)
		fprintf(stderr,"grsexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrsexp);
}
#else  /* ! __GNUC__ */
extern tree *Rgrsexp PROTO((struct Srsection *));
#endif /* ! __GNUC__ */

#define grsexp(xyzxyz) (*Rgrsexp((struct Srsection *) (xyzxyz)))

extern tree mkccall PROTO((stringId, stringId, list));
#ifdef __GNUC__

stringId *Rgccid PROTO((struct Sccall *));

extern __inline__ stringId *Rgccid(struct Sccall *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ccall)
		fprintf(stderr,"gccid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgccid);
}
#else  /* ! __GNUC__ */
extern stringId *Rgccid PROTO((struct Sccall *));
#endif /* ! __GNUC__ */

#define gccid(xyzxyz) (*Rgccid((struct Sccall *) (xyzxyz)))
#ifdef __GNUC__

stringId *Rgccinfo PROTO((struct Sccall *));

extern __inline__ stringId *Rgccinfo(struct Sccall *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ccall)
		fprintf(stderr,"gccinfo: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgccinfo);
}
#else  /* ! __GNUC__ */
extern stringId *Rgccinfo PROTO((struct Sccall *));
#endif /* ! __GNUC__ */

#define gccinfo(xyzxyz) (*Rgccinfo((struct Sccall *) (xyzxyz)))
#ifdef __GNUC__

list *Rgccargs PROTO((struct Sccall *));

extern __inline__ list *Rgccargs(struct Sccall *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ccall)
		fprintf(stderr,"gccargs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgccargs);
}
#else  /* ! __GNUC__ */
extern list *Rgccargs PROTO((struct Sccall *));
#endif /* ! __GNUC__ */

#define gccargs(xyzxyz) (*Rgccargs((struct Sccall *) (xyzxyz)))

extern tree mkscc PROTO((hstring, tree));
#ifdef __GNUC__

hstring *Rgsccid PROTO((struct Sscc *));

extern __inline__ hstring *Rgsccid(struct Sscc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != scc)
		fprintf(stderr,"gsccid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgsccid);
}
#else  /* ! __GNUC__ */
extern hstring *Rgsccid PROTO((struct Sscc *));
#endif /* ! __GNUC__ */

#define gsccid(xyzxyz) (*Rgsccid((struct Sscc *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgsccexp PROTO((struct Sscc *));

extern __inline__ tree *Rgsccexp(struct Sscc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != scc)
		fprintf(stderr,"gsccexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgsccexp);
}
#else  /* ! __GNUC__ */
extern tree *Rgsccexp PROTO((struct Sscc *));
#endif /* ! __GNUC__ */

#define gsccexp(xyzxyz) (*Rgsccexp((struct Sscc *) (xyzxyz)))

#endif
