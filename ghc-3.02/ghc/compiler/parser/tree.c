

#include "hspincl.h"
#include "parser/tree.h"

Ttree ttree(t)
 tree t;
{
	return(t -> tag);
}


/************** hmodule ******************/

tree mkhmodule(PPghname, PPghimplist, PPghexplist, PPghfixes, PPghmodlist, PPghversion, PPghmodline)
 stringId PPghname;
 list PPghimplist;
 maybe PPghexplist;
 list PPghfixes;
 binding PPghmodlist;
 long PPghversion;
 long PPghmodline;
{
	register struct Shmodule *pp =
		(struct Shmodule *) malloc(sizeof(struct Shmodule));
	pp -> tag = hmodule;
	pp -> Xghname = PPghname;
	pp -> Xghimplist = PPghimplist;
	pp -> Xghexplist = PPghexplist;
	pp -> Xghfixes = PPghfixes;
	pp -> Xghmodlist = PPghmodlist;
	pp -> Xghversion = PPghversion;
	pp -> Xghmodline = PPghmodline;
	return((tree)pp);
}

stringId *Rghname(t)
 struct Shmodule *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghname: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghname);
}

list *Rghimplist(t)
 struct Shmodule *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghimplist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghimplist);
}

maybe *Rghexplist(t)
 struct Shmodule *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghexplist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghexplist);
}

list *Rghfixes(t)
 struct Shmodule *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghfixes: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghfixes);
}

binding *Rghmodlist(t)
 struct Shmodule *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghmodlist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghmodlist);
}

long *Rghversion(t)
 struct Shmodule *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghversion: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghversion);
}

long *Rghmodline(t)
 struct Shmodule *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghmodline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghmodline);
}

/************** fixop ******************/

tree mkfixop(PPgfixop, PPgfixinfx, PPgfixprec, PPgfixline)
 qid PPgfixop;
 long PPgfixinfx;
 long PPgfixprec;
 long PPgfixline;
{
	register struct Sfixop *pp =
		(struct Sfixop *) malloc(sizeof(struct Sfixop));
	pp -> tag = fixop;
	pp -> Xgfixop = PPgfixop;
	pp -> Xgfixinfx = PPgfixinfx;
	pp -> Xgfixprec = PPgfixprec;
	pp -> Xgfixline = PPgfixline;
	return((tree)pp);
}

qid *Rgfixop(t)
 struct Sfixop *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != fixop)
		fprintf(stderr,"gfixop: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfixop);
}

long *Rgfixinfx(t)
 struct Sfixop *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != fixop)
		fprintf(stderr,"gfixinfx: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfixinfx);
}

long *Rgfixprec(t)
 struct Sfixop *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != fixop)
		fprintf(stderr,"gfixprec: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfixprec);
}

long *Rgfixline(t)
 struct Sfixop *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != fixop)
		fprintf(stderr,"gfixline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfixline);
}

/************** ident ******************/

tree mkident(PPgident)
 qid PPgident;
{
	register struct Sident *pp =
		(struct Sident *) malloc(sizeof(struct Sident));
	pp -> tag = ident;
	pp -> Xgident = PPgident;
	return((tree)pp);
}

qid *Rgident(t)
 struct Sident *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ident)
		fprintf(stderr,"gident: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgident);
}

/************** lit ******************/

tree mklit(PPglit)
 literal PPglit;
{
	register struct Slit *pp =
		(struct Slit *) malloc(sizeof(struct Slit));
	pp -> tag = lit;
	pp -> Xglit = PPglit;
	return((tree)pp);
}

literal *Rglit(t)
 struct Slit *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lit)
		fprintf(stderr,"glit: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglit);
}

/************** ap ******************/

tree mkap(PPgfun, PPgarg)
 tree PPgfun;
 tree PPgarg;
{
	register struct Sap *pp =
		(struct Sap *) malloc(sizeof(struct Sap));
	pp -> tag = ap;
	pp -> Xgfun = PPgfun;
	pp -> Xgarg = PPgarg;
	return((tree)pp);
}

tree *Rgfun(t)
 struct Sap *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ap)
		fprintf(stderr,"gfun: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfun);
}

tree *Rgarg(t)
 struct Sap *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ap)
		fprintf(stderr,"garg: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgarg);
}

/************** infixap ******************/

tree mkinfixap(PPginffun, PPginfarg1, PPginfarg2)
 qid PPginffun;
 tree PPginfarg1;
 tree PPginfarg2;
{
	register struct Sinfixap *pp =
		(struct Sinfixap *) malloc(sizeof(struct Sinfixap));
	pp -> tag = infixap;
	pp -> Xginffun = PPginffun;
	pp -> Xginfarg1 = PPginfarg1;
	pp -> Xginfarg2 = PPginfarg2;
	return((tree)pp);
}

qid *Rginffun(t)
 struct Sinfixap *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != infixap)
		fprintf(stderr,"ginffun: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xginffun);
}

tree *Rginfarg1(t)
 struct Sinfixap *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != infixap)
		fprintf(stderr,"ginfarg1: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xginfarg1);
}

tree *Rginfarg2(t)
 struct Sinfixap *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != infixap)
		fprintf(stderr,"ginfarg2: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xginfarg2);
}

/************** negate ******************/

tree mknegate(PPgnexp)
 tree PPgnexp;
{
	register struct Snegate *pp =
		(struct Snegate *) malloc(sizeof(struct Snegate));
	pp -> tag = negate;
	pp -> Xgnexp = PPgnexp;
	return((tree)pp);
}

tree *Rgnexp(t)
 struct Snegate *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != negate)
		fprintf(stderr,"gnexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnexp);
}

/************** lambda ******************/

tree mklambda(PPglampats, PPglamexpr, PPglamline)
 list PPglampats;
 tree PPglamexpr;
 long PPglamline;
{
	register struct Slambda *pp =
		(struct Slambda *) malloc(sizeof(struct Slambda));
	pp -> tag = lambda;
	pp -> Xglampats = PPglampats;
	pp -> Xglamexpr = PPglamexpr;
	pp -> Xglamline = PPglamline;
	return((tree)pp);
}

list *Rglampats(t)
 struct Slambda *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lambda)
		fprintf(stderr,"glampats: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglampats);
}

tree *Rglamexpr(t)
 struct Slambda *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lambda)
		fprintf(stderr,"glamexpr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglamexpr);
}

long *Rglamline(t)
 struct Slambda *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lambda)
		fprintf(stderr,"glamline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglamline);
}

/************** let ******************/

tree mklet(PPgletvdefs, PPgletvexpr)
 binding PPgletvdefs;
 tree PPgletvexpr;
{
	register struct Slet *pp =
		(struct Slet *) malloc(sizeof(struct Slet));
	pp -> tag = let;
	pp -> Xgletvdefs = PPgletvdefs;
	pp -> Xgletvexpr = PPgletvexpr;
	return((tree)pp);
}

binding *Rgletvdefs(t)
 struct Slet *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != let)
		fprintf(stderr,"gletvdefs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgletvdefs);
}

tree *Rgletvexpr(t)
 struct Slet *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != let)
		fprintf(stderr,"gletvexpr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgletvexpr);
}

/************** casee ******************/

tree mkcasee(PPgcaseexpr, PPgcasebody, PPgcaseline)
 tree PPgcaseexpr;
 list PPgcasebody;
 long PPgcaseline;
{
	register struct Scasee *pp =
		(struct Scasee *) malloc(sizeof(struct Scasee));
	pp -> tag = casee;
	pp -> Xgcaseexpr = PPgcaseexpr;
	pp -> Xgcasebody = PPgcasebody;
	pp -> Xgcaseline = PPgcaseline;
	return((tree)pp);
}

tree *Rgcaseexpr(t)
 struct Scasee *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != casee)
		fprintf(stderr,"gcaseexpr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcaseexpr);
}

list *Rgcasebody(t)
 struct Scasee *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != casee)
		fprintf(stderr,"gcasebody: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcasebody);
}

long *Rgcaseline(t)
 struct Scasee *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != casee)
		fprintf(stderr,"gcaseline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcaseline);
}

/************** ife ******************/

tree mkife(PPgifpred, PPgifthen, PPgifelse, PPgifline)
 tree PPgifpred;
 tree PPgifthen;
 tree PPgifelse;
 long PPgifline;
{
	register struct Sife *pp =
		(struct Sife *) malloc(sizeof(struct Sife));
	pp -> tag = ife;
	pp -> Xgifpred = PPgifpred;
	pp -> Xgifthen = PPgifthen;
	pp -> Xgifelse = PPgifelse;
	pp -> Xgifline = PPgifline;
	return((tree)pp);
}

tree *Rgifpred(t)
 struct Sife *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ife)
		fprintf(stderr,"gifpred: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgifpred);
}

tree *Rgifthen(t)
 struct Sife *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ife)
		fprintf(stderr,"gifthen: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgifthen);
}

tree *Rgifelse(t)
 struct Sife *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ife)
		fprintf(stderr,"gifelse: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgifelse);
}

long *Rgifline(t)
 struct Sife *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ife)
		fprintf(stderr,"gifline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgifline);
}

/************** doe ******************/

tree mkdoe(PPgdo, PPgdoline)
 list PPgdo;
 long PPgdoline;
{
	register struct Sdoe *pp =
		(struct Sdoe *) malloc(sizeof(struct Sdoe));
	pp -> tag = doe;
	pp -> Xgdo = PPgdo;
	pp -> Xgdoline = PPgdoline;
	return((tree)pp);
}

list *Rgdo(t)
 struct Sdoe *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != doe)
		fprintf(stderr,"gdo: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdo);
}

long *Rgdoline(t)
 struct Sdoe *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != doe)
		fprintf(stderr,"gdoline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdoline);
}

/************** dobind ******************/

tree mkdobind(PPgdobindpat, PPgdobindexp, PPgdobindline)
 tree PPgdobindpat;
 tree PPgdobindexp;
 long PPgdobindline;
{
	register struct Sdobind *pp =
		(struct Sdobind *) malloc(sizeof(struct Sdobind));
	pp -> tag = dobind;
	pp -> Xgdobindpat = PPgdobindpat;
	pp -> Xgdobindexp = PPgdobindexp;
	pp -> Xgdobindline = PPgdobindline;
	return((tree)pp);
}

tree *Rgdobindpat(t)
 struct Sdobind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != dobind)
		fprintf(stderr,"gdobindpat: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdobindpat);
}

tree *Rgdobindexp(t)
 struct Sdobind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != dobind)
		fprintf(stderr,"gdobindexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdobindexp);
}

long *Rgdobindline(t)
 struct Sdobind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != dobind)
		fprintf(stderr,"gdobindline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdobindline);
}

/************** doexp ******************/

tree mkdoexp(PPgdoexp, PPgdoexpline)
 tree PPgdoexp;
 long PPgdoexpline;
{
	register struct Sdoexp *pp =
		(struct Sdoexp *) malloc(sizeof(struct Sdoexp));
	pp -> tag = doexp;
	pp -> Xgdoexp = PPgdoexp;
	pp -> Xgdoexpline = PPgdoexpline;
	return((tree)pp);
}

tree *Rgdoexp(t)
 struct Sdoexp *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != doexp)
		fprintf(stderr,"gdoexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdoexp);
}

long *Rgdoexpline(t)
 struct Sdoexp *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != doexp)
		fprintf(stderr,"gdoexpline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdoexpline);
}

/************** seqlet ******************/

tree mkseqlet(PPgseqlet)
 binding PPgseqlet;
{
	register struct Sseqlet *pp =
		(struct Sseqlet *) malloc(sizeof(struct Sseqlet));
	pp -> tag = seqlet;
	pp -> Xgseqlet = PPgseqlet;
	return((tree)pp);
}

binding *Rgseqlet(t)
 struct Sseqlet *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != seqlet)
		fprintf(stderr,"gseqlet: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgseqlet);
}

/************** record ******************/

tree mkrecord(PPgrcon, PPgrbinds)
 qid PPgrcon;
 list PPgrbinds;
{
	register struct Srecord *pp =
		(struct Srecord *) malloc(sizeof(struct Srecord));
	pp -> tag = record;
	pp -> Xgrcon = PPgrcon;
	pp -> Xgrbinds = PPgrbinds;
	return((tree)pp);
}

qid *Rgrcon(t)
 struct Srecord *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != record)
		fprintf(stderr,"grcon: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrcon);
}

list *Rgrbinds(t)
 struct Srecord *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != record)
		fprintf(stderr,"grbinds: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrbinds);
}

/************** rupdate ******************/

tree mkrupdate(PPgupdexp, PPgupdbinds)
 tree PPgupdexp;
 list PPgupdbinds;
{
	register struct Srupdate *pp =
		(struct Srupdate *) malloc(sizeof(struct Srupdate));
	pp -> tag = rupdate;
	pp -> Xgupdexp = PPgupdexp;
	pp -> Xgupdbinds = PPgupdbinds;
	return((tree)pp);
}

tree *Rgupdexp(t)
 struct Srupdate *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != rupdate)
		fprintf(stderr,"gupdexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgupdexp);
}

list *Rgupdbinds(t)
 struct Srupdate *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != rupdate)
		fprintf(stderr,"gupdbinds: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgupdbinds);
}

/************** rbind ******************/

tree mkrbind(PPgrbindvar, PPgrbindexp)
 qid PPgrbindvar;
 maybe PPgrbindexp;
{
	register struct Srbind *pp =
		(struct Srbind *) malloc(sizeof(struct Srbind));
	pp -> tag = rbind;
	pp -> Xgrbindvar = PPgrbindvar;
	pp -> Xgrbindexp = PPgrbindexp;
	return((tree)pp);
}

qid *Rgrbindvar(t)
 struct Srbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != rbind)
		fprintf(stderr,"grbindvar: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrbindvar);
}

maybe *Rgrbindexp(t)
 struct Srbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != rbind)
		fprintf(stderr,"grbindexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrbindexp);
}

/************** par ******************/

tree mkpar(PPgpare)
 tree PPgpare;
{
	register struct Spar *pp =
		(struct Spar *) malloc(sizeof(struct Spar));
	pp -> tag = par;
	pp -> Xgpare = PPgpare;
	return((tree)pp);
}

tree *Rgpare(t)
 struct Spar *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != par)
		fprintf(stderr,"gpare: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgpare);
}

/************** as ******************/

tree mkas(PPgasid, PPgase)
 qid PPgasid;
 tree PPgase;
{
	register struct Sas *pp =
		(struct Sas *) malloc(sizeof(struct Sas));
	pp -> tag = as;
	pp -> Xgasid = PPgasid;
	pp -> Xgase = PPgase;
	return((tree)pp);
}

qid *Rgasid(t)
 struct Sas *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != as)
		fprintf(stderr,"gasid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgasid);
}

tree *Rgase(t)
 struct Sas *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != as)
		fprintf(stderr,"gase: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgase);
}

/************** lazyp ******************/

tree mklazyp(PPglazyp)
 tree PPglazyp;
{
	register struct Slazyp *pp =
		(struct Slazyp *) malloc(sizeof(struct Slazyp));
	pp -> tag = lazyp;
	pp -> Xglazyp = PPglazyp;
	return((tree)pp);
}

tree *Rglazyp(t)
 struct Slazyp *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lazyp)
		fprintf(stderr,"glazyp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglazyp);
}

/************** plusp ******************/

tree mkplusp(PPgplusp, PPgplusi)
 qid PPgplusp;
 literal PPgplusi;
{
	register struct Splusp *pp =
		(struct Splusp *) malloc(sizeof(struct Splusp));
	pp -> tag = plusp;
	pp -> Xgplusp = PPgplusp;
	pp -> Xgplusi = PPgplusi;
	return((tree)pp);
}

qid *Rgplusp(t)
 struct Splusp *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != plusp)
		fprintf(stderr,"gplusp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgplusp);
}

literal *Rgplusi(t)
 struct Splusp *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != plusp)
		fprintf(stderr,"gplusi: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgplusi);
}

/************** wildp ******************/

tree mkwildp(void)
{
	register struct Swildp *pp =
		(struct Swildp *) malloc(sizeof(struct Swildp));
	pp -> tag = wildp;
	return((tree)pp);
}

/************** restr ******************/

tree mkrestr(PPgrestre, PPgrestrt)
 tree PPgrestre;
 ttype PPgrestrt;
{
	register struct Srestr *pp =
		(struct Srestr *) malloc(sizeof(struct Srestr));
	pp -> tag = restr;
	pp -> Xgrestre = PPgrestre;
	pp -> Xgrestrt = PPgrestrt;
	return((tree)pp);
}

tree *Rgrestre(t)
 struct Srestr *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != restr)
		fprintf(stderr,"grestre: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrestre);
}

ttype *Rgrestrt(t)
 struct Srestr *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != restr)
		fprintf(stderr,"grestrt: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrestrt);
}

/************** tuple ******************/

tree mktuple(PPgtuplelist)
 list PPgtuplelist;
{
	register struct Stuple *pp =
		(struct Stuple *) malloc(sizeof(struct Stuple));
	pp -> tag = tuple;
	pp -> Xgtuplelist = PPgtuplelist;
	return((tree)pp);
}

list *Rgtuplelist(t)
 struct Stuple *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != tuple)
		fprintf(stderr,"gtuplelist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtuplelist);
}

/************** llist ******************/

tree mkllist(PPgllist)
 list PPgllist;
{
	register struct Sllist *pp =
		(struct Sllist *) malloc(sizeof(struct Sllist));
	pp -> tag = llist;
	pp -> Xgllist = PPgllist;
	return((tree)pp);
}

list *Rgllist(t)
 struct Sllist *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != llist)
		fprintf(stderr,"gllist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgllist);
}

/************** eenum ******************/

tree mkeenum(PPgefrom, PPgestep, PPgeto)
 tree PPgefrom;
 maybe PPgestep;
 maybe PPgeto;
{
	register struct Seenum *pp =
		(struct Seenum *) malloc(sizeof(struct Seenum));
	pp -> tag = eenum;
	pp -> Xgefrom = PPgefrom;
	pp -> Xgestep = PPgestep;
	pp -> Xgeto = PPgeto;
	return((tree)pp);
}

tree *Rgefrom(t)
 struct Seenum *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != eenum)
		fprintf(stderr,"gefrom: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgefrom);
}

maybe *Rgestep(t)
 struct Seenum *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != eenum)
		fprintf(stderr,"gestep: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgestep);
}

maybe *Rgeto(t)
 struct Seenum *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != eenum)
		fprintf(stderr,"geto: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgeto);
}

/************** comprh ******************/

tree mkcomprh(PPgcexp, PPgcquals)
 tree PPgcexp;
 list PPgcquals;
{
	register struct Scomprh *pp =
		(struct Scomprh *) malloc(sizeof(struct Scomprh));
	pp -> tag = comprh;
	pp -> Xgcexp = PPgcexp;
	pp -> Xgcquals = PPgcquals;
	return((tree)pp);
}

tree *Rgcexp(t)
 struct Scomprh *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != comprh)
		fprintf(stderr,"gcexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcexp);
}

list *Rgcquals(t)
 struct Scomprh *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != comprh)
		fprintf(stderr,"gcquals: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcquals);
}

/************** qual ******************/

tree mkqual(PPgqpat, PPgqexp)
 tree PPgqpat;
 tree PPgqexp;
{
	register struct Squal *pp =
		(struct Squal *) malloc(sizeof(struct Squal));
	pp -> tag = qual;
	pp -> Xgqpat = PPgqpat;
	pp -> Xgqexp = PPgqexp;
	return((tree)pp);
}

tree *Rgqpat(t)
 struct Squal *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != qual)
		fprintf(stderr,"gqpat: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgqpat);
}

tree *Rgqexp(t)
 struct Squal *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != qual)
		fprintf(stderr,"gqexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgqexp);
}

/************** guard ******************/

tree mkguard(PPggexp)
 tree PPggexp;
{
	register struct Sguard *pp =
		(struct Sguard *) malloc(sizeof(struct Sguard));
	pp -> tag = guard;
	pp -> Xggexp = PPggexp;
	return((tree)pp);
}

tree *Rggexp(t)
 struct Sguard *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != guard)
		fprintf(stderr,"ggexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggexp);
}

/************** lsection ******************/

tree mklsection(PPglsexp, PPglsop)
 tree PPglsexp;
 qid PPglsop;
{
	register struct Slsection *pp =
		(struct Slsection *) malloc(sizeof(struct Slsection));
	pp -> tag = lsection;
	pp -> Xglsexp = PPglsexp;
	pp -> Xglsop = PPglsop;
	return((tree)pp);
}

tree *Rglsexp(t)
 struct Slsection *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lsection)
		fprintf(stderr,"glsexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglsexp);
}

qid *Rglsop(t)
 struct Slsection *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lsection)
		fprintf(stderr,"glsop: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglsop);
}

/************** rsection ******************/

tree mkrsection(PPgrsop, PPgrsexp)
 qid PPgrsop;
 tree PPgrsexp;
{
	register struct Srsection *pp =
		(struct Srsection *) malloc(sizeof(struct Srsection));
	pp -> tag = rsection;
	pp -> Xgrsop = PPgrsop;
	pp -> Xgrsexp = PPgrsexp;
	return((tree)pp);
}

qid *Rgrsop(t)
 struct Srsection *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != rsection)
		fprintf(stderr,"grsop: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrsop);
}

tree *Rgrsexp(t)
 struct Srsection *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != rsection)
		fprintf(stderr,"grsexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrsexp);
}

/************** ccall ******************/

tree mkccall(PPgccid, PPgccinfo, PPgccargs)
 stringId PPgccid;
 stringId PPgccinfo;
 list PPgccargs;
{
	register struct Sccall *pp =
		(struct Sccall *) malloc(sizeof(struct Sccall));
	pp -> tag = ccall;
	pp -> Xgccid = PPgccid;
	pp -> Xgccinfo = PPgccinfo;
	pp -> Xgccargs = PPgccargs;
	return((tree)pp);
}

stringId *Rgccid(t)
 struct Sccall *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ccall)
		fprintf(stderr,"gccid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgccid);
}

stringId *Rgccinfo(t)
 struct Sccall *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ccall)
		fprintf(stderr,"gccinfo: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgccinfo);
}

list *Rgccargs(t)
 struct Sccall *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ccall)
		fprintf(stderr,"gccargs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgccargs);
}

/************** scc ******************/

tree mkscc(PPgsccid, PPgsccexp)
 hstring PPgsccid;
 tree PPgsccexp;
{
	register struct Sscc *pp =
		(struct Sscc *) malloc(sizeof(struct Sscc));
	pp -> tag = scc;
	pp -> Xgsccid = PPgsccid;
	pp -> Xgsccexp = PPgsccexp;
	return((tree)pp);
}

hstring *Rgsccid(t)
 struct Sscc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != scc)
		fprintf(stderr,"gsccid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgsccid);
}

tree *Rgsccexp(t)
 struct Sscc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != scc)
		fprintf(stderr,"gsccexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgsccexp);
}
