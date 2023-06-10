typedef union {
	tree utree;
	list ulist;
	ttype uttype;
	constr uconstr;
	binding ubinding;
	pbinding upbinding;
	entidt uentid;
	id uid;
	qid uqid;
	literal uliteral;
        maybe umaybe;
        either ueither;
	long ulong;
	float ufloat;
	char *ustring;
	hstring uhstring;
} YYSTYPE;
#define	VARID	258
#define	CONID	259
#define	QVARID	260
#define	QCONID	261
#define	VARSYM	262
#define	CONSYM	263
#define	QVARSYM	264
#define	QCONSYM	265
#define	INTEGER	266
#define	FLOAT	267
#define	CHAR	268
#define	STRING	269
#define	CHARPRIM	270
#define	STRINGPRIM	271
#define	INTPRIM	272
#define	FLOATPRIM	273
#define	DOUBLEPRIM	274
#define	CLITLIT	275
#define	OCURLY	276
#define	CCURLY	277
#define	VCCURLY	278
#define	COMMA	279
#define	SEMI	280
#define	OBRACK	281
#define	CBRACK	282
#define	WILDCARD	283
#define	BQUOTE	284
#define	OPAREN	285
#define	CPAREN	286
#define	DOTDOT	287
#define	DCOLON	288
#define	EQUAL	289
#define	LAMBDA	290
#define	VBAR	291
#define	RARROW	292
#define	LARROW	293
#define	AT	294
#define	LAZY	295
#define	DARROW	296
#define	CASE	297
#define	CLASS	298
#define	DATA	299
#define	DEFAULT	300
#define	DERIVING	301
#define	DO	302
#define	ELSE	303
#define	IF	304
#define	IMPORT	305
#define	IN	306
#define	INFIX	307
#define	INFIXL	308
#define	INFIXR	309
#define	INSTANCE	310
#define	LET	311
#define	MODULE	312
#define	NEWTYPE	313
#define	OF	314
#define	THEN	315
#define	TYPE	316
#define	WHERE	317
#define	SCC	318
#define	CCALL	319
#define	CCALL_GC	320
#define	CASM	321
#define	CASM_GC	322
#define	MINUS	323
#define	BANG	324
#define	PLUS	325
#define	AS	326
#define	HIDING	327
#define	QUALIFIED	328
#define	INTERFACE_UPRAGMA	329
#define	SPECIALISE_UPRAGMA	330
#define	INLINE_UPRAGMA	331
#define	NOINLINE_UPRAGMA	332
#define	MAGIC_UNFOLDING_UPRAGMA	333
#define	END_UPRAGMA	334
#define	SOURCE_UPRAGMA	335


extern YYSTYPE yylval;
