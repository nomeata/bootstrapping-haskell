/* -----------------------------------------------------------------------------
 * $Id: StgMacros.h,v 1.25.2.1 2000/08/07 16:00:58 rrt Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Macros used for writing STG-ish C code.
 *
 * ---------------------------------------------------------------------------*/

#ifndef STGMACROS_H
#define STGMACROS_H

/* -----------------------------------------------------------------------------
  The following macros create function headers.

  Each basic block is represented by a C function with no arguments.
  We therefore always begin with either

  extern F_ f(void)

  or
  
  static F_ f(void)

  The macros can be used either to define the function itself, or to provide
  prototypes (by following with a ';').

  Note: the various I*_ shorthands in the second block below are used to
  declare forward references to local symbols. These shorthands *have* to
  use the 'extern' type specifier and not 'static'. The reason for this is
  that 'static' declares a reference as being a static/local variable,
  and *not* as a forward reference to a static variable.

  This might seem obvious, but it had me stumped as to why my info tables
  were suddenly all filled with 0s.

    -- sof 1/99 

  --------------------------------------------------------------------------- */

#define STGFUN(f)       StgFunPtr f(void)
#define EXTFUN(f)	extern StgFunPtr f(void)
#define EXTFUN_RTS(f)	extern DLL_IMPORT_RTS StgFunPtr f(void)
#define FN_(f)		F_ f(void)
#define IFN_(f)		static F_ f(void)
#define IF_(f)		static F_ f(void)
#define EF_(f)		extern F_ f(void)
#define EDF_(f)		extern DLLIMPORT F_ f(void)

#define EXTINFO_RTS	extern DLL_IMPORT_RTS INFO_TBL_CONST StgInfoTable
#define ED_		extern
#define EDD_		extern DLLIMPORT
#define ED_RO_		extern const
#define ID_		static
#define ID_RO_		static const
#define EI_             extern INFO_TBL_CONST StgInfoTable
#define EDI_            extern DLLIMPORT INFO_TBL_CONST StgInfoTable
#define II_             static INFO_TBL_CONST StgInfoTable
#define EC_		extern StgClosure
#define EDC_		extern DLLIMPORT StgClosure
#define IC_		static StgClosure
#define ECP_(x)		extern const StgClosure *(x)[]
#define EDCP_(x)	extern DLLIMPORT StgClosure *(x)[]
#define ICP_(x)		static const StgClosure *(x)[]

/* -----------------------------------------------------------------------------
   Stack Tagging.

   For a  block of non-pointer words on the stack, we precede the
   block with a small-integer tag giving the number of non-pointer
   words in the block.
   -------------------------------------------------------------------------- */

#define ARGTAG_MAX 16		/* probably arbitrary */
#define ARG_TAG(n)  (n)
#define ARG_SIZE(n) (StgWord)n

typedef enum {
    REALWORLD_TAG = 0,
    INT_TAG       = sizeofW(StgInt), 
    INT64_TAG     = sizeofW(StgInt64), 
    WORD_TAG      = sizeofW(StgWord), 
    ADDR_TAG      = sizeofW(StgAddr), 
    CHAR_TAG      = sizeofW(StgChar),
    FLOAT_TAG     = sizeofW(StgFloat), 
    DOUBLE_TAG    = sizeofW(StgDouble), 
    STABLE_TAG    = sizeofW(StgWord), 
} StackTag;

static inline int IS_ARG_TAG( StgWord p );
static inline int IS_ARG_TAG( StgWord p ) { return p <= ARGTAG_MAX; }

/* -----------------------------------------------------------------------------
   Argument checks.
   
   If (Sp + <n_args>) > Su { JMP_(stg_update_PAP); }
   
   Sp points to the topmost used word on the stack, and Su points to
   the most recently pushed update frame.

   Remember that <n_args> must include any tagging of unboxed values.

   ARGS_CHK_LOAD_NODE is for top-level functions, whose entry
   convention doesn't require that Node is loaded with a pointer to
   the closure.  Thus we must load node before calling stg_updatePAP if
   the argument check fails. 
   -------------------------------------------------------------------------- */

#define ARGS_CHK(n) 				\
        if ((P_)(Sp + (n)) > (P_)Su) {		\
		JMP_(stg_update_PAP);		\
	}

#define ARGS_CHK_LOAD_NODE(n,closure)		\
        if ((P_)(Sp + (n)) > (P_)Su) {		\
		R1.p = (P_)closure;             \
		JMP_(stg_update_PAP);		\
	}

/* -----------------------------------------------------------------------------
   Heap/Stack Checks.

   When failing a check, we save a return address on the stack and
   jump to a pre-compiled code fragment that saves the live registers
   and returns to the scheduler.

   The return address in most cases will be the beginning of the basic
   block in which the check resides, since we need to perform the check
   again on re-entry because someone else might have stolen the resource
   in the meantime.
   ------------------------------------------------------------------------- */

#define STK_CHK(headroom,ret,r,layout,tag_assts)		\
	if (Sp - headroom < SpLim) {				\
	    EXTFUN_RTS(stg_chk_##layout);		 	\
	    tag_assts						\
	    (r) = (P_)ret;					\
	    JMP_(stg_chk_##layout);				\
	}
       
#define HP_CHK(headroom,ret,r,layout,tag_assts)			\
        DO_GRAN_ALLOCATE(headroom)                              \
	if ((Hp += headroom) > HpLim) {				\
	    EXTFUN_RTS(stg_chk_##layout);		 	\
	    tag_assts						\
	    (r) = (P_)ret;                                     	\
	    JMP_(stg_chk_##layout);			   	\
	}

#define HP_STK_CHK(stk_headroom,hp_headroom,ret,r,layout,tag_assts) \
        DO_GRAN_ALLOCATE(hp_headroom)                              \
	if (Sp - stk_headroom < SpLim || (Hp += hp_headroom) > HpLim) {	\
	    EXTFUN_RTS(stg_chk_##layout);		 	\
	    tag_assts						\
	    (r) = (P_)ret;	                               	\
	    JMP_(stg_chk_##layout);			   	\
	}

/* -----------------------------------------------------------------------------
   A Heap Check in a case alternative are much simpler: everything is
   on the stack and covered by a liveness mask already, and there is
   even a return address with an SRT info table there as well.  

   Just push R1 and return to the scheduler saying 'EnterGHC'

   {STK,HP,HP_STK}_CHK_NP are the various checking macros for
   bog-standard case alternatives, thunks, and non-top-level
   functions.  In all these cases, node points to a closure that we
   can just enter to restart the heap check (the NP stands for 'node points').

   In the NP case GranSim absolutely has to check whether the current node 
   resides on the current processor. Otherwise a FETCH event has to be
   scheduled. All that is done in GranSimFetch. -- HWL

   HpLim points to the LAST WORD of valid allocation space.
   -------------------------------------------------------------------------- */

#define STK_CHK_NP(headroom,ptrs,tag_assts)			\
	if ((Sp - (headroom)) < SpLim) {			\
	    EXTFUN_RTS(stg_gc_enter_##ptrs);			\
            tag_assts						\
	    JMP_(stg_gc_enter_##ptrs);				\
	}

#define HP_CHK_NP(headroom,ptrs,tag_assts)			\
        DO_GRAN_ALLOCATE(headroom)                              \
	if ((Hp += (headroom)) > HpLim) {			\
	    EXTFUN_RTS(stg_gc_enter_##ptrs);			\
            tag_assts						\
	    JMP_(stg_gc_enter_##ptrs);				\
	}

#define HP_CHK_SEQ_NP(headroom,ptrs,tag_assts)			\
        DO_GRAN_ALLOCATE(headroom)                              \
	if ((Hp += (headroom)) > HpLim) {			\
	    EXTFUN_RTS(stg_gc_seq_##ptrs);			\
            tag_assts						\
	    JMP_(stg_gc_seq_##ptrs);				\
	}

#define HP_STK_CHK_NP(stk_headroom, hp_headroom, ptrs, tag_assts) \
        DO_GRAN_ALLOCATE(hp_headroom)                              \
	if ((Sp - (stk_headroom)) < SpLim || (Hp += (hp_headroom)) > HpLim) { \
	    EXTFUN_RTS(stg_gc_enter_##ptrs);		 	\
            tag_assts						\
	    JMP_(stg_gc_enter_##ptrs);			   	\
	}


/* Heap checks for branches of a primitive case / unboxed tuple return */

#define GEN_HP_CHK_ALT(headroom,lbl,tag_assts)			\
        DO_GRAN_ALLOCATE(headroom)                              \
	if ((Hp += (headroom)) > HpLim) {			\
	    EXTFUN_RTS(lbl);					\
            tag_assts						\
	    JMP_(lbl);						\
	}

#define HP_CHK_NOREGS(headroom,tag_assts) \
    GEN_HP_CHK_ALT(headroom,stg_gc_noregs,tag_assts);
#define HP_CHK_UNPT_R1(headroom,tag_assts)  \
    GEN_HP_CHK_ALT(headroom,stg_gc_unpt_r1,tag_assts);
#define HP_CHK_UNBX_R1(headroom,tag_assts)  \
    GEN_HP_CHK_ALT(headroom,stg_gc_unbx_r1,tag_assts);
#define HP_CHK_F1(headroom,tag_assts)       \
    GEN_HP_CHK_ALT(headroom,stg_gc_f1,tag_assts);
#define HP_CHK_D1(headroom,tag_assts)       \
    GEN_HP_CHK_ALT(headroom,stg_gc_d1,tag_assts);

#define HP_CHK_L1(headroom,tag_assts)       \
    GEN_HP_CHK_ALT(headroom,stg_gc_d1,tag_assts);

#define HP_CHK_UT_ALT(headroom, ptrs, nptrs, r, ret, tag_assts) \
    GEN_HP_CHK_ALT(headroom, stg_gc_ut_##ptrs##_##nptrs, \
		     tag_assts r = (P_)ret;)

/* -----------------------------------------------------------------------------
   Generic Heap checks.

   These are slow, but have the advantage of being usable in a variety
   of situations.  

   The one restriction is that any relevant SRTs must already be pointed
   to from the stack.  The return address doesn't need to have an info
   table attached: hence it can be any old code pointer.

   The liveness mask is a logical 'XOR' of NO_PTRS and zero or more
   Rn_PTR constants defined below.  All registers will be saved, but
   the garbage collector needs to know which ones contain pointers.

   Good places to use a generic heap check: 

        - case alternatives (the return address with an SRT is already
	  on the stack).

	- primitives (no SRT required).

   The stack layout is like this:

          DblReg1-2
	  FltReg1-4
	  R1-8
	  return address
	  liveness mask
	  stg_gen_chk_info

   so the liveness mask depends on the size of an StgDouble (FltRegs
   and R<n> are guaranteed to be 1 word in size).

   -------------------------------------------------------------------------- */

/* VERY MAGIC CONSTANTS! 
 * must agree with code in HeapStackCheck.c, stg_gen_chk
 */

#if SIZEOF_DOUBLE > SIZEOF_VOID_P
#define ALL_NON_PTRS   0xffff
#else /* SIZEOF_DOUBLE == SIZEOF_VOID_P */
#define ALL_NON_PTRS   0x3fff
#endif

#define LIVENESS_MASK(ptr_regs)  (ALL_NON_PTRS ^ (ptr_regs))

#define NO_PTRS   0
#define R1_PTR	  1<<0
#define R2_PTR	  1<<1
#define R3_PTR	  1<<2
#define R4_PTR	  1<<3
#define R5_PTR	  1<<4
#define R6_PTR	  1<<5
#define R7_PTR	  1<<6
#define R8_PTR	  1<<7

#define HP_CHK_GEN(headroom,liveness,reentry,tag_assts)	\
   if ((Hp += (headroom)) > HpLim ) {			\
	EXTFUN_RTS(stg_gen_chk);			\
        tag_assts					\
	R9.w = (W_)LIVENESS_MASK(liveness);		\
        R10.w = (W_)reentry;				\
        JMP_(stg_gen_chk);				\
   }

#define HP_CHK_GEN_TICKY(headroom,liveness,reentry,tag_assts)	\
   HP_CHK_GEN(headroom,liveness,reentry,tag_assts);		\
   TICK_ALLOC_HEAP_NOCTR(headroom)

#define STK_CHK_GEN(headroom,liveness,reentry,tag_assts)	\
   if ((Sp - (headroom)) < SpLim) {				\
	EXTFUN_RTS(stg_gen_chk);				\
        tag_assts						\
	R9.w = (W_)LIVENESS_MASK(liveness);			\
        R10.w = (W_)reentry;					\
        JMP_(stg_gen_chk);					\
   }

#define MAYBE_GC(liveness,reentry)		\
   if (doYouWantToGC()) {			\
	EXTFUN_RTS(stg_gen_hp);			\
	R9.w = (W_)LIVENESS_MASK(liveness);	\
        R10.w = (W_)reentry;			\
        JMP_(stg_gen_hp);			\
   }

/* -----------------------------------------------------------------------------
   Voluntary Yields/Blocks

   We only have a generic version of this at the moment - if it turns
   out to be slowing us down we can make specialised ones.
   -------------------------------------------------------------------------- */

EXTFUN_RTS(stg_gen_yield);
EXTFUN_RTS(stg_gen_block);

#define YIELD(liveness,reentry)			\
  {						\
   R9.w  = (W_)LIVENESS_MASK(liveness);		\
   R10.w = (W_)reentry;				\
   JMP_(stg_gen_yield);				\
  }

#define BLOCK(liveness,reentry)			\
  {						\
   R9.w  = (W_)LIVENESS_MASK(liveness);		\
   R10.w = (W_)reentry;				\
   JMP_(stg_gen_block);				\
  }

#define BLOCK_NP(ptrs)				\
  {						\
    EXTFUN_RTS(stg_block_##ptrs);		\
    JMP_(stg_block_##ptrs);			\
  }

#if defined(PAR)
/*
  Similar to BLOCK_NP but separates the saving of the thread state from the
  actual jump via an StgReturn
*/

#define SAVE_THREAD_STATE(ptrs)                  \
  ASSERT(ptrs==1);                               \
  Sp -= 1;                                       \
  Sp[0] = R1.w;                                  \
  SaveThreadState();                             

#define THREAD_RETURN(ptrs)                      \
  ASSERT(ptrs==1);                               \
  CurrentTSO->what_next = ThreadEnterGHC;        \
  R1.i = ThreadBlocked;                          \
  JMP_(StgReturn);                               
#endif

/* -----------------------------------------------------------------------------
   CCall_GC needs to push a dummy stack frame containing the contents
   of volatile registers and variables.  

   We use a RET_DYN frame the same as for a dynamic heap check.
   ------------------------------------------------------------------------- */

EXTINFO_RTS(stg_gen_chk_info);

/* -----------------------------------------------------------------------------
   Vectored Returns

   RETVEC(p,t) where 'p' is a pointer to the info table for a
   vectored return address, returns the address of the return code for
   tag 't'.

   Return vectors are placed in *reverse order* immediately before the info
   table for the return address.  Hence the formula for computing the
   actual return address is (addr - sizeof(InfoTable) - tag - 1).
   The extra subtraction of one word is because tags start at zero.
   -------------------------------------------------------------------------- */

#ifdef TABLES_NEXT_TO_CODE
#define RET_VEC(p,t) (*((P_)(p) - sizeofW(StgInfoTable) - t - 1))
#else
#define RET_VEC(p,t) (((StgInfoTable *)p)->vector[t])
#endif

/* -----------------------------------------------------------------------------
   Misc
   -------------------------------------------------------------------------- */


/* set the tag register (if we have one) */
#define SET_TAG(t)  /* nothing */

#ifdef EAGER_BLACKHOLING
#  ifdef SMP
#    define UPD_BH_UPDATABLE(info)				\
        TICK_UPD_BH_UPDATABLE();				\
        { 							\
	  bdescr *bd = Bdescr(R1.p);				\
          if (bd->back != (bdescr *)BaseReg) {			\
             if (bd->gen->no >= 1 || bd->step->no >= 1) {	\
        	 LOCK_THUNK(info);				\
             } else {						\
	         EXTFUN_RTS(stg_gc_enter_1_hponly);		\
        	 JMP_(stg_gc_enter_1_hponly);			\
             }							\
          }							\
	}							\
        SET_INFO(R1.cl,&BLACKHOLE_info)
#    define UPD_BH_SINGLE_ENTRY(info)				\
        TICK_UPD_BH_SINGLE_ENTRY();				\
        {							\
	  bdescr *bd = Bdescr(R1.p);				\
          if (bd->back != (bdescr *)BaseReg) {			\
             if (bd->gen->no >= 1 || bd->step->no >= 1) {	\
        	 LOCK_THUNK(info);				\
             } else {						\
	         EXTFUN_RTS(stg_gc_enter_1_hponly);		\
        	 JMP_(stg_gc_enter_1_hponly);			\
             }							\
          }							\
	}							\
        SET_INFO(R1.cl,&BLACKHOLE_info)
#  else
#    define UPD_BH_UPDATABLE(info)		\
        TICK_UPD_BH_UPDATABLE();		\
        SET_INFO(R1.cl,&BLACKHOLE_info)
#    define UPD_BH_SINGLE_ENTRY(info)		\
        TICK_UPD_BH_SINGLE_ENTRY();		\
        SET_INFO(R1.cl,&SE_BLACKHOLE_info)
#  endif
#else /* !EAGER_BLACKHOLING */
#  define UPD_BH_UPDATABLE(thunk)    /* nothing */
#  define UPD_BH_SINGLE_ENTRY(thunk) /* nothing */
#endif /* EAGER_BLACKHOLING */

#define UPD_FRAME_UPDATEE(p)  (((StgUpdateFrame *)(p))->updatee)
#define UPDATE_SU_FROM_UPD_FRAME(p) (Su=((StgUpdateFrame *)(p))->link)

/* -----------------------------------------------------------------------------
   Moving Floats and Doubles

   ASSIGN_FLT is for assigning a float to memory (usually the
              stack/heap).  The memory address is guaranteed to be
	      StgWord aligned (currently == sizeof(long)).

   PK_FLT     is for pulling a float out of memory.  The memory is
              guaranteed to be StgWord aligned.
   -------------------------------------------------------------------------- */

static inline void	  ASSIGN_FLT (W_ [], StgFloat);
static inline StgFloat    PK_FLT     (W_ []);

#if ALIGNMENT_FLOAT <= ALIGNMENT_LONG

static inline void     ASSIGN_FLT(W_ p_dest[], StgFloat src) { *(StgFloat *)p_dest = src; }
static inline StgFloat PK_FLT    (W_ p_src[])                { return *(StgFloat *)p_src; }

#else  /* ALIGNMENT_FLOAT > ALIGNMENT_UNSIGNED_INT */

static inline void ASSIGN_FLT(W_ p_dest[], StgFloat src)
{
    float_thing y;
    y.f = src;
    *p_dest = y.fu;
}

static inline StgFloat PK_FLT(W_ p_src[])
{
    float_thing y;
    y.fu = *p_src;
    return(y.f);
}

#endif /* ALIGNMENT_FLOAT > ALIGNMENT_LONG */

#if ALIGNMENT_DOUBLE <= ALIGNMENT_LONG

static inline void	  ASSIGN_DBL (W_ [], StgDouble);
static inline StgDouble   PK_DBL     (W_ []);

static inline void      ASSIGN_DBL(W_ p_dest[], StgDouble src) { *(StgDouble *)p_dest = src; }
static inline StgDouble PK_DBL    (W_ p_src[])                 { return *(StgDouble *)p_src; }

#else	/* ALIGNMENT_DOUBLE > ALIGNMENT_LONG */

/* Sparc uses two floating point registers to hold a double.  We can
 * write ASSIGN_DBL and PK_DBL by directly accessing the registers
 * independently - unfortunately this code isn't writable in C, we
 * have to use inline assembler.
 */
#if sparc_TARGET_ARCH

#define ASSIGN_DBL(dst0,src) \
    { StgPtr dst = (StgPtr)(dst0); \
      __asm__("st %2,%0\n\tst %R2,%1" : "=m" (((P_)(dst))[0]), \
	"=m" (((P_)(dst))[1]) : "f" (src)); \
    }

#define PK_DBL(src0) \
    ( { StgPtr src = (StgPtr)(src0); \
        register double d; \
      __asm__("ld %1,%0\n\tld %2,%R0" : "=f" (d) : \
	"m" (((P_)(src))[0]), "m" (((P_)(src))[1])); d; \
    } )

#else /* ! sparc_TARGET_ARCH */

static inline void	  ASSIGN_DBL (W_ [], StgDouble);
static inline StgDouble   PK_DBL     (W_ []);

typedef struct
  { StgWord dhi;
    StgWord dlo;
  } unpacked_double;

typedef union
  { StgDouble d;
    unpacked_double du;
  } double_thing;

static inline void ASSIGN_DBL(W_ p_dest[], StgDouble src)
{
    double_thing y;
    y.d = src;
    p_dest[0] = y.du.dhi;
    p_dest[1] = y.du.dlo;
}

/* GCC also works with this version, but it generates
   the same code as the previous one, and is not ANSI

#define ASSIGN_DBL( p_dest, src ) \
	*p_dest = ((double_thing) src).du.dhi; \
	*(p_dest+1) = ((double_thing) src).du.dlo \
*/

static inline StgDouble PK_DBL(W_ p_src[])
{
    double_thing y;
    y.du.dhi = p_src[0];
    y.du.dlo = p_src[1];
    return(y.d);
}

#endif /* ! sparc_TARGET_ARCH */

#endif /* ALIGNMENT_DOUBLE > ALIGNMENT_UNSIGNED_INT */

#ifdef SUPPORT_LONG_LONGS

typedef struct
  { StgWord dhi;
    StgWord dlo;
  } unpacked_double_word;

typedef union
  { StgInt64 i;
    unpacked_double_word iu;
  } int64_thing;

typedef union
  { StgWord64 w;
    unpacked_double_word wu;
  } word64_thing;

static inline void ASSIGN_Word64(W_ p_dest[], StgWord64 src)
{
    word64_thing y;
    y.w = src;
    p_dest[0] = y.wu.dhi;
    p_dest[1] = y.wu.dlo;
}

static inline StgWord64 PK_Word64(W_ p_src[])
{
    word64_thing y;
    y.wu.dhi = p_src[0];
    y.wu.dlo = p_src[1];
    return(y.w);
}

static inline void ASSIGN_Int64(W_ p_dest[], StgInt64 src)
{
    int64_thing y;
    y.i = src;
    p_dest[0] = y.iu.dhi;
    p_dest[1] = y.iu.dlo;
}

static inline StgInt64 PK_Int64(W_ p_src[])
{
    int64_thing y;
    y.iu.dhi = p_src[0];
    y.iu.dlo = p_src[1];
    return(y.i);
}
#endif

/* -----------------------------------------------------------------------------
   Catch frames
   -------------------------------------------------------------------------- */

extern DLL_IMPORT_DATA const StgPolyInfoTable catch_frame_info;

/* -----------------------------------------------------------------------------
   Seq frames

   A seq frame is very like an update frame, except that it doesn't do
   an update...
   -------------------------------------------------------------------------- */

extern DLL_IMPORT_DATA const StgPolyInfoTable seq_frame_info;

#define PUSH_SEQ_FRAME(sp)					\
	{							\
		StgSeqFrame *__frame;				\
		TICK_SEQF_PUSHED();  				\
		__frame = (StgSeqFrame *)(sp); 			\
		SET_HDR_(__frame,&seq_frame_info,CCCS);        	\
		__frame->link = Su;				\
		Su = (StgUpdateFrame *)__frame;			\
	}

/* -----------------------------------------------------------------------------
   Split markers
   -------------------------------------------------------------------------- */

#if defined(USE_SPLIT_MARKERS)
#define __STG_SPLIT_MARKER(n) FN_(__stg_split_marker##n) { }
#else
#define __STG_SPLIT_MARKER(n) /* nothing */
#endif

/* -----------------------------------------------------------------------------
   Closure and Info Macros with casting.

   We don't want to mess around with casts in the generated C code, so
   we use these casting versions of the closure/info tables macros.
   -------------------------------------------------------------------------- */

#define SET_HDR_(c,info,ccs) \
   SET_HDR((StgClosure *)(c),(StgInfoTable *)(info),ccs)

/* -----------------------------------------------------------------------------
   Saving context for exit from the STG world, and loading up context
   on entry to STG code.

   We save all the STG registers (that is, the ones that are mapped to
   machine registers) in their places in the TSO.  

   The stack registers go into the current stack object, and the
   current nursery is updated from the heap pointer.

   These functions assume that BaseReg is loaded appropriately (if
   we have one).
   -------------------------------------------------------------------------- */

#if IN_STG_CODE

static __inline__ void
SaveThreadState(void)
{
  StgTSO *tso;

  /* Don't need to save REG_Base, it won't have changed. */

  tso = CurrentTSO;
  tso->sp       = Sp;
  tso->su       = Su;
  tso->splim    = SpLim;
  CloseNursery(Hp);

#ifdef REG_CurrentTSO
  SAVE_CurrentTSO = tso;
#endif
#ifdef REG_CurrentNursery
  SAVE_CurrentNursery = CurrentNursery;
#endif
#if defined(PROFILING)
  CurrentTSO->prof.CCCS = CCCS;
#endif
}

static __inline__ void 
LoadThreadState (void)
{
  StgTSO *tso;

#ifdef REG_CurrentTSO
  CurrentTSO = SAVE_CurrentTSO;
#endif

  tso = CurrentTSO;
  Sp    = tso->sp;
  Su    = tso->su;
  SpLim = tso->splim;
  OpenNursery(Hp,HpLim);

#ifdef REG_CurrentNursery
  CurrentNursery = SAVE_CurrentNursery;
#endif
# if defined(PROFILING)
  CCCS = CurrentTSO->prof.CCCS;
# endif
}

#endif

/* -----------------------------------------------------------------------------
   Module initialisation
   -------------------------------------------------------------------------- */

#define PUSH_INIT_STACK(reg_function)		\
	*(Sp++) = (W_)reg_function

#define POP_INIT_STACK()			\
	*(--Sp)

#define START_MOD_INIT(reg_mod_name)		\
	static int _module_registered = 0;	\
	FN_(reg_mod_name) {			\
	    FB_;				\
	    if (! _module_registered) {		\
	        _module_registered = 1;		\
		{ 
	    /* extern decls go here, followed by init code */

#define REGISTER_FOREIGN_EXPORT(reg_fe_binder)	\
        STGCALL1(getStablePtr,reg_fe_binder)
	
#define REGISTER_IMPORT(reg_mod_name)		\
	PUSH_INIT_STACK(reg_mod_name)		\
	
#define END_MOD_INIT()				\
        }};					\
	JMP_(POP_INIT_STACK());			\
	FE_ }

/* -----------------------------------------------------------------------------
   Support for _ccall_GC_ and _casm_GC.
   -------------------------------------------------------------------------- */

/* 
 * Suspending/resuming threads for doing external C-calls (_ccall_GC).
 * These functions are defined in rts/Schedule.c.
 */
StgInt        suspendThread ( StgRegTable *cap );
StgRegTable * resumeThread  ( StgInt );

#define SUSPEND_THREAD(token)			\
   SaveThreadState();				\
   token = suspendThread(BaseReg);

#ifdef SMP
#define RESUME_THREAD(token)  			\
   BaseReg = resumeThread(token);		\
   LoadThreadState();
#else
#define RESUME_THREAD(token)			\
   (void)resumeThread(token);			\
   LoadThreadState();
#endif

#endif /* STGMACROS_H */

