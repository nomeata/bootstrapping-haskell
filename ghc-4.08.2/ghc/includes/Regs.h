/* -----------------------------------------------------------------------------
 * $Id: Regs.h,v 1.9 2000/03/23 17:45:31 simonpj Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Registers used in STG code.  Might or might not correspond to
 * actual machine registers.
 *
 * ---------------------------------------------------------------------------*/

#ifndef REGS_H
#define REGS_H

/*
 * This file should do the right thing if we have no machine-registers
 * defined, i.e. everything lives in the RegTable.
 */

/* 
 * This is the table that holds shadow-locations for all the STG
 * registers.  The shadow locations are used when:
 *
 *     1) the particular register isn't mapped to a real machine
 *        register, probably because there's a shortage of real registers.
 *     2) caller-saves registers are saved across a CCall
 */

typedef struct StgSparkPool_ {
  StgClosure **base;
  StgClosure **lim;
  StgClosure **hd;
  StgClosure **tl;
} StgSparkPool;

typedef struct StgRegTable_ {
  StgUnion 	  rR1;
  StgUnion   	  rR2;
  StgUnion   	  rR3;
  StgUnion   	  rR4;
  StgUnion   	  rR5;
  StgUnion   	  rR6;
  StgUnion   	  rR7;
  StgUnion   	  rR8;
  StgUnion   	  rR9;		/* used occasionally by heap/stack checks */
  StgUnion   	  rR10;		/* used occasionally by heap/stack checks */
  StgFloat 	  rF1;
  StgFloat 	  rF2;
  StgFloat 	  rF3;
  StgFloat 	  rF4;
  StgDouble 	  rD1;
  StgDouble 	  rD2;
  StgWord64       rL1;
  StgPtr 	  rSp;
  StgUpdateFrame *rSu;
  StgPtr 	  rSpLim;
  StgPtr 	  rHp;
  StgPtr 	  rHpLim;
  StgTSO         *rCurrentTSO;
  struct _bdescr *rNursery;
  struct _bdescr *rCurrentNursery;
#if defined(SMP) || defined(PAR)
  StgSparkPool   rSparks;	/* per-task spark pool */
#endif
#if defined(SMP)
  struct StgRegTable_ *link;	/* per-task register tables are linked together */
#endif
} StgRegTable;

/* No such thing as a MainRegTable under SMP - each thread must
 * have its own MainRegTable.
 */
#ifndef SMP
extern DLL_IMPORT_RTS StgRegTable  MainRegTable;
#endif

#if IN_STG_CODE

/*
 * Registers Hp and HpLim are global across the entire system, and are
 * copied into the RegTable before executing a thread.
 *
 * Registers Sp, Su, and SpLim are saved in the TSO for the
 * thread, but are copied into the RegTable before executing a thread.
 *
 * All other registers are "general purpose", and are used for passing
 * arguments to functions, and returning values.  The code generator
 * knows how many of these are in real registers, and avoids
 * generating code that uses non-real registers.  General purpose
 * registers are never saved when returning to the scheduler, instead
 * we save whatever is live at the time on the stack, and restore it
 * later.  This should reduce the context switch time, amongst other
 * things.
 *
 * For argument passing, the stack will be used in preference to
 * pseudo-registers if the architecture has too few general purpose
 * registers.
 *
 * Some special RTS functions like newArray and the Integer primitives
 * expect their arguments to be in registers R1-Rn, so we use these
 * (pseudo-)registers in those cases.
 */

/* 
 * Locations for saving per-thread registers.
 */

#define SAVE_Sp    	    (CurrentTSO->sp)
#define SAVE_Su    	    (CurrentTSO->su)
#define SAVE_SpLim    	    (CurrentTSO->splim)

#define SAVE_Hp	    	    (BaseReg->rHp)
#define SAVE_HpLim	    (BaseReg->rHpLim)

#define SAVE_CurrentTSO     (BaseReg->rCurrentTSO)
#define SAVE_CurrentNursery (BaseReg->rCurrentNursery)
#if defined(SMP) || defined(PAR)
#define SAVE_SparkHd 	    (BaseReg->rSparks.hd)
#define SAVE_SparkTl        (BaseReg->rSparks.tl)
#define SAVE_SparkBase      (BaseReg->rSparks.base)
#define SAVE_SparkLim 	    (BaseReg->rSparks.lim)
#endif

/* We sometimes need to save registers across a C-call, eg. if they
 * are clobbered in the standard calling convention.  We define the
 * save locations for all registers in the register table.
 */

#define SAVE_R1             (BaseReg->rR1)
#define SAVE_R2             (BaseReg->rR2)
#define SAVE_R3             (BaseReg->rR3)
#define SAVE_R4             (BaseReg->rR4)
#define SAVE_R5             (BaseReg->rR5)
#define SAVE_R6             (BaseReg->rR6)
#define SAVE_R7             (BaseReg->rR7)
#define SAVE_R8             (BaseReg->rR8)
 
#define SAVE_F1             (BaseReg->rF1)
#define SAVE_F2             (BaseReg->rF2)
#define SAVE_F3             (BaseReg->rF3)
#define SAVE_F4             (BaseReg->rF4)

#define SAVE_D1             (BaseReg->rD1)
#define SAVE_D2             (BaseReg->rD2)

#define SAVE_L1             (BaseReg->rL1)

/* -----------------------------------------------------------------------------
 * Emit the GCC-specific register declarations for each machine
 * register being used.  If any STG register isn't mapped to a machine
 * register, then map it to an offset from BaseReg.
 *
 * First, the general purpose registers.  The idea is, if a particular
 * general-purpose STG register can't be mapped to a real machine
 * register, it won't be used at all.  Instead, we'll use the stack.
 *
 * This is an improvement on the way things used to be done, when all
 * registers were mapped to locations in the register table, and stuff
 * was being shifted from the stack to the register table and back
 * again for no good reason (on register-poor architectures).
 */

#define GLOBAL_REG_DECL(type,name,reg) register type name REG(reg);

#ifdef REG_R1
GLOBAL_REG_DECL(StgUnion,R1,REG_R1)
#else
#define R1 (BaseReg->rR1)
#endif

#ifdef REG_R2
GLOBAL_REG_DECL(StgUnion,R2,REG_R2)
#else
#define R2 (BaseReg->rR2)
#endif

#ifdef REG_R3
GLOBAL_REG_DECL(StgUnion,R3,REG_R3)
#else
# define R3 (BaseReg->rR3)
#endif

#ifdef REG_R4
GLOBAL_REG_DECL(StgUnion,R4,REG_R4)
#else
# define R4 (BaseReg->rR4)
#endif

#ifdef REG_R5
GLOBAL_REG_DECL(StgUnion,R5,REG_R5)
#else
# define R5 (BaseReg->rR5)
#endif

#ifdef REG_R6
GLOBAL_REG_DECL(StgUnion,R6,REG_R6)
#else
# define R6 (BaseReg->rR6)
#endif

#ifdef REG_R7
GLOBAL_REG_DECL(StgUnion,R7,REG_R7)
#else
# define R7 (BaseReg->rR7)
#endif

#ifdef REG_R8
GLOBAL_REG_DECL(StgUnion,R8,REG_R8)
#else
# define R8 (BaseReg->rR8)
#endif

#ifdef REG_R9
GLOBAL_REG_DECL(StgUnion,R9,REG_R9)
#else
# define R9 (BaseReg->rR9)
#endif

#ifdef REG_R10
GLOBAL_REG_DECL(StgUnion,R10,REG_R10)
#else
# define R10 (BaseReg->rR10)
#endif

#ifdef REG_F1
GLOBAL_REG_DECL(StgFloat,F1,REG_F1)
#else
#define F1 (BaseReg->rF1)
#endif

#ifdef REG_F2
GLOBAL_REG_DECL(StgFloat,F2,REG_F2)
#else
#define F2 (BaseReg->rF2)
#endif

#ifdef REG_F3
GLOBAL_REG_DECL(StgFloat,F3,REG_F3)
#else
#define F3 (BaseReg->rF3)
#endif

#ifdef REG_F4
GLOBAL_REG_DECL(StgFloat,F4,REG_F4)
#else
#define F4 (BaseReg->rF4)
#endif

#ifdef REG_D1
GLOBAL_REG_DECL(StgDouble,D1,REG_D1)
#else
#define D1 (BaseReg->rD1)
#endif

#ifdef REG_D2
GLOBAL_REG_DECL(StgDouble,D2,REG_D2)
#else
#define D2 (BaseReg->rD2)
#endif

#ifdef REG_L1
GLOBAL_REG_DECL(StgWord64,L1,REG_L1)
#else
#define L1 (BaseReg->rL1)
#endif

/*
 * If BaseReg isn't mapped to a machine register, just use the global
 * address of the current register table (CurrentRegTable in
 * concurrent Haskell, MainRegTable otherwise).
 */

#ifdef REG_Base
GLOBAL_REG_DECL(StgRegTable *,BaseReg,REG_Base)
#else
#ifdef SMP
#error BaseReg must be in a register for SMP
#endif
#define BaseReg (&MainRegTable)
#endif

#ifdef REG_Sp
GLOBAL_REG_DECL(P_,Sp,REG_Sp)
#else
#define Sp (BaseReg->rSp)
#endif

#ifdef REG_Su
GLOBAL_REG_DECL(StgUpdateFrame *,Su,REG_Su)
#else
#define Su (BaseReg->rSu)
#endif

#ifdef REG_SpLim
GLOBAL_REG_DECL(P_,SpLim,REG_SpLim)
#else
#define SpLim (BaseReg->rSpLim)
#endif

#ifdef REG_Hp
GLOBAL_REG_DECL(P_,Hp,REG_Hp)
#else
#define Hp (BaseReg->rHp)
#endif

#ifdef REG_HpLim
GLOBAL_REG_DECL(P_,HpLim,REG_HpLim)
#else
#define HpLim (BaseReg->rHpLim)
#endif

#ifdef REG_CurrentTSO
GLOBAL_REG_DECL(StgTSO *,CurrentTSO,REG_CurrentTSO)
#else
#define CurrentTSO (BaseReg->rCurrentTSO)
#endif

#ifdef REG_CurrentNursery
GLOBAL_REG_DECL(bdescr *,CurrentNursery,REG_CurrentNursery)
#else
#define CurrentNursery (BaseReg->rCurrentNursery)
#endif

#ifdef REG_SparkHd
GLOBAL_REG_DECL(bdescr *,SparkHd,REG_SparkHd)
#else
#define SparkHd (BaseReg->rSparks.hd)
#endif

#ifdef REG_SparkTl
GLOBAL_REG_DECL(bdescr *,SparkTl,REG_SparkTl)
#else
#define SparkTl (BaseReg->rSparks.tl)
#endif

#ifdef REG_SparkBase
GLOBAL_REG_DECL(bdescr *,SparkBase,REG_SparkBase)
#else
#define SparkBase (BaseReg->rSparks.base)
#endif

#ifdef REG_SparkLim
GLOBAL_REG_DECL(bdescr *,SparkLim,REG_SparkLim)
#else
#define SparkLim (BaseReg->rSparks.lim)
#endif

/* -----------------------------------------------------------------------------
   For any registers which are denoted "caller-saves" by the C calling
   convention, we have to emit code to save and restore them across C
   calls.
   -------------------------------------------------------------------------- */

#ifdef CALLER_SAVES_R1
#define CALLER_SAVE_R1    	SAVE_R1 = R1;
#define CALLER_RESTORE_R1 	R1 = SAVE_R1;
#else
#define CALLER_SAVE_R1      	/* nothing */
#define CALLER_RESTORE_R1    	/* nothing */
#endif

#ifdef CALLER_SAVES_R2
#define CALLER_SAVE_R2    	SAVE_R2 = R2;
#define CALLER_RESTORE_R2 	R2 = SAVE_R2;
#else
#define CALLER_SAVE_R2      	/* nothing */
#define CALLER_RESTORE_R2    	/* nothing */
#endif

#ifdef CALLER_SAVES_R3
#define CALLER_SAVE_R3    	SAVE_R3 = R3;
#define CALLER_RESTORE_R3 	R3 = SAVE_R3;
#else
#define CALLER_SAVE_R3      	/* nothing */
#define CALLER_RESTORE_R3    	/* nothing */
#endif

#ifdef CALLER_SAVES_R4
#define CALLER_SAVE_R4    	SAVE_R4 = R4;
#define CALLER_RESTORE_R4 	R4 = SAVE_R4;
#else
#define CALLER_SAVE_R4      	/* nothing */
#define CALLER_RESTORE_R4    	/* nothing */
#endif

#ifdef CALLER_SAVES_R5
#define CALLER_SAVE_R5    	SAVE_R5 = R5;
#define CALLER_RESTORE_R5 	R5 = SAVE_R5;
#else
#define CALLER_SAVE_R5      	/* nothing */
#define CALLER_RESTORE_R5    	/* nothing */
#endif

#ifdef CALLER_SAVES_R6
#define CALLER_SAVE_R6    	SAVE_R6 = R6;
#define CALLER_RESTORE_R6 	R6 = SAVE_R6;
#else
#define CALLER_SAVE_R6      	/* nothing */
#define CALLER_RESTORE_R6    	/* nothing */
#endif

#ifdef CALLER_SAVES_R7
#define CALLER_SAVE_R7    	SAVE_R7 = R7;
#define CALLER_RESTORE_R7 	R7 = SAVE_R7;
#else
#define CALLER_SAVE_R7      	/* nothing */
#define CALLER_RESTORE_R7    	/* nothing */
#endif

#ifdef CALLER_SAVES_R8
#define CALLER_SAVE_R8    	SAVE_R8 = R8;
#define CALLER_RESTORE_R8 	R8 = SAVE_R8;
#else
#define CALLER_SAVE_R8      	/* nothing */
#define CALLER_RESTORE_R8    	/* nothing */
#endif

#ifdef CALLER_SAVES_R9
#define CALLER_SAVE_R9    	SAVE_R9 = R9;
#define CALLER_RESTORE_R9 	R9 = SAVE_R9;
#else
#define CALLER_SAVE_R9      	/* nothing */
#define CALLER_RESTORE_R9    	/* nothing */
#endif

#ifdef CALLER_SAVES_R10
#define CALLER_SAVE_R10    	SAVE_R10 = R10;
#define CALLER_RESTORE_R10 	R10 = SAVE_R10;
#else
#define CALLER_SAVE_R10      	/* nothing */
#define CALLER_RESTORE_R10    	/* nothing */
#endif

#ifdef CALLER_SAVES_F1
#define CALLER_SAVE_F1    	SAVE_F1 = F1;
#define CALLER_RESTORE_F1 	F1 = SAVE_F1;
#else
#define CALLER_SAVE_F1    	/* nothing */
#define CALLER_RESTORE_F1 	/* nothing */
#endif

#ifdef CALLER_SAVES_F2
#define CALLER_SAVE_F2    	SAVE_F2 = F2;
#define CALLER_RESTORE_F2 	F2 = SAVE_F2;
#else
#define CALLER_SAVE_F2    	/* nothing */
#define CALLER_RESTORE_F2 	/* nothing */
#endif

#ifdef CALLER_SAVES_F3
#define CALLER_SAVE_F3    	SAVE_F3 = F3;
#define CALLER_RESTORE_F3 	F3 = SAVE_F3;
#else
#define CALLER_SAVE_F3    	/* nothing */
#define CALLER_RESTORE_F3 	/* nothing */
#endif

#ifdef CALLER_SAVES_F4
#define CALLER_SAVE_F4    	SAVE_F4 = F4;
#define CALLER_RESTORE_F4 	F4 = SAVE_F4;
#else
#define CALLER_SAVE_F4    	/* nothing */
#define CALLER_RESTORE_F4 	/* nothing */
#endif

#ifdef CALLER_SAVES_D1
#define CALLER_SAVE_D1    	SAVE_D1 = D1;
#define CALLER_RESTORE_D1 	D1 = SAVE_D1;
#else
#define CALLER_SAVE_D1    	/* nothing */
#define CALLER_RESTORE_D1 	/* nothing */
#endif

#ifdef CALLER_SAVES_D2
#define CALLER_SAVE_D2    	SAVE_D2 = D2;
#define CALLER_RESTORE_D2 	D2 = SAVE_D2;
#else
#define CALLER_SAVE_D2    	/* nothing */
#define CALLER_RESTORE_D2 	/* nothing */
#endif

#ifdef CALLER_SAVES_L1
#define CALLER_SAVE_L1    	SAVE_L1 = L1;
#define CALLER_RESTORE_L1 	L1 = SAVE_L1;
#else
#define CALLER_SAVE_L1    	/* nothing */
#define CALLER_RESTORE_L1 	/* nothing */
#endif

#ifdef CALLER_SAVES_Sp
#define CALLER_SAVE_Sp	    	SAVE_Sp = Sp;
#define CALLER_RESTORE_Sp  	Sp = SAVE_Sp;
#else
#define CALLER_SAVE_Sp	    	/* nothing */
#define CALLER_RESTORE_Sp	/* nothing */
#endif

#ifdef CALLER_SAVES_Su
#define CALLER_SAVE_Su	    	SAVE_Su = Su;
#define CALLER_RESTORE_Su  	Su = SAVE_Su;
#else
#define CALLER_SAVE_Su	    	/* nothing */
#define CALLER_RESTORE_Su	/* nothing */
#endif

#ifdef CALLER_SAVES_SpLim
#define CALLER_SAVE_SpLim    	SAVE_SpLim = SpLim;
#define CALLER_RESTORE_SpLim  	SpLim = SAVE_SpLim;
#else
#define CALLER_SAVE_SpLim    	/* nothing */
#define CALLER_RESTORE_SpLim	/* nothing */
#endif

#ifdef CALLER_SAVES_Hp
#define CALLER_SAVE_Hp	    	SAVE_Hp = Hp;
#define CALLER_RESTORE_Hp   	Hp = SAVE_Hp;
#else
#define CALLER_SAVE_Hp	    	/* nothing */
#define CALLER_RESTORE_Hp	/* nothing */
#endif

#ifdef CALLER_SAVES_HpLim
#define CALLER_SAVE_HpLim   	SAVE_HpLim = HpLim;
#define CALLER_RESTORE_HpLim	HpLim = SAVE_HpLim;
#else
#define CALLER_SAVE_HpLim   	/* nothing */
#define CALLER_RESTORE_HpLim   	/* nothing */
#endif

#ifdef CALLER_SAVES_Base
#ifdef SMP
#error "Can't have caller-saved BaseReg with SMP"
#endif
#define CALLER_SAVE_Base	/* nothing */
#define CALLER_RESTORE_Base	BaseReg = &MainRegTable;
#else
#define CALLER_SAVE_Base	/* nothing */
#define CALLER_RESTORE_Base	/* nothing */
#endif

#ifdef CALLER_SAVES_CurrentTSO
#define CALLER_SAVE_CurrentTSO   	SAVE_CurrentTSO = CurrentTSO;
#define CALLER_RESTORE_CurrentTSO	CurrentTSO = SAVE_CurrentTSO;
#else
#define CALLER_SAVE_CurrentTSO   	/* nothing */
#define CALLER_RESTORE_CurrentTSO   	/* nothing */
#endif

#ifdef CALLER_SAVES_CurrentNursery
#define CALLER_SAVE_CurrentNursery   	SAVE_CurrentNursery = CurrentNursery;
#define CALLER_RESTORE_CurrentNursery	CurrentNursery = SAVE_CurrentNursery;
#else
#define CALLER_SAVE_CurrentNursery   	/* nothing */
#define CALLER_RESTORE_CurrentNursery   /* nothing */
#endif

#ifdef CALLER_SAVES_SparkHd
#define CALLER_SAVE_SparkHd   		SAVE_SparkHd = SparkHd;
#define CALLER_RESTORE_SparkHd		SparkHd = SAVE_SparkHd;
#else
#define CALLER_SAVE_SparkHd   		/* nothing */
#define CALLER_RESTORE_SparkHd   	/* nothing */
#endif

#ifdef CALLER_SAVES_SparkTl
#define CALLER_SAVE_SparkTl   		SAVE_SparkTl = SparkTl;
#define CALLER_RESTORE_SparkTl		SparkTl = SAVE_SparkTl;
#else
#define CALLER_SAVE_SparkTl   		/* nothing */
#define CALLER_RESTORE_SparkTl   	/* nothing */
#endif

#ifdef CALLER_SAVES_SparkBase
#define CALLER_SAVE_SparkBase   	SAVE_SparkBase = SparkBase;
#define CALLER_RESTORE_SparkBase	SparkBase = SAVE_SparkBase;
#else
#define CALLER_SAVE_SparkBase   	/* nothing */
#define CALLER_RESTORE_SparkBase   	/* nothing */
#endif

#ifdef CALLER_SAVES_SparkLim
#define CALLER_SAVE_SparkLim   		SAVE_SparkLim = SparkLim;
#define CALLER_RESTORE_SparkLim		SparkLim = SAVE_SparkLim;
#else
#define CALLER_SAVE_SparkLim   		/* nothing */
#define CALLER_RESTORE_SparkLim   	/* nothing */
#endif

#endif /* IN_STG_CODE */

/* ----------------------------------------------------------------------------
   Handy bunches of saves/restores 
   ------------------------------------------------------------------------  */

#if IN_STG_CODE

#define CALLER_SAVE_USER			\
  CALLER_SAVE_R1				\
  CALLER_SAVE_R2				\
  CALLER_SAVE_R3				\
  CALLER_SAVE_R4				\
  CALLER_SAVE_R5				\
  CALLER_SAVE_R6				\
  CALLER_SAVE_R7				\
  CALLER_SAVE_R8				\
  CALLER_SAVE_F1				\
  CALLER_SAVE_F2				\
  CALLER_SAVE_F3				\
  CALLER_SAVE_F4				\
  CALLER_SAVE_D1				\
  CALLER_SAVE_D2				\
  CALLER_SAVE_L1

     /* Save Base last, since the others may
	be addressed relative to it */
#define CALLER_SAVE_SYSTEM			\
  CALLER_SAVE_Sp				\
  CALLER_SAVE_Su				\
  CALLER_SAVE_SpLim				\
  CALLER_SAVE_Hp				\
  CALLER_SAVE_HpLim				\
  CALLER_SAVE_CurrentTSO			\
  CALLER_SAVE_CurrentNursery			\
  CALLER_SAVE_SparkHd				\
  CALLER_SAVE_SparkTl				\
  CALLER_SAVE_SparkBase				\
  CALLER_SAVE_SparkLim                          \
  CALLER_SAVE_Base

#define CALLER_RESTORE_USER			\
  CALLER_RESTORE_R1				\
  CALLER_RESTORE_R2				\
  CALLER_RESTORE_R3				\
  CALLER_RESTORE_R4				\
  CALLER_RESTORE_R5				\
  CALLER_RESTORE_R6				\
  CALLER_RESTORE_R7				\
  CALLER_RESTORE_R8				\
  CALLER_RESTORE_F1				\
  CALLER_RESTORE_F2				\
  CALLER_RESTORE_F3				\
  CALLER_RESTORE_F4				\
  CALLER_RESTORE_D1				\
  CALLER_RESTORE_D2				\
  CALLER_RESTORE_L1

     /* Restore Base first, since the others may
	be addressed relative to it */
#define CALLER_RESTORE_SYSTEM			\
  CALLER_RESTORE_Base				\
  CALLER_RESTORE_Sp				\
  CALLER_RESTORE_Su				\
  CALLER_RESTORE_SpLim				\
  CALLER_RESTORE_Hp				\
  CALLER_RESTORE_HpLim				\
  CALLER_RESTORE_CurrentTSO			\
  CALLER_RESTORE_CurrentNursery			\
  CALLER_RESTORE_SparkHd			\
  CALLER_RESTORE_SparkTl			\
  CALLER_RESTORE_SparkBase			\
  CALLER_RESTORE_SparkLim

#else /* not IN_STG_CODE */

#define CALLER_SAVE_USER       /* nothing */
#define CALLER_SAVE_SYSTEM     /* nothing */
#define CALLER_RESTORE_USER    /* nothing */
#define CALLER_RESTORE_SYSTEM  /* nothing */

#endif /* IN_STG_CODE */

#define CALLER_SAVE_ALL				\
  CALLER_SAVE_SYSTEM				\
  CALLER_SAVE_USER

#define CALLER_RESTORE_ALL			\
  CALLER_RESTORE_SYSTEM				\
  CALLER_RESTORE_USER

#endif /* REGS_H */

