/* -----------------------------------------------------------------------------
 * $Id: StgCRun.c,v 1.18.2.1 2000/11/13 14:51:49 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * STG-to-C glue.
 *
 * To run an STG function from C land, call
 *
 *		rv = StgRun(f,BaseReg);
 *
 * where "f" is the STG function to call, and BaseReg is the address of the
 * RegTable for this run (we might have separate RegTables if we're running
 * multiple threads on an SMP machine).
 *
 * In the end, "f" must JMP to StgReturn (defined below),
 * passing the return-value "rv" in R1,
 * to return to the caller of StgRun returning "rv" in
 * the whatever way C returns a value.
 *
 * NOTE: StgRun/StgReturn do *NOT* load or store Hp or any
 * other registers (other than saving the C callee-saves 
 * registers).  Instead, the called function "f" must do that
 * in STG land.
 * 
 * GCC will have assumed that pushing/popping of C-stack frames is
 * going on when it generated its code, and used stack space
 * accordingly.  However, we actually {\em post-process away} all
 * such stack-framery (see \tr{ghc/driver/ghc-asm.lprl}). Things will
 * be OK however, if we initially make sure there are
 * @RESERVED_C_STACK_BYTES@ on the C-stack to begin with, for local
 * variables.  
 *
 * -------------------------------------------------------------------------- */

/* include Stg.h first because we want real machine regs in here: we
 * have to get the value of R1 back from Stg land to C land intact.
 */
#include "Stg.h"
#include "Rts.h"
#include "StgRun.h"

#ifdef DEBUG
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Printer.h"
#endif

#ifdef USE_MINIINTERPRETER

/* -----------------------------------------------------------------------------
   any architecture (using miniinterpreter)
   -------------------------------------------------------------------------- */
	
/* The static @jmp_environment@ variable allows @miniInterpret@ to
 * communicate with @StgReturn@.
 * 
 * Because @StgRun@ may be used recursively, we carefully
 * save and restore the whole of @jmp_environment@.
 */
#include <setjmp.h>
#include <string.h> /* for memcpy */

static jmp_buf jmp_environment;

#if 1

extern StgThreadReturnCode StgRun(StgFunPtr f, StgRegTable *basereg)
{
   while (f) {
      IF_DEBUG(evaluator,
	       fprintf(stderr,"Jumping to ");
	       printPtr((P_)f);
	       fprintf(stderr,"\n");
	      );
      f = (StgFunPtr) (f)();
   }
   return (StgThreadReturnCode)R1.i;
}

EXTFUN(StgReturn)
{
   return 0;
}

#else

#define CHECK_STACK   0
#define STACK_DETAILS 0

static int enters = 0;

static void scanStackSeg ( W_* ptr, int nwords )
{
   W_ w;
#if CHECK_STACK
   int nwords0 = nwords;
#if STACK_DETAILS
   while (nwords > 0) {
      w = *ptr;
      if (IS_ARG_TAG(w)) {
         fprintf ( stderr, "%d",w ); nwords--; ptr++;
	 while (w) { fprintf(stderr, "_"); w--; nwords--; ptr++; }
      }
      else {
         fprintf(stderr, "p"); 
         nwords--; ptr++;
      }
   }
   if (nwords < 0) fprintf(stderr, "erk: nwords < 0\n");
#endif
   checkStackChunk ( ptr, ptr-nwords0 );
#endif
}

extern StgFunPtr stg_enterStackTop;
extern StgThreadReturnCode StgRun(StgFunPtr f, StgRegTable *basereg)
{
    char* nm;
    while (1) {

#if CHECK_STACK
   {
   int i;
   StgTSO*  tso = basereg->rCurrentTSO;
   StgWord* sb  = tso->stack + tso->stack_size;
   StgWord* sp;
   StgWord* su;
   int ws;

   if (f == &stg_enterStackTop) {
      sp = tso->sp;
      su = tso->su;
   } else {
      sp  = basereg->rSp;
      su  = basereg->rSu;
   }

#if STACK_DETAILS
   fprintf(stderr, 
           "== SB = %p   SP = %p(%p)   SU = %p   SpLim = %p(%p)\n", 
           sb, sp, tso->sp,   su, basereg->rSpLim, tso->splim);
#endif

   if (su >= sb) goto postloop;
   if (!sp || !su) goto postloop;

   printStack ( sp, sb, su);

   while (1) {
      ws = su - sp;
      switch (get_itbl((StgClosure*)su)->type) {
         case STOP_FRAME: 
            scanStackSeg(sp,ws);
#if STACK_DETAILS
            fprintf(stderr, "S%d ",ws); 
            fprintf(stderr, "\n");
#endif
            goto postloop;
         case UPDATE_FRAME: 
            scanStackSeg(sp,ws);
#if STACK_DETAILS
            fprintf(stderr,"U%d ",ws); 
#endif
            sp = su + sizeofW(StgUpdateFrame);
            su = ((StgUpdateFrame*)su)->link;
            break;
         case SEQ_FRAME: 
            scanStackSeg(sp,ws);
#if STACK_DETAILS
            fprintf(stderr,"Q%d ",ws); 
#endif
            sp = su + sizeofW(StgSeqFrame);
            su = ((StgSeqFrame*)su)->link;
            break;
         case CATCH_FRAME: 
            scanStackSeg(sp,ws);
#if STACK_DETAILS
            fprintf(stderr,"C%d ",ws); 
#endif
            sp = su + sizeofW(StgCatchFrame);
            su = ((StgCatchFrame*)su)->link;
            break;
         default:
            fprintf(stderr, "?\nweird record on stack\n");
            assert(0);
            goto postloop;
      }
   }
   postloop:
   }
#endif
#if STACK_DETAILS
       fprintf(stderr,"\n");
#endif
#if 1
       fprintf(stderr,"-- enter %p ", f);
       nm = nameFromOPtr ( f );
          if (nm) fprintf(stderr, "%s", nm); else
          printPtr((P_)f);
       fprintf ( stderr, "\n");
#endif
#if STACK_DETAILS
       fprintf(stderr,"\n");
#endif
    zzz:
       if (enters % 1000 == 0) fprintf(stderr, "%d enters\n",enters);
       enters++;
       f = (StgFunPtr) (f)();
       if (!f) break;
    }
    fprintf (stderr, "miniInterpreter: bye!\n\n" );
    return (StgThreadReturnCode)R1.i;
}

EXTFUN(StgReturn)
{
   return 0;
}
#endif



#else /* !USE_MINIINTERPRETER */

#ifdef LEADING_UNDERSCORE
#define STG_RETURN "_StgReturn"
#else
#define STG_RETURN "StgReturn"
#endif

/* -----------------------------------------------------------------------------
   x86 architecture
   -------------------------------------------------------------------------- */
	
#ifdef i386_TARGET_ARCH

StgThreadReturnCode
StgRun(StgFunPtr f, StgRegTable *basereg) {

    StgChar space[ RESERVED_C_STACK_BYTES + 4*sizeof(void *) ];
    StgThreadReturnCode r;

    __asm__ volatile (
	/* 
	 * save callee-saves registers on behalf of the STG code.
	 */
	"movl %%esp, %%eax\n\t"
	"addl %4, %%eax\n\t"
        "movl %%ebx,0(%%eax)\n\t"
        "movl %%esi,4(%%eax)\n\t"
        "movl %%edi,8(%%eax)\n\t"
        "movl %%ebp,12(%%eax)\n\t"
	/*
	 * Set BaseReg
	 */
	"movl %3,%%ebx\n\t"
	/*
	 * grab the function argument from the stack, and jump to it.
	 */
        "movl %2,%%eax\n\t"
        "jmp *%%eax\n\t"

	".global " STG_RETURN "\n"
       	STG_RETURN ":\n\t"

	"movl %%esi, %%eax\n\t"   /* Return value in R1  */

	/*
	 * restore callee-saves registers.  (Don't stomp on %%eax!)
	 */
	"movl %%esp, %%edx\n\t"
	"addl %4, %%edx\n\t"
        "movl 0(%%edx),%%ebx\n\t"	/* restore the registers saved above */
        "movl 4(%%edx),%%esi\n\t"
        "movl 8(%%edx),%%edi\n\t"
        "movl 12(%%edx),%%ebp\n\t"

      : "=&a" (r), "=m" (space)
      : "m" (f), "m" (basereg), "i" (RESERVED_C_STACK_BYTES)
      : "edx" /* stomps on %edx */
    );

    return r;
}

#endif

/* -----------------------------------------------------------------------------
   Sparc architecture

   -- 
   OLD COMMENT from GHC-3.02:

   We want tailjumps to be calls, because `call xxx' is the only Sparc
   branch that allows an arbitrary label as a target.  (Gcc's ``goto
   *target'' construct ends up loading the label into a register and
   then jumping, at the cost of two extra instructions for the 32-bit
   load.)

   When entering the threaded world, we stash our return address in a
   known location so that \tr{%i7} is available as an extra
   callee-saves register.  Of course, we have to restore this when
   coming out of the threaded world.

   I hate this god-forsaken architecture.  Since the top of the
   reserved stack space is used for globals and the bottom is reserved
   for outgoing arguments, we have to stick our return address
   somewhere in the middle.  Currently, I'm allowing 100 extra
   outgoing arguments beyond the first 6.  --JSM

   Updated info (GHC 4.06): we don't appear to use %i7 any more, so
   I'm not sure whether we still need to save it.  Incedentally, what
   does the last paragraph above mean when it says "the top of the
   stack is used for globals"?  What globals?  --SDM

   Updated info (GHC 4.08.2): not saving %i7 any more (see below).
   -------------------------------------------------------------------------- */
	
#ifdef sparc_TARGET_ARCH

StgThreadReturnCode
StgRun(StgFunPtr f, StgRegTable *basereg) {

    StgChar space[RESERVED_C_STACK_BYTES];
    f();
    __asm__ volatile (
	    ".align 4\n"		
            ".global " STG_RETURN "\n"
       	    STG_RETURN ":" 
	    : : : "l0","l1","l2","l3","l4","l5","l6","l7");
    /* we tell the C compiler that l0-l7 are clobbered on return to
     * StgReturn, otherwise it tries to use these to save eg. the
     * address of space[100] across the call.  The correct thing
     * to do would be to save all the callee-saves regs, but we
     * can't be bothered to do that.
     *
     * The code that gcc generates for this little fragment is now
     * terrible.  We could do much better by coding it directly in
     * assembler.
     */
#if 0
    /* updated 4.08.2: we don't save %i7 in the middle of the reserved
     * space any more, since gcc tries to save its address across the
     * call to f(), this gets clobbered in STG land and we end up
     * dereferencing a bogus pointer in StgReturn.
     */
    __asm__ volatile ("ld %1,%0" 
		      : "=r" (i7) : "m" (((void **)(space))[100]));
#endif
    return (StgThreadReturnCode)R1.i;
}

#endif

/* -----------------------------------------------------------------------------
   alpha architecture
   -------------------------------------------------------------------------- */

#ifdef alpha_TARGET_ARCH

StgThreadReturnCode
StgRun(StgFunPtr f, StgRegTable *basereg) 
{
    StgThreadReturnCode ret;

    __asm__ volatile ("stq $9,-8($30)\n\t"
                      "stq $10,-16($30)\n\t"
                      "stq $11,-24($30)\n\t"
                      "stq $12,-32($30)\n\t"
                      "stq $13,-40($30)\n\t"
                      "stq $14,-48($30)\n\t"
                      "stq $15,-56($30)\n\t"
                      "stt $f2,-64($30)\n\t"
                      "stt $f3,-72($30)\n\t"
                      "stt $f4,-80($30)\n\t"
                      "stt $f5,-88($30)\n\t"
                      "stt $f6,-96($30)\n\t"
                      "stt $f7,-104($30)\n\t"
                      "stt $f8,-112($30)\n\t"
                      "stt $f9,-120($30)\n\t"
    	    	      "lda $30,-%0($30)" : :
                      "K" (RESERVED_C_STACK_BYTES+
			   8*sizeof(double)+8*sizeof(long)));

    f();

    __asm__ volatile (".align 3\n"
               	      ".globl " STG_RETURN "\n"
                      STG_RETURN ":\n\t"
		      "lda %0,($14)\n\t"  /* save R1 */
               	      "lda $30,%0($30)\n\t"
               	      "ldq $9,-8($30)\n\t"
               	      "ldq $10,-16($30)\n\t"
               	      "ldq $11,-24($30)\n\t"
               	      "ldq $12,-32($30)\n\t"
               	      "ldq $13,-40($30)\n\t"
               	      "ldq $14,-48($30)\n\t"
               	      "ldq $15,-56($30)\n\t"
               	      "ldt $f2,-64($30)\n\t"
               	      "ldt $f3,-72($30)\n\t"
               	      "ldt $f4,-80($30)\n\t"
               	      "ldt $f5,-88($30)\n\t"
               	      "ldt $f6,-96($30)\n\t"
    	    	      "ldt $f7,-104($30)\n\t"
    	    	      "ldt $f8,-112($30)\n\t" 
    	    	      "ldt $f9,-120($30)" 
		      : "=r" (ret)
		      : "K" (RESERVED_C_STACK_BYTES+
			   8*sizeof(double)+8*sizeof(long)));

    return ret;
}

#endif /* alpha_TARGET_ARCH */

/* -----------------------------------------------------------------------------
   HP-PA architecture
   -------------------------------------------------------------------------- */

#ifdef hppa1_1_TARGET_ARCH

StgThreadReturnCode
StgRun(StgFunPtr f, StgRegTable *basereg) 
{
    StgChar space[RESERVED_C_STACK_BYTES+16*sizeof(long)+10*sizeof(double)];
    StgThreadReturnCode ret;

    __asm__ volatile ("ldo %0(%%r30),%%r19\n"
		      "\tstw %%r3, 0(0,%%r19)\n"
                      "\tstw %%r4, 4(0,%%r19)\n"
                      "\tstw %%r5, 8(0,%%r19)\n"
                      "\tstw %%r6,12(0,%%r19)\n"
                      "\tstw %%r7,16(0,%%r19)\n"
                      "\tstw %%r8,20(0,%%r19)\n"
                      "\tstw %%r9,24(0,%%r19)\n"
		      "\tstw %%r10,28(0,%%r19)\n"
                      "\tstw %%r11,32(0,%%r19)\n"
                      "\tstw %%r12,36(0,%%r19)\n"
                      "\tstw %%r13,40(0,%%r19)\n"
                      "\tstw %%r14,44(0,%%r19)\n"
                      "\tstw %%r15,48(0,%%r19)\n"
                      "\tstw %%r16,52(0,%%r19)\n"
                      "\tstw %%r17,56(0,%%r19)\n"
                      "\tstw %%r18,60(0,%%r19)\n"
		      "\tldo 80(%%r19),%%r19\n"
		      "\tfstds %%fr12,-16(0,%%r19)\n"
		      "\tfstds %%fr13, -8(0,%%r19)\n"
		      "\tfstds %%fr14,  0(0,%%r19)\n"
		      "\tfstds %%fr15,  8(0,%%r19)\n"
		      "\tldo 32(%%r19),%%r19\n"
		      "\tfstds %%fr16,-16(0,%%r19)\n"
		      "\tfstds %%fr17, -8(0,%%r19)\n"
		      "\tfstds %%fr18,  0(0,%%r19)\n"
		      "\tfstds %%fr19,  8(0,%%r19)\n"
		      "\tldo 32(%%r19),%%r19\n"
		      "\tfstds %%fr20,-16(0,%%r19)\n"
		      "\tfstds %%fr21, -8(0,%%r19)\n" : :
                      "n" (-(116 * sizeof(long) + 10 * sizeof(double))) : "%r19"
		      );

    f();

    __asm__ volatile (".align 4\n"
               	      "\t.EXPORT " STG_RETURN ",CODE\n"
		      "\t.EXPORT " STG_RETURN ",ENTRY,PRIV_LEV=3\n"
                      STG_RETURN "\n"
                      /* "\tldo %0(%%r3),%%r19\n" */
                      "\tldo %1(%%r30),%%r19\n"
                      "\tcopy %%r11, %0\n"  /* save R1 */
		      "\tldw  0(0,%%r19),%%r3\n"
                      "\tldw  4(0,%%r19),%%r4\n"
                      "\tldw  8(0,%%r19),%%r5\n"
                      "\tldw 12(0,%%r19),%%r6\n"
                      "\tldw 16(0,%%r19),%%r7\n"
                      "\tldw 20(0,%%r19),%%r8\n"
                      "\tldw 24(0,%%r19),%%r9\n"
		      "\tldw 28(0,%%r19),%%r10\n"
                      "\tldw 32(0,%%r19),%%r11\n"
                      "\tldw 36(0,%%r19),%%r12\n"
                      "\tldw 40(0,%%r19),%%r13\n"
                      "\tldw 44(0,%%r19),%%r14\n"
                      "\tldw 48(0,%%r19),%%r15\n"
                      "\tldw 52(0,%%r19),%%r16\n"
                      "\tldw 56(0,%%r19),%%r17\n"
                      "\tldw 60(0,%%r19),%%r18\n"
		      "\tldo 80(%%r19),%%r19\n"
		      "\tfldds -16(0,%%r19),%%fr12\n"
		      "\tfldds  -8(0,%%r19),%%fr13\n"
		      "\tfldds   0(0,%%r19),%%fr14\n"
		      "\tfldds   8(0,%%r19),%%fr15\n"
		      "\tldo 32(%%r19),%%r19\n"
		      "\tfldds -16(0,%%r19),%%fr16\n"
		      "\tfldds  -8(0,%%r19),%%fr17\n"
		      "\tfldds   0(0,%%r19),%%fr18\n"
		      "\tfldds   8(0,%%r19),%%fr19\n"
		      "\tldo 32(%%r19),%%r19\n"
		      "\tfldds -16(0,%%r19),%%fr20\n"
		      "\tfldds  -8(0,%%r19),%%fr21\n" 
		         : "=r" (ret)
		         : "n" (-(116 * sizeof(long) + 10 * sizeof(double)))
		         : "%r19"
		      );

    return ret;
}

#endif /* hppa1_1_TARGET_ARCH */

#endif /* !USE_MINIINTERPRETER */
